library(tidyverse)

#depends on get_illness_analysis_frame
illness_model_dat <-
  BD_dat %>%
  get_illness_analysis_frame(outcome_var = 'ili')
############################# Exploratory RF modeling approach ################################
library(randomForest)

illness_model_dat_split <- train_test_split(illness_model_dat)

ili.rf.fit <- randomForest(y = illness_model_dat_split[['train']][,1] ,
                           x = illness_model_dat_split[['train']][,-1],
                           mtry = 26,
                           ntree = 200,
                           importance = T)
ili.rf.preds <- predict(ili.rf.fit,
                        newdata = illness_model_dat_split[['test']][,-1], type='class')
rf.conf.matrix <- table(illness_model_dat_split[['test']][,1], ili.rf.preds)
rf.conf.matrix
cat('Proportion of respondents with illness:', mean( as.logical(illness_model_dat_split[['test']][,1]) ) )
cat('Random Forest OOS misclassification rate:', 1 - ( sum(diag(rf.conf.matrix)) / sum(rf.conf.matrix)))
importance(ili.rf.fit)
varImpPlot(ili.rf.fit)
###############################################################################################
################################ GBM modeling approach #########################################
library(gbm)

#convert logical to factors
illness_model_dat_gbm <- illness_model_dat %>% mutate_if(is.logical, as.factor)
#split data into 3 sets liek normal
illness_model_dat_gbm <- train_test_val_split(illness_model_dat_gbm)

ili.gbm.fit <- gbm.fit(y = as.logical(illness_model_dat_gbm[['train']][,1]),
                       x = illness_model_dat_gbm[['train']][,-1],
                       verbose = F,
                       interaction.depth = 5,
                       shrinkage = 0.0002,
                       n.trees = 10000)
#predict probs
gbm.probs <- predict(ili.gbm.fit, type = 'response', newdata = undr_matrix, n.trees = 10000 )
#and convert to class labels
gbm.class <- vector(mode = 'logical', length = length(gbm.probs))
for(i in 1:length(gbm.probs)){
  if(gbm.probs[i] > 0.5){
    gbm.class[i] <- T
  }
  else{
    gbm.class[i] <- F
  }
}
#tabulate confusion matrix
gbm.conf.matrix <- table(as.logical(undr_dat[,1]), gbm.class)
gbm.conf.matrix
cat('GBM OOS misclassification rate:', 1 - (sum(diag(gbm.conf.matrix)) / sum(gbm.conf.matrix)))
cat('GBM AUROC:', gbm.roc.area(obs = as.logical(undr_dat[,1]), pred = gbm.probs))
##########################################################################################
########################### calculate crossEntropy function ###############################
#TODO: refactor this to take data sets instead of vectors

#'Calculate cross-entropy for random forest fit
#' @param y class-labels (factor with levels "TRUE" "FALSE") from test set
#' @param phat vector of probabilities for "TRUE" response
#' @return Binomial Cross-entropy
crossEntropy <- function(y, phat){
  y <- as.logical( unlist(y) ) #unlist to vector and coerce to logical
  res <- base::ifelse(y, log(phat), log(1-phat))
  return( -sum(res) ) #return sum for total cross entropy
}

# test loss function
# crossEntropy( y = split_dat[['val']]['poultry_contact_handled'], #class labels
#               phat = rf.val.prediction[,'TRUE']) #probability of "true" class


####################################################################################
############################## compute misclass rate function ######################
#' Compute missclassification rate
#' @param test_dat test data frame
#' @param outcome_var name of outcome variable as a character
#' @param rf_fit fitted RF model (fitted on training data)
#' @return misclassification rate
#' @export
misclassRF <- function(test_dat, outcome_var, rf_fit){
  #unlist from frame
  y <- unlist( test_dat[outcome_var] )
  yhat <- predict(rf_fit, test_dat, type='response')
  conf.matrix <- table(y, yhat)
  misclass <- 1 - ( sum( diag(conf.matrix) ) / sum(conf.matrix) )
  return(misclass)
}

####################################################################################################
############################ RF hyperparameter optimization ########################################
#TODO: wrap this into a function
#expand grid for hyperparameter search
tree.grid <- expand.grid(mtry = c(4,5,6,7,8),
                         ntree = c(50,100,250,1000),
                         maxnodes = c(3,5,7,9,11,13,15),
                         insamp_misclass = 0,
                         insamp_crossentropy = 0,
                         outsamp_misclass = 0,
                         outsamp_crossentropy = 0)
# Hyperparameter optimization for RF
for(i in 1:nrow(tree.grid)){
  #perform Rf fit
  rf.fit <- randomForest(formula = poultry_contact_handled ~ .,
                         data = split_dat[['train']],
                         mtry = tree.grid[i,'mtry'],
                         ntree = tree.grid[i, 'ntree'],
                         maxnodes = tree.grid[i, 'maxnodes'])

  #in-sample probabilities
  rf.train.prob <- predict(rf.fit, split_dat[['train']], type='prob')

  #out of sample (validation) probabilities
  rf.val.prob <- predict(rf.fit, split_dat[['val']], type = 'prob')

  #calculate in sample cross ent
  tree.grid[i, 'insamp_crossentropy'] <- crossEntropy(y = split_dat[['train']]['poultry_contact_handled'],
                                                      phat = rf.train.prob[,'TRUE'])
  #calculate in sample misclass rate
  tree.grid[i, 'insamp_misclass'] <- misclassRF(split_dat[['train']], 'poultry_contact_handled', rf.fit)

  #calculate out of sample cross ent
  tree.grid[i, 'outsamp_crossentropy'] <- crossEntropy(y = split_dat[['val']]['poultry_contact_handled'],
                                                       phat = rf.val.prob[,'TRUE'])
  #calculate out of sample misclass rate
  tree.grid[i, 'outsamp_misclass' ] <- misclassRF(split_dat[['val']], 'poultry_contact_handled', rf.fit)

  #print status of search to screen
  if( (i%%20 == 0) ){
    cat('\n', i, 'searches completed', nrow(tree.grid) - i, 'iterations remaining')
  }
  else if(i ==0){
    cat('\nsearches completed')
  }
}

#fit random forest with best params
rf.fit <- randomForest(formula = poultry_contact_handled ~ .,
                       data = split_dat[['train']],
                       mtry = 8,
                       ntree = 100,
                       maxnodes = 9,
                       importance = T)

#return labels for test data
rf.val.prediction <- predict(rf.fit, split_dat[['test']], type='response')
#calculate missclassifiation rate for test data
misclassRF(test_dat = split_dat[['test']], outcome_var = 'poultry_contact_handled', rf_fit = rf.fit)
#plot variable importance
varImpPlot(rf.fit)

#########################################################################################################
####################################### exploratory h2o modeling approach ###############################

#start h2o session
h <- h2o.init()

#h2o dataframe
train_dat <- as.h2o( split_dat[['train']] )
test_dat <- as.h2o( split_dat[['test']] )
val_dat <- as.h2o( split_dat[['val']] )

#exploratory fit
rf_fit <- h2o.randomForest( y = 'poultry_contact_handled',
                            training_frame = train_dat)
h2o.gainsLift(rf_fit, val_dat )
############################################################################################
#association rule mining
library(arules)
library(arulesCBA)


#oversample negative to correct class imbalance
ovr_neg <- ili_model_dat %>% filter(!as.logical(ili)) %>% sample_n(size = 538-194, replace = T)
ovr_pos <- ili_model_dat %>% filter(as.logical(ili))
ovr_dat <- rbind(ovr_pos, ovr_neg)
ovr_dat_split <- ovr_dat %>% train_test_split()

#sample size calc
c <-  0.1 #90% confidence
supp <- 0.05
epsilon <- 0.1
n <- -2 * log(c)/ (supp * epsilon^2)
boot <- sample_n(ovr_dat, replace = T, size = n)

#induct rules using oversampled position data frame
ili.rules <- apriori(data=ovr_dat_split$train %>%
                       select(-matches('in_dwelling')) %>%
                       select(-matches('prov')) %>%
                       select(-matches('age')),
                     parameter=list (supp=0.05,conf = 0.5, maxlen=7),
                     appearance = list( rhs='ili=TRUE'))

inspect( head(ili.rules, by='confidence', n=10))
#build rule based classifier via apriori algorithm
ili.classifier <- CBA(formula = ili ~ . ,
                      data = data.matrix( ovr_dat_split$train %>%
                                            select(-matches('in_dwelling')) %>%
                                            select(-matches('prov')) %>%
                                            select(-matches('age')) ))
