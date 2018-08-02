############################### Visualization Pipeline Test ############################
# from dana_manip_funs.R
library(eidith)
options(ed_sql_path = "data/PREDICT2data.sqlite")
devtools::load_all()
library(plyr)
library(tidyverse)
library(replyr) #for coalesce to fill in missing crossprods

#download all human tables
dat <- ed2_human()

#test country codes
country_codes <- c("BD", "TH")
#test texa types
taxa_types <- c('rodents', "bats", 'nhp', 'swine', 'poultry')

all_contaxa_dat <- dat %>%
  reshape_contaxa_data(taxa = taxa_types) %>%
  group_by(contx_type, taxa_type) %>%
  count() %>%
  ungroup()


BDTH_demo_dat <-
  dat %>%
  select_country_dat(country_codes = country_codes) %>%
  select_demo_dat()

BDTH_country <-
  dat %>%
  select_country_dat(country_code = country_codes) %>%
  get_country_codes() %>%
  select(participant_id, country)

BDTH_demo_dat <- join(BDTH_demo_dat, BDTH_country, by='participant_id')

BDTH_country_grouped <-
  BDTH_join %>%
  calc_percent_repondents_by_var(demo_dat = BDTH_demo_dat,
                                 var='country')

BDTH_country_full <-
  BDTH_country_grouped %>%
  expand_contaxa_by_var(var_name = 'country')

BDTH_country_annotated <-
  BDTH_country_full %>%
  annotate_factor_with_n(var_name = 'country',
                         demo_dat = BDTH_demo_dat)

#BD contaxa data
BD_contaxa_dat <-
  dat %>%
  select_country_dat(country_codes = 'BD') %>%
  reshape_contaxa_data(taxa = taxa_types)

#BD demographic data
BD_demo <- dat %>%
  select_country_dat(country_codes = 'BD') %>%
  select_demo_dat()

# test function
BD_join_dat <- join_contx_and_demo(BD_contaxa_dat,
                                     BD_demo)

BD_concurrent_grouped <-
  BD_join_dat %>%
  calc_percent_repondents_by_var(demo_dat = BD_demo,
                                 var_name = 'concurrent_sampling_site')



BD_occupation_grouped <-
  BD_join_dat %>%
  calc_percent_repondents_by_var(demo_dat = BD_demo,
                                 var_name = 'occupation')
BD_age_grouped <-
  BD_join_dat %>%
  calc_percent_repondents_by_var(demo_dat = BD_demo,
                                 var_name = 'age_quint_range')


BD_occupation_group_full <-
  BD_occupation_grouped %>%
  expand_contaxa_by_var(var_name = 'occupation')

BD_concurrent_group_full <-
  BD_concurrent_grouped %>%
  expand_contaxa_by_var(var_name ='concurrent_sampling_site')

BD_age_group_full <-
  BD_age_grouped %>%
  expand_contaxa_by_var(var_name = 'age_quint_range')


BD_occupation_annotated <-
  BD_occupation_group_full %>%
  annotate_factor_with_n(var_name = 'occupation',
                         demo_dat = BD_demo)

BD_concurrent_annotated <-
  BD_concurrent_group_full %>%
  annotate_factor_with_n(var_name = 'concurrent_sampling_site',
                         demo_dat = BD_demo)

BD_age_annotated <-
  BD_age_group_full %>%
  annotate_factor_with_n(var_name = 'age_quint_range',
                         demo_dat = BD_demo)


################################ Vizualization Pipeleine ################################
### From data_viz_funs.R

library(tidyverse) #interactive beauty
library(plotly) #interactive beauty
library(ggthemes) #pretty fivethirtyeight theme
library(viridis)
library(RColorBrewer)
library(colorRamps)

BD_occupation_annotated %>%
  plot_contaxa_heatmap(strat = 'occupation',
                                           human_dat = dat %>%
                                             select_country_dat(country_codes = 'BD'), wrap_contact = T)
#########################################################################################
##################################LASSO Pipeline#########################################
### from model_fit_funs.R
library(glmnet)
library(plyr) #for join_all
library(tidyverse)
library(foreach)
library(Matrix)
library(selectiveInference)

#download human questionnaire data for all countries
dat <- ed2_human()

#subset BD dat for analysis
BD_dat <-
  dat %>%
  select_country_dat(country_codes = 'BD') #from data_manip_funs.R

#get taxa contacts widened for analysis
BD_contacts_dat <-
  BD_dat %>%
  get_clean_exposures(taxa_names = c('bats', 'nhp', 'swine', 'poultry', 'rodents'))

#get clean covariates widened for analysis
BD_covars <- BD_dat %>% get_clean_illness_covariates()

#get self reports widened for univariate stats
BD_self_reports <- BD_dat %>% get_self_reports()


#TODO: wrap the contaxa dataframe generation into a function
BD_univariate_analysis_dat <- join_all(list(BD_covars, BD_self_reports, BD_contacts_dat),
                            by = 'participant_id',
                            type='full')

#run univariate statistics for gender on all contact types
BD_contaxa_crosstab <- run_contaxa_crosstab(analysis_dat = BD_univariate_analysis_dat,
                                            condition_var = 'gender_male')
#get ili analysis frame
ili_model_dat <-
  dat %>%
  select_country_dat(country_codes = 'BD') %>%
  get_illness_analysis_dat(outcome_var = 'ili')

#test function
res <- ili_model_dat %>% remove_colinear_columns()

#output logical test that algo works
cat('Rank matches number of columns?',
    ncol(res$model_data_matrix)==rankMatrix(res$model_data_matrix))


############################# LASSO Regularized Logistic Regression ##########################
#import reduced two way matrix
res2 <- readRDS('./data/ili_reduced_matrix.rds')
res2_reduced <- remove_duplicate_columns(res2$model_data_matrix[,-1])



#create two way interaction model matrix from reduced one way matrix
ili_x_matrix <- model.matrix(outcome ~ ., data = data.frame(data.matrix(ili_model_dat) ))
ili_x_reduced <- remove_duplicate_columns(ili_x_matrix) #remove duplicates


set.seed(99) #wayne gretsky
#' perform cross-validation to get optimal lambda (shrinkage) parameter
#'
#' @param model_matrix model matrix (features, no response) to be used for logistic regression
#' @param reponse vector of response values (binary) for logisitic regression
#' @param cv_folds number of folds for cross_validation
#' @param verbose logical indicating wheter to print lambda to std output
#' @return lambda which minimizes the binomial deviance
#' @author matteo-V
#' @importFrom glmnet cv.glmnet plot.cv.glmnet
#' @noRd
cv_shrinkage_param <- function(model_matrix, response, cv_folds=5, verbose = F){
  lambda.fit <- cv.glmnet(x = model_matrix,
                          y = response ,
                          nfolds = cv_folds, #5-fold CV
                          family = 'binomial',
                          alpha = 1) #LASSO regularize
  #plot bias variance tradeoff curve
  plot(lambda.fit)
  #print value of minimum lambda
  if(verbose){
    cat('Minimum value of lambda by 5-fold CV:', lambda.fit$lambda.min)
  }
  #select minimum lam
  lam <- lambda.fit$lambda.min
  return(lam)
}

lam <- cv_shrinkage_param(ili_x_reduced, response = ili_model_dat[,1])
n <- length(ili_model_dat[,1])

#create model matrix
#ili_x_matrix <- model.matrix(outcome ~ . + (.)^2, data = data.frame(res$model_data_matrix) )

#' fit glmnet with optimal lambda on training data
#'
#' @param model_matrix model matrix of features (no response column) for logisitc regression
#' @param response response vector (binary)
#' @return fitted glmnet object
#' @author matteo-V
#' @importFrom glmnet glmnet
#' @export
fit_lasso_model <- function(model_matrix, response){
  #get optimal lambda
  lam <- cv_shrinkage_param(model_matrix, response)
  #fir lasso model
  lasso.fit <- glmnet(x = model_matrix,
                      y = response, #fit using logical labels
                      family = 'binomial',
                      lambda = lam, #use CV'd lambda.min
                      alpha = 1,  #LASSO
                      standardize = F,
                      thresh = 1e-30)
  return(lasso.fit)
}

lasso.fit <- fit_lasso_model(ili_x_reduced, response = ili_model_dat[,1])
#summarize model
#get non-zero coefficients odds ratios
#exp( coef(lasso.fit)[which( coef(lasso.fit) != 0),] )

#' Computes misclassification rate of a fitted lasso model
#' @param lasso_fit from fit_lasso_model function
#' @param model_matrix heldout model matrix of features
#' @param response response (y) vector
#' @author matteo-V
#' @importFrom glmnet predict.glmnet
#' @export
compute_lasso_performance <- function(lasso_fit, model_matrix, response){
  #get class predictions
  pred <- predict(lasso_fit,
                        newx = model_matrix,
                        type = 'class')

  pred <- as.logical(pred)
  #get misclassification rate
  lasso.conf.matrix <- table(pred, response)
  cat('Confusion Matrix:\n')
  print(lasso.conf.matrix)
  #get misclassification rate
  misclass_rate <- 1 - ( sum(diag(lasso.conf.matrix)) / sum(lasso.conf.matrix))
  cat("\nMisclassification rate:", misclass_rate)
  #compute sensitivity
  sens <- lasso.conf.matrix[2,2] / (lasso.conf.matrix[2,2] + lasso.conf.matrix[1,2])
  cat("\nFalse negative rate:", 1 - sens)
  #compute specificity
  specif <- lasso.conf.matrix[1,1] / (lasso.conf.matrix[1,1] + lasso.conf.matrix[2,1])
  cat("\nFalse positive rate:", 1 - specif)
}

#' Perform Inference on fitted lasso model
#'
#' @param lasso_fit from fit_lasso_model
#' @param model_matrix model matrix of features
#' @param response vector of response values
#' @return formatted LASSO inference table (variable, mean odds ration, 95% ci, p-values)
#' @author matteo-V
#' @importFrom stats coef
#' @importFrom selectiveInference fixedLassoInf
#' @export
perform_lasso_inference <- function(lasso_fit, model_matrix, response){
  cat("WARNING: This function may take a while to execute fully")
  #get lambda and N
  lam <- lasso_fit$lambda
  n <- length(response)
  #get betas
  betas <- coef(lasso_fit,
                s = lam/n, #glmnet multiplies the first term by a factor of 1/n
                exact = T,
                x = model_matrix,
                y = as.factor( response )) #[-1] removes the intercept

  #perform inference for glmnet (LASSO) model
  lasso.inference <-
    fixedLassoInf(x = model_matrix,
                  y = as.logical( response )*1 ,
                  beta = betas,
                  lambda = lam,
                  family = 'binomial',
                  gridrange = c(-1e6, 1e6),
                  tol.kkt = 1)

  #variables names for output
  var_names <- names(lasso.inference$vars)

  #mean effect sizes
  effects <- lasso.inference$coef0

  #lower CI bound
  lower_ci <- lasso.inference$ci[,1]
  upper_ci <- lasso.inference$ci[,2]

  #pvalues
  pvalues <- lasso.inference$pv

  #create table
  lasso_inference_table <- data_frame(var_names = var_names,
                                      effect_size=effects,
                                      lower=lower_ci,
                                      upper=upper_ci,
                                      pvalues=pvalues)

  #annotate significant values
  lasso_inference_table$significant <- (lasso_inference_table$pvalues < 0.05)

  #convert to odds ratios
  lasso_inference_table <-
    lasso_inference_table %>%
    mutate_at(.vars = vars(effect_size, lower, upper), .funs = funs(exp)) %>%
    mutate(var_names = str_replace_all(var_names, '_', ' '))

  return(lasso_inference_table)

}


#subset significant variables
# sig_lasso_inference <- subset(lasso_inference_table, significant == T)
# sig_lasso_inference <- sig_lasso_inference[order(sig_lasso_inference$effect_size, decreasing = F),]
# var_order <- sig_lasso_inference$var_names

#' plot output from perform_lasso_inference
#'
#' @param lasso_inference_table from perform_lasso_inference function
#' @param title desired plot title
#' @return ggplot2 object
#' @import ggplot2
#' @export
plot_lasso_inference <- function(lasso_inference_table, title=NULL){
  #plot pretty things
  out.plot <-
    ggplot(data = lasso_inference_table, aes(y = var_names, x = effect_size)) +
    geom_point(aes(color = var_names), cex = 4) +
    geom_errorbarh(aes(xmin = lower, xmax=upper, color = var_names), lwd=0.8) +
    geom_vline(aes(xintercept = 1), lty=2, color = 'black', alpha = 0.8) +
    ggtitle(title) +
    scale_color_discrete(guide=F) +
    scale_x_continuous(limits = c(0, 100)) +
    theme_bw() +
    xlab('Odds Ratio') +
    theme(plot.title = element_text(size=16, hjust=0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=14))
  return(out.plot)
  }



