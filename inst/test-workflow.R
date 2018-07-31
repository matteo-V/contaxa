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
country_codes <- c("BD", "TH", "CN")
#test texa types
taxa_types <- c('rodents', "bats", 'nhp', 'swine', 'poultry')

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

BD_age_annotated %>%  plot_contaxa_heatmap(strat = 'age_quint_range',
                                           human_dat = dat %>%
                                             select_country_dat(country_codes = 'BD'))
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


#create matrix of
ili_x_matrix <- model.matrix(outcome ~ (.)^2 + ., data = data.frame(res$model_data_matrix) )
############################# LASSO Regularized Logistic Regression ##########################

#illness_model_dat_split <- train_test_split(illness_model_dat)
#get optimal lambda
lambda.fit <- cv.glmnet(x = res$model_data_matrix[,-1] ,
                        y = res$model_data_matrix[,1] ,
                        nfolds = , #5-fold CV
                        family = 'binomial',
                        #lambda = sapply(1:20, function(x) 2^(-x)),
                        alpha = 1) #LASSO regularize
plot(lambda.fit)
cat('Minimum value of lambda by 5-fold CV:', lambda.fit$lambda.min)

lam <- lambda.fit$lambda.min
n <- length(ili_model_dat[,1])

#create model matrix
ili_x_matrix <- model.matrix(outcome ~ . + (.)^2, data = data.frame(res$model_data_matrix) )

#fit glmnet with optimal lambda on training data
lasso.fit <- glmnet(x = res$model_data_matrix[,-1],
                    y = as.logical(ili_model_dat[,1]), #fit using logical labels
                    family = 'binomial',
                    lambda = lam , #use CV'd lambda.min
                    alpha = 1,  #LASSO
                    standardize = T,
                    thresh = 1e-25)
#summarize model
# lasso.fit
#get non-zero coefficients
exp( coef(lasso.fit)[which( coef(lasso.fit) != 0),])

#get class predictions
lasso.pred <- predict(lasso.fit,
                      newx = res$model_data_matrix[,-1],
                      type = 'class')


#get misclassification rate
lasso.conf.matrix <- table(ili_model_dat[,1], as.factor(lasso.pred))
lasso.conf.matrix
1 - ( sum(diag(lasso.conf.matrix)) / sum(lasso.conf.matrix))


#glmnet multiplies the first term by a factor of 1/n
betas = coef(lasso.fit,
             s = lam/n,
             exact = T,
             x = res$model_data_matrix[,-1] ,
             y = as.factor( ili_model_dat[,1] ))[-1] #[-1] removes the intercept

#perform inference for glmnet (LASSO) model
lasso.inference <-
  fixedLassoInf(x = res$model_data_matrix[,-1],
                y = as.logical( ili_model_dat[,1] )*1 ,
                beta = betas,
                lambda = lam
  )

#variables names for output
var_names <- names(lasso.inference$vars)

#mean effect sizes
effects <- lasso.inference$coef0[,1]

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

#subset significant variables
sig_lasso_inference <- subset(lasso_inference_table, significant == T)
sig_lasso_inference <- sig_lasso_inference[order(sig_lasso_inference$effect_size, decreasing = F),]
var_order <- sig_lasso_inference$var_names

#plot things
ggplot(data = lasso_inference_table, aes(y = var_names, x = effect_size)) +
  geom_point(aes(color = var_names), cex = 4) +
  geom_errorbarh(aes(xmin = lower, xmax=upper, color = var_names), lwd=0.8) +
  geom_vline(aes(xintercept = 1), lty=2, color = 'black', alpha = 0.8) +
  ggtitle('ILI in BD: Variable Odds Ratios') +
  scale_color_discrete(guide=F) +
  theme_bw() +
  xlab('Odds Ratio') +
  theme(plot.title = element_text(size=16, hjust=0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14))

