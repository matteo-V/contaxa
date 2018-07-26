# from dana_manip_funs.R

devtools::load_all()

library(plyr)
library(tidyverse)
library(replyr) #for coalesce to fill in missing crossprods

#download all human tables
dat <- ed2_human()
event_dat <- ed2_events()


#test country codes
country_codes <- c("BD", "TH", "CN")
#test texa types
taxa_types <- c('rodents', "bats", 'nhp', 'swine', 'poultry')

#test contaxa function
BD_contaxa_dat <- reshape_contaxa_data(dat = BD_dat, taxa = taxa_types)

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
  expand_contaxa_crossprod_by_var(var_name = 'age_quint_range')


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

### From data_viz_funs.R

library(tidyverse) #interactive beauty
library(plotly) #interactive beauty
library(ggthemes) #pretty fivethirtyeight theme
library(viridis)
library(RColorBrewer)
library(colorRamps)

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
BD_contacts_dat <-
  BD_dat %>%
  get_clean_exposures(taxa_names = c('bats', 'nhp', 'swine', 'poultry', 'rodents'))

BD_covars <- BD_dat %>% get_clean_illness_covariates()

####################### example self-report dat ##########################################

#test function
BD_self_reports <- BD_dat %>% get_self_reports()

##########################################################################################
########################### example analysis data #########################################

#TODO: wrap the contaxa dataframe generation into a function
BD_analysis_dat <- join_all(list(BD_covars, BD_self_reports, BD_contacts_dat),
                            by = 'participant_id',
                            type='full')

#test function
BD_contaxa_crosstab <- run_contaxa_crosstab(analysis_dat = BD_analysis_dat,
                                            condition_var = 'gender')

ili_model_dat <-
  BD_dat %>%
  get_illness_analysis_dat(outcome_var = 'ili')

#test function
res <- ili_model_dat %>% remove_colinear_columns()

#output logical test that algo works
cat('Rank matches number of columns?',
    ncol(res$model_data_matrix)==rankMatrix(res$model_data_matrix))


#create matrix of
ili_x_matrix <- model.matrix(outcome ~ . + (.)^2, data = data.frame(res$model_data_matrix) )
############################# LASSO Regularized Logistic Regression ##########################

#illness_model_dat_split <- train_test_split(illness_model_dat)
#get optimal lambda
lambda.fit <- cv.glmnet(x = ili_x_matrix ,
                        y =  ili_model_dat[,1] ,
                        nfolds = , #5-fold CV
                        family = 'binomial',
                        #lambda = sapply(1:20, function(x) 2^(-x)),
                        alpha = 1) #LASSO regularize
plot(lambda.fit)
cat('Minimum value of lambda by 5-fold CV:', lambda.fit$lambda.min)
lam <- lambda.fit$lambda.min
n <- length(ili_model_dat[,1])


#create matrix of
ili_x_matrix <- model.matrix(outcome ~ . + (.)^2, data = data.frame(res$model_data_matrix) )
############################# LASSO Regularized Logistic Regression ##########################

#illness_model_dat_split <- train_test_split(illness_model_dat)
#get optimal lambda
lambda.fit <- cv.glmnet(x = ili_x_matrix ,
                        y =  ili_model_dat[,1] ,
                        nfolds = , #5-fold CV
                        family = 'binomial',
                        #lambda = sapply(1:20, function(x) 2^(-x)),
                        alpha = 1) #LASSO regularize
plot(lambda.fit)
cat('Minimum value of lambda by 5-fold CV:', lambda.fit$lambda.min)
lam <- lambda.fit$lambda.min
n <- length(ili_model_dat[,1])
