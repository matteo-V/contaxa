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
  select_country_dat(country_codes = c('BD')) %>%
  get_illness_analysis_dat(outcome_var = 'ili')

#test function
res <- ili_model_dat %>% remove_colinear_columns()

#output logical test that algo works
cat('Rank matches number of columns?',
    ncol(res$model_data_matrix)==rankMatrix(res$model_data_matrix))


############################# LASSO Regularized Logistic Regression ##########################


#create two way interaction model matrix from reduced one way matrix

ili_interaction_mat <- model.matrix(ili ~ . + (.)^2,
                                    data = ili_model_dat)
#removes linear dependencies
#ili_interaction_mat <- remove_colinear_columns(ili_interaction_mat)
#removes duplicates necessary for selectiveInference
#ili_interaction_mat <- remove_duplicate_columns(ili_interaction_mat)
#import reduced two way matrix
#red_mat <- readRDS('./data/reduced_interaction_matrix.rds')
BD_pca <- dudi.pca(ili_interaction_mat, scannf = F, nf=5)
pcr_dat <- as.matrix(BD_pca$li)
fit <-  glm.fit(y = ili_model_dat[,1], x=pcr_dat, family = 'binomial')
summary(fit)

set.seed(99) #wayne gretsky
#test cv function
lam <- cv_shrinkage_param(red_mat, response = ili_model_dat[,1])

lasso.fit <- fit_lasso_model(red_mat, response = ili_model_dat[,1])

#summarize model
#get names (support) of model
supp <- names( coef(lasso.fit)[which( coef(lasso.fit) != 0),] )
#get non-zero coefficients odds ratios
exp( coef(lasso.fit)[which( coef(lasso.fit) != 0),] )

lasso.fit %>% compute_lasso_performance(model_matrix = red_mat, ili_model_dat[,1])

lasso_inf_table <- lasso.fit %>% perform_lasso_inference(model_matrix = red_mat, response = ili_model_dat[,1])

lasso_inf_table %>%
  plot_lasso_inference() +
  ggtitle('Bangladesh: ILI Risk Factors') +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(hjust=0.5))


