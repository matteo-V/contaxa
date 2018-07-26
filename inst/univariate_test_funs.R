
BD_univariate_dat <- readRDS(file='data/BD_univariate_dat.rds')

#calculate frequencies
BD_gender_freqs <- BD_univariate_dat %>%
  group_by(gender, contx_type, taxa_type) %>%
  summarise(num_respondents = n())

#Define grid of all possible cross prods
support <- expand.grid(gender = BD_gender_freqs %>% get_var_vals(var_name = 'gender'),
                       contx_type = BD_gender_freqs %>% get_var_vals(var_name = 'contx_type'), #all contx_type
                       taxa_type = BD_gender_freqs %>% get_var_vals(var_name = 'taxa_type'),
                       stringsAsFactors = FALSE)
#Fill in missing cross prods
BD_gender_freqs <-
  BD_gender_freqs %>%
  replyr_coalesce(support = support, fills= list(num_respondents = 0)) %>%
  arrange(gender, contx_type, taxa_type)


################# prototype statistical tests ###############################
#cross tabs for statistical tests
attach(BD_univariate_dat)
BD_crosstab_gender <- table(contx_type, taxa_type, gender)
BD_gender_table <- ftable(BD_crosstab)
##############################################################################
##############################################################################
#cochran-mantel_haenszel test
#H0: contx_type and taxa_type are conditionally independent in each stratum
mantel_test <- mantelhaen.test(BD_crosstab)
mantel_test
###


