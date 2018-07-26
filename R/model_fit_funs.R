########################################################################################
# library(glmnet)
# library(plyr) #for join_all
# library(tidyverse)
# library(foreach)
# library(Matrix)
# library(selectiveInference)
######################### import example data ##########################################
# #download human questionnaire data for all countries
# dat <- ed2_human()
#
# #subset BD dat for analysis
# BD_dat <-
#   dat %>%
#   select_country_dat(country_codes = 'BD') #from data_manip_funs.R


#######################################################################################
####################### get exposure response data function ###########################
#'@function create cleaned, binary response frame for contx_type
#'@param dat raw data frame, prefereably passed through select_country_dat
#'@param taxa_names character vector of taxa names
#'@return wide data frame with binary exposures of interest
#'@author matteo-V
get_clean_exposures <- function(dat, taxa_names){

  #create final list for plyr join_all
  res <- list()
  #iterate through all taxa_names
  for(taxa in taxa_names){

    #create name to use in data pipeline
    contx_type <- paste0(taxa, '_contact')
    no_contx_type <- paste0('no_', contx_type)
    #create single taxa exposure frame
    taxa_dat <-
      dat %>%
      select(participant_id, !!sym(contx_type)) %>%  #Non-standard eval of name
      ed2_expand_wide(!!sym(contx_type)) %>% #expand into wide frame of binary vars
      #rename n_a in ed2_expand_wide for interpretability
      rename( !!sym(no_contx_type) := n_a) %>% #use special assign := to work with !!sym(var)
      select(-!!sym(contx_type)) #remove original multiresponse from final frame

    #add taxa_exposure frame to list
    res[[taxa]] <- taxa_dat
  }
  #create final joined data
  out <- join_all(dfs = res, #join list of data frames
                        by='participant_id') #use primary key
  return(out)
}

###############################################################################
################################# test clean exposures ########################


# BD_contacts_dat <-
#   BD_dat %>%
#   get_clean_exposures(taxa_names = c('bats', 'nhp', 'swine', 'poultry', 'rodents'))


###############################################################################
#################### get covariate data function ##############################
#'@name get_clean_illness_covars
#'@function: create and widen covariate data for self reported illness modeling
#'@param dat a raw human table from EIDITH which to clean
#'@return wide data set with data types cleaned for regression analysis
#'@author matteo-V
get_clean_illness_covariates <- function(dat){
  #select and widen covariate data
  dat %>%
  select(-(matches("_contact"))) %>% #remove multiresponse contact cols
  select(participant_id, #select all covar data
         age,
         gender,
         length_lived,
         matches("dwelling"),
         matches("water"),
         matches("education"),
         primary_livelihood, #these are to build occupations variable.
         livelihood_groups_other, #use this to disambiguate the other categories
         matches("animals"),
         live_state_prov,
         dedicated_location_for_waste,
         work_location_prov,
         dedicated_location_for_waste,
         #had_symptoms_in_last_year_other_people,
         -matches("_notes"), #remove notes cols,
         matches("meat"),
         matches("scratched_bitten"),
         matches("hunted")) %>%  #this may be excluded?
  ed2_expand_wide(col = drinking_water_source) %>%
  ed2_expand_wide(col = scratched_bitten_action) %>%
  ed2_expand_wide(col = water_treatment) %>%
  select(-drinking_water_source, -scratched_bitten_action, -water_treatment) %>%
  #TODO: convert to proper data types
  #TODO: mutate primary livelihood for others to other and other-housewife to housewife
  #TODO: maybe mutate businees owners for the other category?
  #mutate character vectors to factors where appropriate
  mutate_at(.vars = vars(gender,
                      length_lived,
                      live_state_prov,
                      work_location_prov,
                      highest_education,
                      highest_education_mother),
            .funs = funs(factor)
            ) %>%
  #TODO: mutate and create a new occupation column using primary livelihood and livelihood groups other
  mutate(occupation = #create new occupation variable
           if_else( #condition
             condition = str_detect(primary_livelihood, "[Oo]ther"), #if other
             true = livelihood_groups_other, #use other group to map
             false = primary_livelihood #else use primary livelihood
             )
         ) %>%
  #TODO: now clean up missing values in occupation with primary livelihood
  mutate(occupation =
           if_else(
             condition = occupation == '', #if occupation is empty
             true = primary_livelihood,
             false = occupation
           )
  ) %>%
  #map other column back to "Other" because the cleaning never ends
  mutate(occupation =
            if_else(condition = str_detect(occupation, '[Oo]ther'),
                    true = 'Other',
                    false = occupation
                    )
          ) %>%
  #make occupation a factor
  mutate(occupation = as.factor(occupation)) %>%
  #TODO: Drop the livelihood columns and now use occupation for modeling
  select(-matches('livelihood')) %>%
  #TODO: coerce numeric covariates to proper types
  mutate_at(
    .vars = vars(age,
      people_in_dwelling,
      children_in_dwelling,
      males_in_dwelling,
      rooms_in_dwelling),
    .funs = funs(as.numeric)
  ) %>%
  #map yes/no questions such that: yes == TRUE, no/NA == FALSE
  mutate_at(.vars = vars(pet_in_dwelling_last_year,
                         animals_in_dwelling_last_year,
                         shared_water_last_year,
                         handle_animals_last_year,
                         raised_animals_last_year,
                         animals_in_food_last_year,
                         cooked_meat_last_year,
                         eaten_raw_meat_last_year,
                         scratched_bitten_last_year,
                         hunted_animal_last_year,
                         #had_symptoms_in_last_year_other_people,
                         dwelling_permanent_structure,
                         dedicated_location_for_waste,
                         pet_in_dwelling_life,
                         animals_in_dwelling_life,
                         water_treated,
                         water_used_by_animals,
                         shared_water_life,
                         handle_animals_life,
                         raised_animals_life,
                         animals_in_food_life,
                         cooked_meat_life,
                         eaten_raw_meat_life,
                         scratched_bitten_life,
                         hunted_animal_life
                         ),
            #map yes to TRUE and all other responses to FALSE
            .funs = funs(
              if_else(str_detect(., '[Yy]es'),
                      true = TRUE,
                      false = FALSE))
  ) %>%
  #add count for occupation to aggregate rare categories
  add_count(occupation) %>%
  #aggregate rarre occupations into 'rare' column
  mutate(occupation = if_else(n < 10, 'rare', as.character(occupation))) %>%
  #reconvert to factor
  mutate(occupation = as.factor(occupation)) %>%
  #expansion of factors to binary categorical outcomes
  ed2_expand_wide(occupation) %>%
  ed2_expand_wide(highest_education) %>%
  ed2_expand_wide(highest_education_mother) %>%
  ed2_expand_wide(gender) %>%
  ed2_expand_wide(length_lived) %>%
  ed2_expand_wide(live_state_prov) %>%
  ed2_expand_wide(work_location_prov) %>%
  #drop counting and other expanded factor variables
  select(-n,
         -occupation,
         -highest_education,
         -highest_education_mother,
         -gender,
         -length_lived,
         -live_state_prov,
         -work_location_prov
         ) %>%
  #scale and center numeric variables to mean 0 and sd 1
  mutate_if(is.numeric, scale)

}

####################################################################################
######################## example covariate data ####################################

# BD_covars <- BD_dat %>% get_clean_illness_covariates()

####################################################################################
######################## get symptoms response data function #######################

#'@function obtain and flatten self reported symptom outcome data
#'@param dat data frame for which to obtain self-reported outcomes
#'@param of_interest logical indicating to only select outcomes of interest as per analysis plan
#'@return data frame (tibble) with self reported illness widened to dichotomous variables
#'@author matteo-V
get_self_reports <- function(dat, of_interest = T){
  if(!of_interest){ #if not of interest outcomes
  dat %>%
    select(participant_id,
           symptoms_life,
           symptoms_in_last_year) %>%
    ed2_expand_wide(col = symptoms_life) %>%
    ed2_expand_wide(col = symptoms_in_last_year) %>%
    #drop other reported diseases that are coerced to NA
    select(-symptoms_life, -symptoms_in_last_year) }
  else{
    dat %>%
      select(participant_id,
             symptoms_life,
             symptoms_in_last_year) %>%
      ed2_expand_wide(col = symptoms_life) %>%
      ed2_expand_wide(col = symptoms_in_last_year) %>%
      #drop other reported diseases that are coerced to NA
      select(-symptoms_life, -symptoms_in_last_year) %>%
      select(participant_id, matches('sari|ili|hemorrhagic|encephalitis')) %>%
      select(-matches('life')) #remove lifetime self reports
  }
}

##########################################################################################
# ####################### example self-report dat ##########################################
#
# #test function
# BD_self_reports <- BD_dat %>% get_self_reports()
#
# ##########################################################################################
# ########################### example analysis data #########################################
#
# #TODO: wrap the contaxa dataframe generation into a function
# BD_analysis_dat <- join_all(list(BD_covars, BD_self_reports, BD_contacts_dat),
#                             by = 'participant_id',
#                             type='full')

#############################################################################################
################################## crosstab function ##########################################

#'@function: crosstab, run fisher test (robust to small counts) and output p-value
#'@name run_crosstab
#'@param condition for which to test independence
#'@param outcome the outcome variable of interest
#'@return p value of fisher test
#'@author matteo-V
run_crosstab <- function(dat, condition, outcome){
  #fisher test is robust to small counts
  fisher.test( table(dat[,condition], dat[,outcome]) )$p.val
}

#test run_crosstab
# run_crosstab(dat = BD_analysis_dat,
#              condition = 'gender',
#              outcome = 'poultry_contact_cooked_handled')

################################################################################################
###################################### run contaxa crosstab function ###########################
#'@function run crosstab for exposures (taxa contacts)
#'@depends run_crosstab
#'@param analysis_dat cleaned covariate data
#'@param condition_var variable on which to condition (test) for the exposures
#'@param alpha the alpha value for which to assess significance
#'@return data frame of results for condition, taxa, contact type, p-value and significance (bonferroni corrected)
#'@author matteo-V
run_contaxa_crosstab <- function(analysis_dat, condition_var, alpha = 0.05){

  #get unique contact types from data set
  #TODO: will need to fill these using coalesce later
  contx_vars <-
    analysis_dat %>%
    select(matches("_contact")) %>% #get contact cols
    colnames()

  #empty dataframe to store results
  res_frame <- data_frame(var = character(), taxa_type = character(), contx_type = character(), p_value = numeric())

  for(contx_var in contx_vars){
    #parse taxa name
    taxa <- str_remove_all(contx_var, "(no_|_contact.*)") #remove extraneous tokens
    #parse contaxa name
    contx <- str_remove(contx_var, "(.*_contact_|.*_contact)") #empty string correspond to no_contact
    #calculate p_values
    p_val <- run_crosstab(BD_analysis_dat, condition = condition_var, outcome = contx_var)
    #create new dataframe row
    res <-  data_frame(var = condition_var, taxa_type = taxa, contx_type = contx, p_value = p_val)
    #append to results
    res_frame <- rbind(res_frame, res)

  }
  #TODO: clean up empty contx_type
  res_frame <-
    res_frame %>%
    mutate(contx_type = if_else(condition = contx_type == '',
                                true = 'no contact',
                                false = contx_type)) %>%
    #create significance variable using bonferroni correction that is
    #pval < alpha = 0.05/length(contx_vars) since this is the number of indiv. tests
    mutate(significant = p_value < alpha/length(contx_vars)) %>%
  #TODO: clean up contx_type to reflect same names as other data set
    mutate(contx_type = case_when(
      contx_type == 'cooked_handled' ~ str_replace(contx_type, "_", "/"),
      contx_type == 'eaten_sick' ~  str_replace(contx_type, "_", " "),
      contx_type == 'feces_in_or_near_food' ~ str_replace_all(contx_type, "_", " "),
      contx_type == 'scratched_bitten' ~ str_replace(contx_type, "_", "/"),
      contx_type == 'in_house' ~ str_replace(contx_type, "_", " "),
      contx_type == 'hunted_trapped' ~ str_replace(contx_type, "_", "/"),
      contx_type == 'found_dead_collected' ~ 'found dead/collected',
      contx_type == 'eaten_raw_undercooked' ~ 'eaten raw/undercooked',
      contx_type == 'slaughtered' ~ 'slaughtered',
      contx_type == 'pets' ~ 'pets',
      contx_type == 'raised' ~ 'raised',
      contx_type == 'handled' ~ 'handled',
      contx_type == 'no contact' ~ 'no contact'
      )
    )
  return(res_frame)
  }

# #test function
# BD_contaxa_crosstab <- run_contaxa_crosstab(analysis_dat = BD_analysis_dat,
#                                               condition_var = 'gender')

#TODO: write wrapper run_all_contaxa_crosstab for all covariates of interest
#TODO: output should be a concat table (store in list) of all results
#################################################################################################
# library(stargazer)
#TODO: look into this output type
#TODO: look into knitr and RMD for these things
# stargazer(BD_contaxa_crosstab, type = 'latex', summary=F, rownames=F, dep.var.labels = c('p_value', 'significant'))
# saveRDS(BD_contaxa_crosstab, file='data/BD_contaxa_crosstab.rds')
###############################################################################################
############################# Train test val split function ###################################

#'@function train_test_val
#'@param dat dataframe to split for training
#'@return named list of data frames
#'@author matteo-V
train_test_split <- function(dat){
  #empty list for data frames
  res <- list()
  #get total n
  n <- nrow(dat)
  n1 <- floor(n * 0.7)
  n2 <- floor(n * 0.3)
  #sample random integer numbers
  ii <- sample(1:n,n)
  #partition into 3 split data set
  train <- dat[ii[1:n1],]
  test  <- dat[ii[n1+1:n2],]
  #assign output to named list
  res[['train']] <- train
  res[['test']] <- test
  #return list
  return(res)
  }

#################################################################################################
############################# create illness analysis frame function ############################

#'@function creates data frame ready for random forest
#'@depends get_clean_exposures
#'@depends get_self_reports
#'@depends get_clean_illness_covariates
#'@param dat data frame from which to create analysis data set created from get_self_reports or get_contaxa
#'@param outcome_var outcome variable of interest keyword (sari, ili, encehpalitis or hemorrhagic)
#'@param taxa_names vector of taxa types for which exposures willbe included
#'@author matteo-V
get_illness_analysis_dat <- function(dat,
                                       outcome_var,
                                       taxa_names = c('rodents', 'nhp', 'bats', 'swine', 'poultry')){

  #outcome var name
  outcome_var_name <-
    dat %>%
    get_self_reports() %>%
    select(matches(outcome_var)) %>%
    colnames()

  #create outcome frame
  outcome <-
    dat %>%
    get_self_reports() %>%
    select(participant_id, matches(outcome_var)) %>%  #use regular expression to match outcome type
    mutate(!!sym(outcome_var_name) := as.factor(!!sym(outcome_var_name)))

  #covariate dat
  covars <-
    dat %>%
    get_clean_illness_covariates()

  #add exposure data?
  xposures <-
     dat %>%
    get_clean_exposures(taxa_names = taxa_names)

  #analysis data set
  analysis_dat <-
    join_all(list(outcome, covars, xposures),
         by='participant_id') %>%
    select(-participant_id) #drop key variable for analysis

  #return analysis frame
  analysis_dat
}

#test create analysis frame
# ili_model_dat <-
#   BD_dat %>%
#   get_illness_analysis_dat(outcome_var = 'ili')
##############################################################################################
############################## remove_colinear_columns #######################################

#'@name remove_colinear_columns
#'@function uses matrix rank algorithm to remove linearly dependent columns from data
#'@param dat data frame generated by get_XXX_analysis_frame
#'@return named list of removed variable names (removed_vars) and matrix for lasso modeling (data_matrix)
remove_colinear_columns <- function(dat){
  outcome <- dat[,1]
  outcome_var <- colnames(dat)[1]
  #TODO: test rank reduce algo on full data matrix (with interactions)
  #TODO: allow users to specify interactions
  #form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.', ' + ', '(.)^2'))
  #model_matrix <- model.matrix(form, data = dat)
  model_matrix <- data.matrix(dat[,-1])
  mat_rank <- rankMatrix(model_matrix[,-1]) #rank of matrix without outcome variable
  cat('Matrix Rank: ', mat_rank)
  var_names <- colnames(model_matrix) #remove outcome varaible name

  for(ii in 1:length(var_names)){
    tmp_matrix <- model_matrix[,-ii]
    tmp_rank <- rankMatrix(tmp_matrix)
    if(tmp_rank == mat_rank){ #if rank unchanged with removal
      model_matrix <- model_matrix[,-ii]
    }
    else{
      next; #skip to next iteration
    }
  }
  for(jj in 1:ncol(model_matrix)){

    tmp_matrix <- model_matrix[,-jj]
    tmp_rank <- rankMatrix(tmp_matrix)
    if(tmp_rank == mat_rank){
      model_matrix <-  model_matrix[,-jj]
      }

  }
  final_matrix <- cbind(outcome, model_matrix)
  removed_ii <- (!(colnames(dat[,-1]) %in% colnames(model_matrix)))
  removed_vars <- colnames(dat[,-1])[removed_ii]
  return(list(removed_vars = removed_vars, model_data_matrix = final_matrix))
}

# #test function
# res <- ili_model_dat %>% remove_colinear_columns()
#
# #output logical test that algo works
# cat('Rank matches number of columns?',
#     ncol(res$model_data_matrix)==rankMatrix(res$model_data_matrix))
#
# #create matrix of
# ili_x_matrix <- model.matrix(outcome ~ . + (.)^2, data = data.frame(res$model_data_matrix) )
# ############################# LASSO Regularized Logistic Regression ##########################
#
# #illness_model_dat_split <- train_test_split(illness_model_dat)
# #get optimal lambda
# lambda.fit <- cv.glmnet(x = ili_x_matrix ,
#                        y =  ili_model_dat[,1] ,
#                        nfolds = , #5-fold CV
#                        family = 'binomial',
#                        #lambda = sapply(1:20, function(x) 2^(-x)),
#                        alpha = 1) #LASSO regularize
# plot(lambda.fit)
# cat('Minimum value of lambda by 5-fold CV:', lambda.fit$lambda.min)
# lam <- lambda.fit$lambda.min
# n <- length(ili_model_dat[,1])
#fit glmnet with optimal lambda on training data
# lasso.fit <- glmnet(x = ili_x_matrix,
#                     y = ili_model_dat[,1],
#                     family = 'binomial',
#                     lambda = lam , #use CV'd lambda.min
#                     alpha = 1,  #LASSO
#                     standardize = T,
#                     thresh = 1e-25)
# #summarize model
# lasso.fit
# #get non-zero coefficients
# exp( coef(lasso.fit)[which( coef(lasso.fit) != 0),])
# #get class predictions
# lasso.pred <- predict(lasso.fit,
#                       newx = ili_x_matrix,
#                       type = 'class')
# lasso.pred
#
# #get misclassification rate
# lasso.conf.matrix <- table(ili_model_dat[,1], as.factor(lasso.pred))
# lasso.conf.matrix
# 1 - ( sum(diag(lasso.conf.matrix)) / sum(lasso.conf.matrix))
#
#
# #glmnet multiplies the first term by a factor of 1/n
# betas = coef(lasso.fit,
#              s = lam/n,
#              exact = T,
#              x = ili_x_matrix ,
#              y = as.factor( ili_model_dat[,1] ))[-1] #[-1] removes the intercept
#
# #perform inference for glmnet (LASSO) model
# lasso.inference <-
#   fixedLassoInf(x = ili_x_matrix,
#                 y = as.logical( ili_model_dat[,1] )*1 ,
#                 beta = betas,
#                 lambda = lam
#   )


###############################################################################################
