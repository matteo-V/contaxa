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
#' Create cleaned, binary response frame for contx_type
#' @param dat raw data frame, prefereably passed through select_country_dat
#' @param taxa_names character vector of taxa names
#' @return wide data frame with binary exposures of interest
#' @author matteo-V
#' @export
#' @importFrom dplyr %>% select rename
#' @importFrom eidith ed2_expand_wide
#' @importFrom rlang !! sym :=
#' @importFrom plyr join_all
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
#' Create and widen covariate data for self reported illness modeling
#' @param dat a raw human table from EIDITH which to clean
#' @return wide data set with data types cleaned for regression analysis
#' @author matteo-V
#' @export
#' @importFrom dplyr %>% select matches mutate mutate_at mutate_if add_count
#' @importFrom eidith ed2_expand_wide
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
  #drop counting and other expanded factor variables
  select(-n,
         -occupation,
         -highest_education,
         -highest_education_mother,
         -gender,
         -length_lived
         )
  #scale and center numeric variables to mean 0 and sd 1
  #mutate_if(is.numeric, scale())

}

####################################################################################
######################## example covariate data ####################################

# BD_covars <- BD_dat %>% get_clean_illness_covariates()

####################################################################################
######################## get symptoms response data function #######################

#' Obtain and flatten self reported symptom outcome data
#' @param dat data frame for which to obtain self-reported outcomes
#' @param of_interest logical indicating to only select outcomes of interest as per analysis plan
#' @return data frame (tibble) with self reported illness widened to dichotomous variables
#' @author matteo-V
#' @export
#' @importFrom dplyr %>% select
#' @importFrom eidith ed2_expand_wide
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

#' Crosstab, run fisher test (robust to small counts) and output p-value
#' @param condition for which to test independence
#' @param outcome the outcome variable of interest
#' @return p value of fisher test
#' @author matteo-V
#' @importFrom stats fisher.test
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
#' Run crosstab for exposures (taxa contacts)
#' @param analysis_dat cleaned covariate data
#' @param condition_var variable on which to condition (test) for the exposures
#' @param alpha the alpha value for which to assess significance
#' @return data frame of results for condition, taxa, contact type, p-value and significance (bonferroni corrected)
#' @author matteo-V
#' @export
#' @importFrom dplyr %>% select matches data_frame mutate
#' @importFrom stringr str_remove_all str_remove
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
    p_val <- run_crosstab(analysis_dat, condition = condition_var, outcome = contx_var)
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

#' Train test val split function
#' @param dat dataframe to split for training
#' @return named list of data frames
#' @author matteo-V
#' @export
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

#' Create illness analysis frame
#'
#' Creates data frame ready for random forest
#' @param dat data frame from which to create analysis data set created from get_self_reports or get_contaxa
#' @param outcome_var outcome variable of interest keyword (sari, ili, encehpalitis or hemorrhagic)
#' @param taxa_names vector of taxa types for which exposures willbe included
#' @author matteo-V
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang !! sym :=
#' @importFrom plyr join_all
#' @export
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

#########################################################################################
############################## remove_colinear_columns ##################################

# ili_model_dat <-
#   BD_dat %>%
#   get_illness_analysis_dat(outcome_var = 'ili')
##############################################################################################
############################## remove_colinear_columns #######################################


#' Use matrix rank algorithm to remove linearly dependent columns from data
#' @param dat data frame generated by get_XXX_analysis_frame
#' @return named list of removed variable names (removed_vars) and matrix for lasso modeling (data_matrix)
#' @export
#' @importFrom  Matrix rankMatrix
remove_colinear_columns <- function(dat){
  cat('WARNING: This function requires large amonuts of compute power.')
  #outcome <- dat[,1]
  #outcome_var <- colnames(dat)[1]
  # TODO: test rank reduce algo on full data matrix (with interactions)
  # TODO: allow users to specify interactions
  #form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.', ' + ', '(.)^2'))
  #model_matrix <- model.matrix(form, data = dat)
  #remove duplicates
  model_matrix <- remove_duplicate_columns(dat)
  mat_rank <- rankMatrix(model_matrix) #rank of matrix without outcome variable
  cat('\nMatrix Rank: ', mat_rank)
  var_names <- colnames(model_matrix) #remove outcome varaible name

  for(ii in length(var_names):1){
    tmp_matrix <- model_matrix[,-ii]
    tmp_rank <- rankMatrix(tmp_matrix)
    if(tmp_rank == mat_rank){ #if rank unchanged with removal
      model_matrix <- tmp_matrix
    }
    else{
      next; #skip to next iteration
    }
  }

  final_matrix <- cbind(model_matrix)
  removed_ii <- (!(colnames(dat) %in% colnames(model_matrix)))
  removed_vars <- colnames(dat)[removed_ii]
  return(list(removed_vars = removed_vars, model_data_matrix = final_matrix))
}

#' De-duplicate matrix columns
#' @param model_matrix from which to remove duplicates
#' @return matrix with duplicated columns removed for lasso and inference
#' @author matteo-V
#' @importFrom base apply duplicated.default any
#' @noRd
remove_duplicate_columns <- function(model_matrix){
  model_matrix <- model_matrix[,
                               !duplicated.default(
                                 apply(model_matrix, 2,
                                       function(x) {
                                         if(any(!(x %in% c(0,1))))
                                           return(list(x))
                                         else if(x[1]) {
                                           return(list(as.logical(x)))
                                         } else {
                                           return(list(!x))
                                         }
                                       }))]
  return(model_matrix)
}
# ############################# LASSO Regularized Logistic Regression ##########################

#' perform cross-validation to get optimal elastic net parameter
#' @param model_matrix model matrix (features, no response) to be used for logistic regression
#' @param reponse vector of response values (binary) for logisitic regression
#' @param cv_folds number of folds for cross_validation
#' @param verbose logical indicating wheter to print lambda to std output
#' @return lambda which minimizes the binomial deviance
#' @author matteo-V
#' @importFrom glmnet cv.glmnet plot.cv.glmnet
#' @noRd
cv_elasticnet_param <- function(alpha_vals, model_matrix, response, cv_folds){
  cat("Performing alpha cross validation\n")
  res <- list()
  for(alpha in alpha_vals){
    lambda.fit <- cv.glmnet(x = model_matrix,
                            y = response ,
                            nfolds = cv_folds, #5-fold CV
                            family = 'binomial',
                            alpha = alpha)
    res[[as.character(alpha)]] <- lambda.fit$cvm
  }
  res <- stack(res)
  #get min alpha
  as.numeric(as.character(res$ind[which.min(res$values)]))

}

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
cv_shrinkage_param <- function(model_matrix, response, cv_folds=5, verbose = F, ...){
  # best_alpha <- cv_elasticnet_param(seq(0.05,1,by=0.05), model_matrix, response, cv_folds)
  # cat("Best alpha by cross validation:", best_alpha)
  lambda.fit <- cv.glmnet(x = model_matrix,
                          y = response ,
                          nfolds = cv_folds, #5-fold CV
                          family = 'binomial',
                          lambda = 10^-(seq(1, 2, by=0.005)),
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
                      standardize = T,
                      thresh = 1e-30)
  return(lasso.fit)
}


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
                  gridrange = c(1e-6, 1e6))

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
    ggplot(data = lasso_inference_table, aes(x = var_names, y = effect_size)) +
    #geom_point(aes(color = var_names), cex = 4) +
    geom_hline(aes(yintercept = 1), lty=2, color = 'black', alpha = 0.8) +
    geom_errorbar(aes(ymin = lower, ymax=upper, color = var_names), lwd=0.8) +
    geom_text(data= lasso_inference_table  %>% filter(significant==T),
              aes(x=var_names, y=(upper*2), label='*'),
              size=6)+
    ggtitle(title) +
    scale_color_discrete(guide=F) +
    scale_y_continuous(trans = 'log') +
    theme_tufte() +
    coord_flip() +
    xlab('Variables') +
    ylab('Odds Ratio') +
    theme(plot.title = element_text(size=16, hjust=0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=14))
  return(out.plot)
}

###############################################################################################
