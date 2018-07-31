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

###################################################################

#'@function to filter country data by country codes
#'@return full data set, filtered by countries of interest
#'@param dat human data table from EIDITH
#'@param country_codes a vector of 2 digit country codes of interest
#'@author matteo-V
select_country_dat <- function(dat, country_codes){

  #build country regex from passed character vector
  country_regex <- country_codes %>%
    str_flatten(collapse="|") #join with regex disjunction

  #select desired data
  dat %>%
    filter(str_detect(event_name, country_regex)==T)  #filter event_names by regex
}

#'@function to select taxa contact cols by taxa names
#'@returns participantID (key) and taxa columns of interest
#'@param dat is the data set from which to select taxa columns
#'@param taxa_types a vector of taxa names of interest
#'@author matteo-V
select_taxa_dat <- function(human_dat, taxa_types){
  #build taxa regex
  taxa_regex <- taxa_types %>%
    str_flatten(collapse="|") %>%
    paste("_contact", sep='')
  #select unique ID (for joining) and taxa contact of interest
  human_dat %>%
    select(participant_id, matches(taxa_regex))
}

#'@function to get colnames from list of taxa
#'@return colnames for taxa data of interest
#'@depend ed2_human
#'@helper reshape_contaxa_data function
#'@param human_dat is the data set from which to select taxa columns
#'@param taxa_types a vector of taxa names of interest
#'@author matteo-V
get_taxa_colnames <- function(human_dat, taxa_types){
  #build taxa regex
  taxa_regex <- taxa_types %>%
    str_flatten(collapse="|") %>%
    paste("_contact", sep='')

  #get colnames
  human_dat %>%
    select(matches(taxa_regex)) %>% #select colnames by taxa
    colnames()
}

#'@name get_concurrent_sites
#'@function to get concurrent site names for within country visualizations
#'@depend ed2_human & ed2_event
#'@param human_dat from ed2_human
#'@param event_dat from ed2_event
#'@return human_dat frame with concurrent sites added
#'@author matteo-V
get_concurrent_sites <- function(human_dat){

  sites_dat <-
    ed2_events() %>%
    select(event_name, concurrent_sampling_site)

  join(human_dat, sites_dat, by='event_name', type='left')

}

#'@name get_country_codes
#'@function creates varaible with two letter country code for cross country viz
#'@depend ed2_human
#'@param human_dat EIDITH human table with event_name
#'@return human_dat frame with country code added
#'@author matteo-V
get_country_codes <- function(human_dat){
  human_dat %>%
    mutate(country = str_extract(event_name, '[A-Z]{2}-')) %>%
    mutate(country = str_remove(country, '-'))

  }

#'@name get_clean_occupations
#'@function creates new variables occupation using all pertinent data from survey, can also aggregate rare
#'@param human_dat EIDITH human data table
#'@param aggregateRate logical indicating whether uncommon occupations should be aggregated
#'@param threshold number below which a occupation is considered 'rare'
#'@return human data table with new occupation column
#'@author matteo-V
get_clean_occupations <- function(human_dat,
                                  aggregate_rare = T,
                                  threshold = 10,
                                  aggregate_value = 'rare'){
  res <- human_dat %>%
  mutate(occupation = #create new occupation variable
           if_else( #condition
             condition = str_detect(primary_livelihood, "[Oo]ther"), #if other
             true = livelihood_groups_other, #use other group to population occupation
             false = primary_livelihood #else use primary livelihood
           )
  ) %>%
    #TODO: now clean up missing values in occupation with primary livelihood
    mutate(occupation =
             if_else(
               condition = occupation == '', #if occupation is empty since livelihood groups other was empty
               true = primary_livelihood, #use primary_livelihood to populate
               false = occupation #else leave it the same
             )
    ) %>%
    #map other columns with 'other' values back to "Other" because the cleaning never ends
    mutate(occupation =
             if_else(condition = str_detect(occupation, '[Oo]ther'),
                     true = 'Other',
                     false = occupation
             )
    ) %>%
    #make occupation a factor
    mutate(occupation = as.factor(occupation))
  if(aggregate_rare){
    #add count for occupation to aggregate rare categories
    res <-
      res %>%
      add_count(occupation) %>%
      #aggregate rarre occupations into 'rare' column
      mutate(occupation = if_else(n < threshold, aggregate_value, as.character(occupation))) %>%
      #reconvert to factor
      mutate(occupation = as.factor(occupation))
  }
  #remove count column
  res <-
    res %>%
    select(-n)
  return(res) #output dataset
}

###### test country and taxa pipeline functions ##############
# tst_country <- dat %>%
#   select_country_dat(country_codes = 'BD') %>%
#   select_taxa_dat(taxa_types = taxa_types)
##############################################################
###### test taxa colnames pipeline function ##################
# cols <- dat %>%
#   select_country_dat(country_codes = 'BD') %>%
#   get_taxa_colnames(taxa_types = taxa_types)
##############################################################
######## bangladesh data for testing #########################
# BD_dat <- dat %>%
#   filter(str_detect(event_name, "BD")==T) %>%
#   select(participant_id, #select only taxa of interest
#          matches("rodents|bats|nhp|poultry|swine_contact")
#          )
###################################################################
################# reshape contaxa data function ###################

#'@function function to unnest and restructure taxa contact cols into
#'@depends get_taxa_colnames function
#'@depends select_taxa_dat function
#'@param dat returned from select_taxa_dat function
#'@param taxa vector of taxa types of interest
#'@author: matteo-V
#'@return dataframe with participant_id (key), taxa_type, contx_type, and aggregate sum
reshape_contaxa_data <- function(dat, taxa){

  #empty list for dataframes
  full_dat <- list()
  #cols to iterate over
  cols <-
    dat %>%
    get_taxa_colnames(taxa_types = taxa)
  #build taxa regex
  taxa_regex <- taxa %>%
    str_flatten(collapse="|")

  #iterate through taxa of interest
  for(i in cols){
    var_name <- i
    dat_i <- dat %>%
      select(participant_id, var_name) %>% #select vars
      #unquote var name, split multiresponse data
      #treat var_name as symbol and !! -> eval
      mutate(contx_type = str_split( (!! sym(var_name)) , ';')) %>%
      unnest(contx_type) %>%  #unnest split data into "long" format
      #trim trailing and leading whitespace
      mutate(contx_type = str_trim(contx_type, 'both')) %>%
      rename(taxa_type = var_name) %>%  #rename taxa_contact col to taxa_type
      #extract taxa type from var_name
      mutate(taxa_type = str_extract(var_name, taxa_regex)) %>%
      #replace blanks with no contact for readability
      mutate_at(.vars = 'contx_type',
                .funs = funs(replace(., .=="", 'no contact')))

    #add data set to list of data, indexed
    full_dat[[i]] <- dat_i
  }

  #concatenate data sets
  res <- do.call(rbind, full_dat)

  #output concatenated data set
  return(res)
  }

#test contaxa function
BD_contaxa_dat <- reshape_contaxa_data(dat = BD_dat, taxa = taxa_types)

############################################################################
######## test demographic selection pipeline ###############################
# vars <- list('participant id', 'gender', 'age', 'primary livelihood')
# demo_vars <- str_replace(vars, pattern=' ', replacement= '_')
# tst_demo <- dat %>%
#   filter(str_detect(event_name, "BD")==T) %>%
#   select( demo_vars )
#
# tst_join_dat <- join(tst_contaxa_fn, tst_demo, by='participant_id')
####################################################################
############ age quintiles functions ###############################
#'@name quintiles
#'@function generates non-overlapping age ntiles
#'@param x age variable
#'@param m number of ntiles to generate
#'@helper to get age quintiles
#'@function to generate non-overlapping quintiles
ntiles <- function(x, m = 5){
  qrt <- quantile(x,probs=seq(0, 1, 1/m) )
  y <- x
  for (i in (1:m) ) {y[x<=qrt[m+2-i]] <- m+1-i}
  return(y)
}

#TODO: could this be a general variable quintile function
#'@name get_age_quitiles
#'@depend quintiles function
#'@function to annotate dataframe with age quintiles
#'@param dat EIDITH demographic dataframe which to annotate (must contain age column)
#'@param ntiles passed to quntiles function
#'@author matteo-V
get_age_ntiles <- function(dat, ntiles = 5){
  #create age quintile column
  dat %>%
    mutate(age = as.numeric(age)) %>%
    mutate(age_quint = ntiles(age, ntiles)) %>%
    group_by(age_quint) %>% #group
    mutate(age_quint_range = paste0(min(age), '-', max(age))) %>% #create level with min-max for quintile
    ungroup()
}


######################################################
#############demo select function ##################################

#'@name select_demo_dat
#'@function to select demographic vars from data
#'@param dat EIDITH human dataframe from which to select variables
#'@depend get_age_quintiles to calculate age quintiles
#'@depends ed2_human
#'@return dataframe of demographic data and participantID (key)
#'@author matteo-V

select_demo_dat <- function(dat){
  #select demo vars of interest for joining
  dat %>%
    get_concurrent_sites() %>% #get concurrent sampling sites
    select( participant_id, #unique ID
            gender, #gender
            age, #age
            concurrent_sampling_site, #concurrent site
            primary_livelihood,
            livelihood_groups_other,
            highest_education, #respondent ed
            highest_education_mother) %>%  #maternal ed
    get_age_ntiles() %>% #get age quintiles
    get_clean_occupations() %>% #clean occupation column
    select(-primary_livelihood, -livelihood_groups_other)
}

#test select demo dat function
BD_demo <- dat %>%
  select_country_dat(country_codes = 'BD') %>%
  select_demo_dat()
#######################################################
############## join contaxa and demo data ################

#'@name join_contx_and_demo
#'@function to join contx data and demo data
#'@depends reshape_contaxa_data & select_demo_dat
#'@param contx_dat from reshape_contaxa_data function
#'@param demo_dat from select_demo_dat function
#'@returns joined contaxa  and demographic variables frames
join_contx_and_demo <- function(contx_dat, demo_dat){
  #join dat on unique ID
  join(contx_dat, #longest data frame first for left join
       demo_dat, #demo data
       by = 'participant_id', #Unique ID
       type = 'left') #left join since contx is longer
}

#test function
BD_join_dat <- join_contx_and_demo(BD_contaxa_dat,
                                    BD_demo)

##############################################################
#test age quintiles (start with quintiles)
# tst_ntile_joined_dat <-
#   tst_join_dat %>%
#   get_age_quintiles()
############################################################################
#### test total respondents by variable pipeline ###########################
# var_name <- 'gender'
# tst_demo %>%
#   filter( (!! sym(var_name)) == 'male') %>% #select var where var == var_val
#   group_by( (!! sym(var_name)) ) %>%  #group by var
#   summarise(total_count = n()) %>% #count number of rows
#   select(total_count) %>% #select new data column
#   unlist() %>%  #unist object
#   unname() #remove name
#########################################################################
######### calc total respondents function ####################################

#'@name calc_total_repondents
#'@depend select_demo_dat function
#'@param demo_dat from select_demo_dat function
#'@param var_name stratification variable name
#'@param var_val stratification variable value
#'@function returns total number of respondents where var_name == var_val
#'@author matteo-V
calc_total_respondents <- function(demo_dat, var_name, var_val){
  #calculate number of respondents by var == var_val
  demo_dat %>%
    filter( (!!sym(var_name)) == var_val ) %>%
    group_by( (!!sym(var_name)) ) %>%
    summarise(total_count = n()) %>%  #count rows with val
    select(total_count) %>% #select this
    unlist() %>%  #unlist to vector
    unname() #unname vector
}

###################################################################################
#### get variable values helper for expanding crossprod functions #################

#'@name get_var_vals
#'@helper to get values of variables easily
#'@param dat dataframe from join_contx_and_demo
#'@param var_name variable for which to retrieve values
#'@function for expand_contaxa and calc_respondents functions to get variable values
#'@return vector of variable levels
#'@author matteo-V
get_var_vals <- function(dat, var_name){
  dat %>%
    ungroup() %>%
    select(var_name) %>%
    unique() %>%
    unlist () %>%
    unname()
}

##################################################################################################
############################### general calculate respondents function ###########################

#'@name cal_percent_respondents_by_var
#'@function to calculate percent of respondents with contx_type X taxa_type by stratification var
#'@depend select_demo_dat
#'@param contaxa_dat joined dataframe from join_contx_and_demo
#'@param demo_dat dataframe from select_demo_dat that was used in join_contx_and_demo function
#'@param var_name stratification variable name (character type)
#'@return dataframe with percent of respondents normalized by stratification variable count
calc_percent_repondents_by_var <- function(contaxa_dat,
                                           demo_dat,
                                           var_name){

  var_vals <- get_var_vals(dat = contaxa_dat, var_name = var_name)

  #pipeline for final results data
  res <-
    contaxa_dat %>%
    group_by(!!sym(var_name), #stratification variable
             contx_type,
             taxa_type) %>%
    summarise(num_respondents = n()) %>% #could change to count(num_respondents)
    mutate(num_respondents = as.numeric(num_respondents)) #change to numeric type
  #iterate and calc num_respondents
  for(var_val in var_vals){
    res <-
      res %>%
      mutate_at(.vars = 'num_respondents',
                .funs = funs(
                if_else(condition = (!!sym(var_name)) == var_val,
                        true = num_respondents / calc_total_respondents(demo_dat,
                                                                        var_name = var_name,
                                                                        var_val = var_val),
                        false = num_respondents)
                )
      )} #end for loop
  res <-
    res %>%
    #rename to reflect change to percentage over repsondents
    rename(percent_respondents = num_respondents)

  #return mutated data set with renamed col
  return(res)
}
####################################################################################
######################## test caluclate percent general function ###################
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
#####################################################################################
########################### general expand contaxa by variable function #############
#'@name expand_contaxa_crossprod_by_var
#'@function fill in dataframe with all crossprods of (contx_type X taxa_type)
#'@param dat dataframe returned from calc_percent_respondents_by_var function
#'@param var_name variable name used for stratification  as character
#'@returns dataframe with full crossproduct of (contx_type X taxa_type) filled in for viz functions
#'@author matteo-V
expand_contaxa_by_var <- function(dat, var_name){

  #create support grid of crossprods for replyr_coalesce
  support <-
    expand.grid(var_name = dat %>% get_var_vals(var_name = var_name), #re-coerce to character
                contx_type = dat %>% get_var_vals(var_name = 'contx_type'), #could hard-code these?
                taxa_type = dat %>%  get_var_vals(var_name = 'taxa_type'), #could hard-code these?
                stringsAsFactors = FALSE)
  #rename first column of support to match stratification variable in data frame
  names(support)[1] <- var_name
  #fill in missing crossproducts
  dat %>%
    replyr_coalesce(support = support,
                    fills=list(percent_respondents = 0.0)) #fills missing crossprods with 0

  }
########################################################################################
########## test general expand contaxa function ########################################

BD_occupation_group_full <-
  BD_occupation_grouped %>%
  expand_contaxa_by_var(var_name = 'occupation')

BD_concurrent_group_full <-
  BD_concurrent_grouped %>%
  expand_contaxa_by_var(var_name ='concurrent_sampling_site')

BD_age_group_full <-
  BD_age_grouped %>%
  expand_contaxa_crossprod_by_var(var_name = 'age_quint_range')
########################################################################################
################################### annotate group N function ##########################

#'@name annotate_group_n
#'@depend ed2_human()
#'@param dat human_dat from ed2_human
#'@param var_name stratification variable name as character
#'@param var_val used to iterate over values of strat variable
#'@return dataframe with (N=XX) appended to the stratification variable
#'@author matteo-V
annotate_group_n <- function(demo_dat, var_name, var_val){
  #calculate and create string of N value
  paste0(' (N=',
         calc_total_respondents(demo_dat, var_name, var_val),
         ')' )
}

#'@name annotate_factor_with_n
#'@depend annotate_group_n
#'@param var_name variable for which to append (N=XX)
#'@param human_dat from ed2_human used to calculate N
#'@param contaxa_data dataframe from reshape_contaxa_data joined with demographic variables
#'@return dataframe with (N=XX) appended to var_name
#'@author matteo-V
annotate_factor_with_n <- function(contaxa_dat, var_name, demo_dat){
  #get variable values to iterate over
  var_vals <- contaxa_dat %>% get_var_vals(var_name = var_name)
  #create output frame
  out <-
    contaxa_dat %>%
    mutate(!!sym(var_name) := as.character(!!sym(var_name))) #convert to character prior to appending
  #get demographic data for counting
  #demo_dat <- human_dat %>% select_demo_dat()
  #iterate for all var values
  for(var_val in var_vals){
    #re-assign data frame with mutated factors to include N
    out <-
      out %>%
      mutate(!!sym(var_name) := if_else( !!sym(var_name) == var_val,
                                         true = paste0(!!sym(var_name),
                                                       annotate_group_n(demo_dat,
                                                                        var_name = var_name,
                                                                        var_val = var_val)),
                                         false = !!sym(var_name) #else keep variable the same
                                         )
        )
    } #end for loop
  return(out) #return mutated data frame
  }
########################################################################################
################# test annotate factor functions #######################################

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
########################################################################################
