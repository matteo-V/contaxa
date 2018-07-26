library(plyr)
library(tidyverse)
library(plotly)
library(ggvis)

#download all tables
dat <- ed2_human()

#filter bangladesh data
#TODO: paste two digit code and - for string match in a function
BD_dat <- dat %>%
  filter(str_detect(event_name, "BD-")==T) %>%
  select(participant_id, matches("rodents|bats|nhp|poultry|swine_contact")) #select only taxa of interest

#get vector of colnames
cols <- BD_dat %>%
  select(matches("rodents|bats|nhp|poultry|swine_contact")) %>%
  colnames()

#test data manipulation pipeline
tst <- BD_dat %>%
  select(participant_id, rodents_contact) %>%
  mutate(contx_type = str_split(rodents_contact, ';') ) %>%
  unnest() %>%
  rename(taxa_type = rodents_contact) %>%
  mutate(taxa_type = str_extract('rodents_contact', "rodent"))


#function to build 3 column contaxa dataset: participantID, contx_type, taxa_type
get_contaxa_data <- function(cols, dat){

  #empty list for dataframes
  full_dat <- list()

  #iterate through taxa of interest
  for(i in cols){
    var_name <- i
    dat_i <- dat %>%
      select(participant_id, var_name) %>% #select vars
      #unquote var name, split multiresponse data
      mutate(contx_type = str_split( (!! sym(var_name)) , ';')) %>%
      unnest(contx_type) %>%  #unnest split data into "long" format
      rename(taxa_type = var_name) %>%  #rename taxa_contact col to taxa_type
      #extract taxa type from var_name
      mutate(taxa_type = str_extract(var_name, "rodents|bats|nhp|poultry|swine"))
    #add data set to list of data, indexed
    full_dat[[i]] <- dat_i
  }

  #concatenate data sets
  res <- do.call(rbind, full_dat)

  #output concatenated data set
  return(res)
  }

#test data frame function
tst_fn <- get_contaxa_data(cols=cols, dat=BD_dat)

