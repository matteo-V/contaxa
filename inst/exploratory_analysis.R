library(plyr) #for join all
library(tidyverse)

#check status of DB
ed_db_check_status()

#get sample of human questionnaire data from lcoal db
dat <- ed2_human()

#subset data from bangladesh and select taxa contact data
BD_dat <- dat %>%
  filter( str_detect(event_name, "BD-") == T ) %>% #match event_name country code by regex
  select( participant_id, matches("_contact") ) #match contact cols by regex
###############################################################################################################
############################## create wide data set for taxa of interest function #############################
#TODO: THIS IS STUPID/REPETITIVE
#TODO: Fashion this into a wrapper function so that contact taxa (vector) may be passed here
# should join all the tables back together so that this can be visualized
#wide expand the 5 taxa of interest
#rodents
BD_rod_dat <- BD_dat %>%
  select(participant_id, rodents_contact) %>%
  ed2_expand_wide(rodents_contact) %>%
  rename(no_rodent_contact = n_a) %>% #rename n_a for interpretability
  select(-(rodents_contact)) #drop col

#bats
BD_bats_dat <- BD_dat %>%
  select(participant_id, bats_contact) %>%
  ed2_expand_wide(bats_contact) %>%
  rename(no_bat_contact = n_a) %>% #rename n_a for interpretability
  select(-(bats_contact)) #drop raw col

#NHPs
BD_nhp_dat <- BD_dat %>%
  select(participant_id, nhp_contact) %>%
  ed2_expand_wide(nhp_contact) %>%
  rename(no_nhp_contact = n_a) %>% #rename n_a for interpretability
  select(-(nhp_contact)) #drop raw col

#poultry
BD_poultry_dat <- BD_dat %>%
  select(participant_id, poultry_contact) %>%
  ed2_expand_wide(poultry_contact) %>%
  rename(no_poultry_contact = n_a) %>% #rename n_a for interpretability
  select(-(poultry_contact))  #drop raw col

#swine
BD_swine_dat <- BD_dat %>%
  select(participant_id, swine_contact) %>%
  ed2_expand_wide(swine_contact) %>%
  rename(no_swine_contact = n_a) %>% #rename n_a for interpretability
  select(-(swine_contact)) #drop raw col

#join all data frames
#'@import: join_all() from plyr
BD_contact_dat <- join_all(
  list(BD_rod_dat,
       BD_bats_dat,
       BD_nhp_dat,
       BD_poultry_dat,
       BD_swine_dat),
  by='participant_id') #join by key

#save joined contaxa data to .rds
saveRDS(BD_contact_dat, file='data/contaxa_BD_dat.rds')
#####################################################################################################################
#####################################################################################################################
#TODO: create human data table to join
BD_demo_dat <- dat %>%
  filter(str_detect(event_name, "BD-") == T) %>% #filter BD data
  select(participant_id, #select vars for visuals
         interview_state_prov,
         gender,
         age,
         primary_livelihood)

###################### Save example data sets #######################################################################
#join demo and contact tables
BD_viz_dat <- join(BD_demo_dat, BD_contact_dat, by='participant_id') #join on keys

#save vis data set
saveRDS(BD_viz_dat, file='data/BD_viz_dat.rds')

#join full data set for regularized GLM (logisitic regression)
BD_full_dat <- join(BD_dat, BD_contact_dat, by='participant_id')

#save modeling data set
saveRDS(BD_full_dat, file='data/BD_model_dat.rds')

######################################################################################################################
