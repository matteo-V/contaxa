library(plyr) #for join all
library(tidyverse)
#check status of DB
ed_db_check_status()

#get sample of human questionnaire data from lcoal db
dat <- ed2_human()

#subset data from bangladesh and select taxa contact data
BD_dat <- dat %>%
  filter( str_detect(event_name, "BD-") == T ) %>%
  select( participant_id, matches("_contact") )


#get column names minus participant ID
cntacts <- colnames(BD_dat)[-(1)]

#This breaks the function
#wide expand with contact vector
#BD_wide_dat <- ed2_expand_wide(BD_dat, col=cntacts)

#TODO: THIS IS STUPID/REPETITIVE
#TODO: Fashion this into a wrapper function so that contact taxa (vector) may be passed here
# should join all the tables back together so that this can be visualized

# wide expand the 5 taxa of interest
#rodents
BD_rod_dat <- BD_dat %>%
  select(participant_id, rodents_contact) %>%
  ed2_expand_wide(rodents_contact) %>%
  rename(no_rodent_contact = n_a) %>%
  select(-(rodents_contact)) #drop col

#bats
BD_bats_dat <- BD_dat %>%
  select(participant_id, bats_contact) %>%
  ed2_expand_wide(bats_contact) %>%
  rename(no_bat_contact = n_a) %>%
  select(-(bats_contact)) #drop col

#NHPs
BD_nhp_dat <- BD_dat %>%
  select(participant_id, nhp_contact) %>%
  ed2_expand_wide(nhp_contact) %>%
  select(-(nhp_contact))
#poultry
BD_poultry_dat <- BD_dat %>%
  select(participant_id, poultry_contact) %>%
  ed2_expand_wide(poultry_contact) %>%
  select(-(poultry_contact))
#swine
BD_swine_dat <- BD_dat %>%
  select(participant_id, swine_contact) %>%
  ed2_expand_wide(swine_contact) %>%
  select(-(swine_contact))

#join all data frames
BD_contact_dat <- join_all(
  list(BD_rod_dat,
       BD_bats_dat,
       BD_nhp_dat,
       BD_poultry_dat,
       BD_swine_dat),
  by='participant_id',
  type='full')

