library(lavaan)
library(semPlot)

alldat_covars <-
  dat %>%
  get_clean_illness_covariates()

extra_dat <-
  dat %>% select(participant_id,
                 dedicated_location_for_waste,
                 food_storage_containers) %>%
  ed2_expand_wide(food_storage_containers)

wash_predict_dat <- join(extra_dat, alldat_covars, by='participant_id')
ses.model <-
'
ses =~
highest_education_none
highest_education_mother_none  +
dwelling_permanent_structure +
animals_in_food_life +
animals_in_dwelling_life


#variances

highest_education_mother_none ~~ 0.249 * highest_education_mother_none
rooms_in_dwelling ~~ 1 * rooms_in_dwelling
people_in_dwelling ~~ 1 * people_in_dwelling
'

#fit model
ses.fit <- cfa(model = ses.model, data = alldat_covars)

#get summary
summary(ses.fit, standardized = T, fit.measures = T, rsquare = T)

#check model fit
AIC(ses.fit)

#plot model
semPaths(ses.fit, whatLabels = 'std',
         edge.label.cex = 0.8,
         rotation=2,
         what = 'std',
         edge.color = 'purple',
         nCharNodes = 15,
         sizeMan = 12,
         sizeMan2 = 12)

#####################################################################################################
wash.model <-
'
#low WASH indicator
low_wash =~
drinking_water_source_uncovered_well_pond_river +
water_used_by_animals +
shared_water_last_year +
scratched_bitten_action_nothing_kept_working
'
#fit wash model
wash.fit <- sem(model = wash.model, data = pd_dat )

#summarize
summary(wash.fit, standardized = T, fit.measures = T, rsquare = T)

#check model fit
AIC(wash.fit)

#draw SEM Plot
semPaths(wash.fit,
         whatLabels = 'std',
         rotation = 2,
         edge.color = 'blue',
         what = 'std',
         nCharNodes = 25,
         sizeMan = 15,
         sizeMan2 = 10
         )

#predict wash indicator, need to subset dataframe to only include relevant variables
pd_dat <-
  alldat_covars %>% select(participant_id,
                         drinking_water_source_uncovered_well_pond_river,
                         water_used_by_animals,
                         shared_water_last_year,
                         scratched_bitten_action_nothing_kept_working) %>%
  mutate_if(is.logical, as.numeric)


#create wash data
wash_dat <-
  alldat_covars %>%
  mutate(wash_index = lavPredict(wash.fit)) %>%
  select(participant_id, wash_index)

#
all_demo_dat <-
  dat %>%
  get_country_codes() %>%
  get_concurrent_sites() %>%
  select(participant_id,
         country,
         primary_livelihood,
         livelihood_groups_other,
         highest_education_mother,
         highest_education,
         concurrent_sampling_site
         ) %>%
  get_clean_occupations() %>%
  select(-primary_livelihood, -livelihood_groups_other)




wash_viz_dat <-
  join(all_demo_dat, wash_dat, by='participant_id')

ggplot(dat = wash_viz_dat, aes(x = country)) + geom_boxplot(aes(y = wash_index))

ggplot(dat = wash_viz_dat) +
  geom_boxplot(aes(y = wash_index)) +
  #geom_violin(aes(y = wash_index, x = country)) +
  facet_wrap(~country, strip.position = 'bottom') +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(-0.4, 0.6)) +
  theme(axis.text.x.bottom = element_blank()) +
  ylab('Wash Index') +
  xlab('Occupation') +
  ggtitle('WASH Index by Country')

