devtools::load_all()
#compute odds ratios
ili_OR_res <-
  dat %>%
  select_country_dat(country_codes = c('BD', 'TH')) %>%
  calculate_illness_odds(outcome_var = 'ili')


contact_OR <-
  ili_OR_res %>%
  filter(str_detect(var_name, '_contact')) %>%
  mutate(taxa_type = str_extract(var_name, 'rodents|bats|poultry|nhp|swine')) %>%
  mutate(contx_type = str_extract(var_name, 'no|_contact_.*')) %>%
  mutate(contx_type = str_remove(contx_type, '_contact_')) %>%
  mutate(significant = if_else(condition = pvalue < (0.05/length(pvalue)),
                               true = T,
                               false = F)) %>%
  mutate(contx_type = fct_relevel(as.factor(contx_type), 'no')) %>%
  tidyr::complete(contx_type, taxa_type,  fill = list(LowerCI=NA, UpperCI=NA, OR=NA)) %>%
  rowwise() %>%
  mutate(missing = if_else(any(is.na(c(LowerCI, OR, UpperCI, significant))), true = T, false=F))


#viz odds ratios for contacts
ggplot(contact_OR,
       #filter(str_detect(var_name, "_last_year") | str_detect(var_name, '_life')),
       aes(x=contx_type, y=OR, group = taxa_type)) + #x can be contx type
  #line style plot
  # geom_line(aes(col=taxa_type), lwd=1.2) +
  # geom_pointrange(data=contact_OR %>% filter(significant==F),
  #                 aes(ymin=LowerCI, ymax=UpperCI),
  #                 position=position_dodge(0.7),
  #                 col='black') +
  #point style plot
  geom_hline(aes(yintercept=1), lty=2,col='black') +
  geom_pointrange(data=contact_OR,
                  aes(ymin=LowerCI, ymax=UpperCI, col=taxa_type),
                  position=position_dodge(0.7)) +
  geom_text(data = contact_OR %>% filter(missing==T),
            aes(x=contx_type, y=0.01, label='*'), size=4) +
  coord_flip() + #for points
  ylab("Odds Ratio") +
  xlab("Contact Type") +
  facet_grid(taxa_type~., scales = 'free_y') + #for points
  #facet_grid(.~taxa_type, scales = 'free_y') + #for line
  theme_tufte() +
  theme(strip.text = element_blank())

#vis odds life
ggplot(ili_OR_res %>%
         filter(str_detect(var_name, '_life|_last_year')) %>%
         mutate(time_gp = str_extract(var_name, 'life|last_year')) %>%
         mutate(behavior = str_remove_all(var_name, "_life|_last_year"))
       ,
       aes(x=behavior, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, col=time_gp), position=position_dodge(0.5)) +
  theme_tufte() +
  coord_flip()
  # facet_grid(time_gp~.)

#vis odds education
ggplot(ili_OR_res %>%
         filter(str_detect(var_name, 'education')) %>%
         mutate(person = if_else(str_detect(var_name, 'mother'),
                                 true = 'Maternal',
                                 false = 'Respondent')) %>%
         mutate(level = str_extract(var_name, '(primary|secondary|none|college)_?.*')),
       aes(x=level, y = OR, group=person)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, color=person), position=position_dodge(0.2)) +
  theme_tufte() +
  coord_flip()

#scratched bitten action
ggplot(ili_OR_res %>%
         filter(str_detect(var_name,'scratched_bitten_action')),
       aes(x=var_name, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI), position=position_dodge(0.2)) +
  theme_tufte() +
  coord_flip()

#water
ggplot(ili_OR_res %>%
         filter(str_detect(var_name,'drinking_water|water_treatment')),
       aes(x=var_name, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI), position=position_dodge(0.2)) +
  theme_tufte() +
  coord_flip()

#occupation
ggplot(ili_OR_res %>%
         filter(str_detect(var_name,'occupation')) %>%
         mutate(occupation = str_replace_all(str_remove(var_name,
                                                        'occupation'),
                                             '_',
                                             ' ')),
       aes(x=occupation, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, col=occupation), position=position_dodge(0.2)) +
  theme_tufte() +
  coord_flip() +
  ylab('Odds Ratio') +
  guides(col=F)
