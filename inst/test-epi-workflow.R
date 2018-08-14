devtools::load_all()
library(eidith)
options(ed_sql_path = "data/PREDICT2data.sqlite")
library(plyr)
library(tidyverse)
library(ggthemes)
#compute odds ratios
ili_OR_res <-
  ed2_human() %>%
  select_country_dat(country_codes = c('BD', 'TH', 'CN', 'ID')) %>%
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
  mutate(missing = if_else(any(is.na(c(LowerCI, OR, UpperCI, significant))), true = T, false=F)) %>%
  mutate(contx_type := str_replace_all(contx_type, '_', ' '))


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
  theme_classic() +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(lineheight = 2, hjust=0.3)) +
  ggtitle('Taxa Contact Risk Factors: SE Asia')


#vis odds life
ggplot(ili_OR_res %>%
         filter(str_detect(var_name, '_life|_last_year')) %>%
         mutate(time_gp = str_extract(var_name, 'life|last_year')) %>%
         mutate(time_gp = str_replace_all(time_gp, '_', ' ')) %>%
         mutate(behavior = str_remove_all(var_name, "_life|_last_year")) %>%
         mutate(behavior = str_replace_all(behavior, '_', ' '))
       ,
       aes(x=behavior, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, col=time_gp), position=position_dodge(0.5)) +
  theme_classic() +
  coord_flip() +
  ggtitle('ILI Behavioral Risk Factors: SE Asia') +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_discrete(name = 'Proximity') +
  xlab("Behavior") +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(lineheight = 2, hjust=0.3)) +
  ylab('Odds Ratio')
  # facet_grid(time_gp~.)

#vis odds education
ggplot(ili_OR_res %>%
         filter(str_detect(var_name, 'education')) %>%
         mutate(person = if_else(str_detect(var_name, 'mother'),
                                 true = 'Maternal',
                                 false = 'Respondent')) %>%
         mutate(level = str_extract(var_name,
                                    '(primary|secondary|none|college)_?.*')) %>%
         filter(!is.na(level)) %>%
         mutate(level = str_replace_all(level, '_', ' ')),
       aes(x=level, y = OR, group=person)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, color=person), position=position_dodge(0.2)) +
  theme_classic() +
  coord_flip() +
  ggtitle("Educational Risk Factors: SE Asia") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(lineheight = 2, hjust=0.3)) +
  xlab('Educational Level') +
  ylab('Odds Ratio') +
  scale_color_discrete(name='Recipient')

#scratched bitten action
ggplot(ili_OR_res %>%
         filter(str_detect(var_name,'scratched_bitten_action')) %>%
         mutate(var_name = str_replace_all(str_remove_all(var_name, 'scratched_bitten_action_'), '_', ' ')) %>%
         filter(!var_name == 'missing'),
       aes(x=var_name, y = OR)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, color=var_name), position=position_dodge(0.2)) +
  theme_classic() +
  coord_flip() +
  ggtitle("Hygienic Practice after Injury: SE Asia") +
  theme(plot.title=element_text(hjust=0.5)) +
  guides(color=F) +
  theme(axis.text = element_text(size=10),
                         axis.title = element_text(size=12),
                         axis.title.x = element_text(lineheight = 2, hjust=0.3)) +
  xlab('Action When Injured') +
  ylab('Odds Ratio')

#water
ggplot(ili_OR_res %>%
         filter(str_detect(var_name,'drinking_water|water_treatment')) %>%
         mutate(type = str_extract(var_name, 'drinking_water_source|water_treatment')) %>%
         mutate(var_name = str_remove_all(var_name, 'drinking_water_source|water_treatment')) %>%
         mutate(var_name = str_replace_all(var_name, '_', ' ')) %>%
          mutate(type = str_replace_all(type, '_', ' ')),
       aes(x=var_name, y = OR, group=type)) +
  geom_hline(aes(yintercept=1), lty=2) +
  geom_pointrange(aes(ymin=LowerCI, ymax=UpperCI, col=type), position=position_dodge(0.2)) +
  theme_classic() +
  coord_flip() +
  ggtitle("Water Infrastructure: SE Asia") +
  ylab('Odds Ratio') +
  xlab('Water Insfrastructure') +
  scale_color_discrete(name='Infrastructure Type') +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(lineheight = 2, hjust=0.3))

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
  theme_classic() +
  coord_flip() +
  ylab('Odds Ratio') +
  xlab('Occupation') +
  ggtitle('Occupational Risks: SE Asia') +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.title.x = element_text(lineheight = 2, hjust=0.3)) +
  guides(col=F)
