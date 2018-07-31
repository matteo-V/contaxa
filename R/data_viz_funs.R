# library(tidyverse) #interactive beauty
# library(plotly) #interactive beauty
# library(ggthemes) #pretty fivethirtyeight theme
# library(viridis)
# library(RColorBrewer)
# library(colorRamps)

#' Get education factor order
#'
#' @author matteo-V
#' @param dat dataframe with education factor levels (with annotation) to reorder
#' @param strat_var stratification variable
#' @return vector of factor levels ordered by decreasing education
#' @importFrom dplyr %>% select
#' @importFrom stringr str_extract
#' @noRd
get_education_factor_order <- function(dat, strat_var){

  strat_levels <-
    dat %>%
    select(strat_var) %>%
    unique()

  none_level <- str_extract(strat_levels[[strat_var]], 'none.*' )  #match none factor level
  none_level <- none_level[!is.na(none_level)]

  primary_level <- str_extract(strat_levels[[strat_var]], 'primary.*')
  primary_level <- primary_level[!is.na(primary_level)]

  secondary_level <- str_extract(strat_levels[[strat_var]], 'secondary.*')
  secondary_level <- secondary_level[!is.na(secondary_level)]

  college_level <- str_extract(strat_levels[[strat_var]], 'college.*')
  college_level <- college_level[!is.na(college_level)]

  return(c(none_level, primary_level, secondary_level, college_level))
  }

#' Get age quantile factor order
#'
#' @param human_dat EIDITH human data table, filtered for country of interest for plotting
#' @return ordered character vector of age_quint_range levels
#' @author matteo-V
#' @importFrom dplyr %>% select filter
#' @importFrom stringr str_extract
#' @noRd
get_age_ntile_factor_order <- function(dat, human_dat){

  #build demo dat
  demo_dat <-
    human_dat %>%
    get_age_ntiles() %>%
    select(age_quint, age_quint_range)

  #calculate number of age_ntiles
  num_age_ntiles <-
    demo_dat %>%
    select(age_quint) %>%
    unique() %>%
    unlist() %>%
    max(.)

  age_ntile_ranges <-
    dat %>%
    select(age_quint_range) %>%
    unique() %>%
    unlist()

  #create output vector
  res <- vector(mode='character', length = num_age_ntiles)

  #iterate through ntiles and create vector of factor orders
  for(ii in 1:num_age_ntiles){
    #build quint regex
    quint_regex <-
      demo_dat %>%
      filter(age_quint == ii) %>% #filter for spefici age_quint
      select(age_quint_range) %>%
      unique() %>%
      paste0(., '.*')

    #extract quint_range level
    level <- str_extract(age_ntile_ranges, quint_regex)
    level <- level[!is.na(level)]
    #assign to vector
    res[ii] <- level
  } #end for loop
  #return vector
  return(res)
}


#' Plot heatmap wrapped by contact type
#' @param strat_var varaible to use on y axis
#' @param factor_order used by plot_contaxa_heatmap to order plotting factors
#' @return unstyled heatmap to be faceted by plot_contaxa_heatmap
#' @author matteo-V
#' @import ggplot2
#' @noRd
plot_contact_wrap_heatmap <- function(contaxa_dat, strat_var, factor_order=NULL){
  ggplot(data = contaxa_dat,
         color = 'gray',
         aes(x = taxa_type,
             y = fct_relevel( !!sym(strat_var),
                             factor_order))) +
    geom_tile(aes(x = taxa_type,
                  y = fct_relevel(!!sym(strat_var),
                                  factor_order),
                  fill = percent_respondents * 100 #normalize to whole percent
                  ),
              color = 'gray') +
    geom_label(aes(label = paste0(round(percent_respondents*100, 0),
                                  '%'),
                   fontface='bold')) +
    scale_x_discrete(position = 'top') + #flip-axes
    scale_fill_viridis(name ='Percentage of Respondents',
                       option = ,
                       limits = c(0,100)) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust=0.5),
          legend.text = element_text(angle=0),
          strip.text = element_text(size=14),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12,
                                     angle=35,
                                     hjust = 0)
    )
  }

#' Create heat map of contx_type X taxa_type
#' @param contaxa_data data passed to
#' @return heatmap to be faceted by plot_contaxa_heatmap
#' @author matteo-V
#' @import ggplot2
#' @noRd
plot_strat_wrap_heatmap <- function(contaxa_dat){
  ggplot(data = contaxa_dat,
         color = 'gray',
         aes(x = taxa_type,
             y = contx_type)) +
    geom_tile(aes(x = taxa_type,
                  y = contx_type,
                  fill = percent_respondents * 100 #normalize to whole percent
    ),
    color = 'gray') +
    geom_label(aes(label = paste0(round(percent_respondents*100, 0),
                                  '%'),
                   fontface='bold')) +
    scale_x_discrete(position = 'top') + #flip-axes
    scale_fill_viridis(name ='Percentage of Respondents',
                       option = ,
                       limits = c(0,100)) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust=0.5),
          legend.text = element_text(angle=0),
          strip.text = element_text(size=14),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12,
                                     angle=35,
                                     hjust = 0)
    )
}


#' Main heatmap function
#' @author matteo-V
#' @param dat contaxa dataframe of frequencies by stratification variable
#' @param strat string of variable name for stratification
#' @param human_dat (optional) used for ordering plots by age_quinte_range
#' @return ggplot2 heatmap object
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom dplyr %>%
#' @export
plot_contaxa_heatmap <- function(dat, strat, human_dat = NULL, wrap_contact = T){

  if(wrap_contact){
    #if education stratification variable
    if(str_detect(strat, 'education')){

      #get education factor order level
      education_factor_order <-
        dat %>%
        get_education_factor_order(strat_var = strat)

      out_plot <- plot_contact_wrap_heatmap(dat,
                                            strat_var = strat,
                                            factor_order = education_factor_order)
    }
    else if(str_detect(strat, 'gender')){

      out_plot <- plot_contact_wrap_heatmap(dat,
                                            strat_var = strat)
    }
    else if(str_detect(strat, 'age_quint_range')){
      #get age quint order, depends on passing in raw, country-filtered data frame
      age_quint_order <-
        get_age_ntile_factor_order(dat, human_dat)

      out_plot <- plot_contact_wrap_heatmap(dat,
                                            strat_var = strat,
                                            factor_order = age_quint_order)
    }
    else{
      out_plot <- plot_contact_wrap_heatmap(dat,
                                            strat_var = strat)
    }
    #now add themes and wrap via facets
    out_plot +
      #reorder factor levels by order in questionnaire
      facet_wrap(~fct_relevel(contx_type,
                              'pets',
                              'handled',
                              'raised',
                              'feces in or near food',
                              'in house',
                              'cooked/handled',
                              'eaten raw/undercooked',
                              'eaten sick',
                              'found dead/collected',
                              'scratched/bitten',
                              'hunted/trapped',
                              'slaughtered',
                              'no contact'),
                 strip.position = 'bottom')
  }
  else{
    # if(str_detect(strat, 'education')){
    #
    #   #get education factor order level
    #   education_factor_order <-
    #     dat %>%
    #     get_education_factor_order(strat_var = strat)
    #
    #   out_plot <- plot_strat_wrap_heatmap(dat)
    # }
    # else if(str_detect(strat, 'gender')){
    #
    #   out_plot <- plot_strat_wrap_heatmap(dat)
    # }
    # else if(str_detect(strat, 'age_quint_range')){
    #   #get age quint order, depends on passing in raw, country-filtered data frame
    #   age_quint_order <-
    #     get_age_ntile_factor_order(dat, human_dat)
    #
    #   out_plot <- plot_strat_wrap_heatmap(dat)
    # }
    # else{
      out_plot <- plot_strat_wrap_heatmap(dat)
    # }
    out_plot +
      #reorder factor levels by order in questionnaire
      facet_wrap(strat,
                 strip.position = 'bottom')
  }

#now add themes and wrap via facets
# out_plot +
# #reorder factor levels by order in questionnaire
# facet_wrap(~fct_relevel(contx_type,
#                         'pets',
#                         'handled',
#                         'raised',
#                         'feces in or near food',
#                         'in house',
#                         'cooked/handled',
#                         'eaten raw/undercooked',
#                         'eaten sick',
#                         'found dead/collected',
#                         'scratched/bitten',
#                         'hunted/trapped',
#                         'slaughtered',
#                         'no contact'),
#            strip.position = 'bottom')
}

