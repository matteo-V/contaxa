#' Get categorical data for global profile plots
#'
#' @param var_name categorical variable for which to create plot
#' @return data to pass to plot global profile function
#' @author matteo-V
#' @importFrom eidith ed2_human
#' @importFrom dplyr %>% filter group_by ungroup count
#' @importFrom rlang !! sym
#' @export
categorical_global_profile_frame <- function(var_name){

    ed2_human() %>%
    get_country_codes() %>%
    filter(country!='MP') %>%  #drop peninsular malaysia
    group_by(country, !!sym(var_name)) %>%
    count(!!sym(var_name)) %>%
    ungroup()
}


#' Get categorical data for global profile plots
#'
#' @param var_name categorical variable for which to create plot
#' @return data to pass to plot global profile function
#' @author matteo-V
#' @importFrom eidith ed2_human
#' @importFrom dplyr %>% filter group_by ungroup count
#' @importFrom rlang !! sym
#' @export
categorical_global_aggregate_frame <- function(var_name){

  ed2_human() %>%
    get_country_codes() %>%
    filter(country!='MP') %>%  #drop peninsular malaysia
    group_by(!!sym(var_name)) %>%
    count() %>%
    ungroup() %>%
    mutate(fraction = n / sum(n)) %>%
    arrange(fraction) %>%
    mutate(ymax = cumsum(fraction)) %>%
    mutate(ymin = c(0, head(ymax, n=-1)))


}

#' Get continuous data for global profile plots
#'
#' @param var_name continuous variable for which to create plot
#' @return data to pass to plot global profile function
#' @author matteo-V
#' @importFrom eidith ed2_human
#' @importFrom forcats fct_relevel
#' @importFrom dplyr %>% select filter
#' @importFrom rlang !! sym
#' @export
continuous_global_profile_frame <- function(var_name){
  ed2_human() %>%
    get_country_codes() %>%
    filter(country!='MP') %>% #drop peninsular malaysia
    select(country, !!sym(var_name)) %>%
    mutate(!!sym(var_name) := as.numeric(!!sym(var_name)))
}

#' Plot stacked bar chart for categorical variables
#'
#' @param dat from global_profile_frame
#' @param var_name var_name used in global profile frame
#' @author matteo-V
#' @importFrom rlang !! sym
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom forcats fct_relevel
#' @import ggplot2
#' @export
plot_categorical_global_profile <- function(dat, var_name){
  if(str_detect(var_name, "education")){
  ggplot(data=dat, aes(x=country,
                       y=n,
                       order=!!sym(var_name))) +
    geom_bar(position = 'fill',
             stat = 'identity',
             aes(fill=fct_relevel(!!sym(var_name),
                                  'MISSING',
                                  'none',
                                  'primary school',
                                  'secondary school',
                                  'college/university/professional'
                                  ))) +
    scale_fill_discrete(name = var_name,
                        guide=guide_legend(reverse = T)) +
    theme(plot.title = element_text(hjust=0.5)) +
    xlab('Partner Country') +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_y_continuous(labels = scales::percent)
}
  else if(str_detect(var_name, 'length_lived')){
    ggplot(data=dat, aes(x=country,
                         y=n,
                         order=!!sym(var_name))) +
      geom_bar(position = 'fill',
               stat = 'identity',
               aes(fill=fct_relevel(!!sym(var_name),
                                    '<1 month',
                                    '1 month - 1 year',
                                    '>1 - 5 years',
                                    '>5 - 10 years',
                                    '>10 years'
                                    ))) +
      ggtitle('Global Profile: Respondent Education') +
      scale_fill_discrete(guide=guide_legend(reverse = T)) +
      theme(plot.title = element_text(hjust=0.5)) +
      xlab('Partner Country') +
      coord_flip() +
      theme_fivethirtyeight() +
      scale_y_continuous(labels = scales::percent)
  }
  else if(str_detect(var_name, 'gender')){
    ggplot(data=dat, aes(x=country,
                         y=n,
                         order=!!sym(var_name))) +
      geom_bar(position = 'fill',
               stat = 'identity',
               aes(fill=fct_relevel(!!sym(var_name),
                                    'male',
                                    'other',
                                    'female'
               ))) +
      ggtitle('Global Profile: Respondent Education') +
      scale_fill_discrete(guide=guide_legend(reverse = T)) +
      theme(plot.title = element_text(hjust=0.5)) +
      xlab('Partner Country') +
      coord_flip() +
      theme_fivethirtyeight() +
      scale_y_continuous(labels = scales::percent)
  }
  else{
    ggplot(data=dat, aes(x=country,
                         y=n,
                         fill=!!sym(var_name),
                         order=!!sym(var_name))) +
      geom_bar(position = 'fill', stat = 'identity') +
      ggtitle('Global Profile: Respondent Education') +
      scale_fill_discrete(guide=guide_legend(reverse = T)) +
      theme(plot.title = element_text(hjust=0.5)) +
      xlab('Partner Country') +
      coord_flip() +
      theme_fivethirtyeight() +
      scale_y_continuous(labels = scales::percent)
  }


}

#' Plot box and whisker for continuous variables
#'
#' @param dat from continuous_global_profile
#' @param var_name var used in continuous global profile
#' @return ggplot2 object
#' @author matteo-V
#' @import ggplot2
#' @importFrom rlang !! sym
#' @importFrom ggthemes theme_fivethiryeight
#' @export
plot_continuous_global_profile <- function(dat, var_name){
  ggplot(data=dat, aes(x=country)) +
    geom_boxplot(aes(y = !!sym(var_name),
                     fill = country)) +
    scale_fill_discrete( name='Country') +
    theme(plot.title = element_text(hjust=0.5)) +
    xlab('Partner Country') +
    ylab('Number of Respondents') +
    coord_flip() +
    theme_fivethirtyeight()
}


#' Plot categorical global aggregate data
#'
#' @param dat from categorical_global_aggregate
#' @param var_name from categorigal aggregate frame
#' @return ggplot2 object
#' @author matteo-V
#' @import ggplot2
#' @imprtFrom ggtheme theme_fivethirtyeight
#' @importFrom rlang !! sym
#' @export
plot_categorical_global_aggregate <- function(dat, var_name){
  if(str_detect(var_name, 'education')){
    ggplot(data=dat,
           aes(fill=fct_relevel(!!sym(var_name),
                                'none',
                                'primary school',
                                'secondary school',
                                'college/university/professional'),

                         ymin=ymin,
                         ymax=ymax,
                         xmax=4,
                         xmin=3)) +
      geom_rect(color='black') +
      scale_fill_discrete(name='') +
      xlim(c(0,4)) +
      coord_polar(theta = 'y') +
      theme_bw()+
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(plot.background = element_rect(colour = 'white'))

  }
  else if(str_detect(var_name, 'length_lived')){
    ggplot(data=dat,
           aes(fill=fct_relevel(!!sym(var_name),
                                '<1 month',
                                '1 month - 1 year',
                                '>1 - 5 years',
                                '>5 - 10 years',
                                '>10 years'),

               ymin=ymin,
               ymax=ymax,
               xmax=4,
               xmin=3)) +
      geom_rect(color='black') +
      scale_fill_discrete(name='') +
      xlim(c(0,4)) +
      coord_polar(theta = 'y') +
      theme_bw() +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(plot.background = element_rect(colour = 'white'))
  }
  else{
    ggplot(data=dat,
           aes(fill=!!sym(var_name),

               ymin=ymin,
               ymax=ymax,
               xmax=4,
               xmin=3)) +
      geom_rect(color='black') +
      scale_fill_discrete(name='') +
      xlim(c(0,4)) +
      coord_polar(theta = 'y') +
      theme_bw() +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(plot.background = element_rect(colour = 'white'))
  }
}
