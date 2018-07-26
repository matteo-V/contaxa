library(tidyverse)
library(ggthemes) #pretty themes
library(viridis) #pretty colors, backed by science
library(ggvis) #interactive viz plots

#load in full viz data set
vizdat <- readRDS(file='data/BD_viz_dat.rds')

#remove non-contact variables names
varnames <- colnames(vizdat)[-(1:5)] #remove first 5 columns for

#calculate totals for conversion of counts to percents
num_for_percent <- function(index, viz_frame, varnm){
  num <- nrow(viz_frame[viz_frame[[varnm]] == index,]) #calculate numer of respondents in row
  return ( as.vector(num) )
}

#number to percentage
num_to_percent <- function(index, varnm, full_viz_frame, target_viz_frame){
    res <-
    target_viz_frame[target_viz_frame[[varnm]] == index, ][-(1)] /
    num_for_percent(index, full_viz_frame, varnm) #call num for percent above
    res
}

#define num of quintiles
ntiles <- 5
#create age N-tiles (start with quintiles)
vizdat <- vizdat %>%
  mutate(age_quint = ntile(age, 5))

#group data by age and sum
age_viz_dat <- vizdat %>%
  group_by(age_quint) %>% #group by age
  summarize_if(is.logical, sum, na.rm=F) %>% #sum all logical cols (contacts)
  #convert to percentage for plotting
  mutate_at( .vars = varnames,
             .funs = funs( #use case statement to subset and calculate row totals
               case_when(age_quint == 1 ~ . / num_for_percent(1, vizdat, 'age_quint'),
                         age_quint == 2 ~ . / num_for_percent(2, vizdat, 'age_quint'),
                         age_quint == 3 ~ . / num_for_percent(3, vizdat, 'age_quint'),
                         age_quint == 4 ~ . / num_for_percent(4, vizdat, 'age_quint'),
                         age_quint == 5 ~ . / num_for_percent(5, vizdat, 'age_quint'))
             )
            )

#group data by gender and sum
gender_viz_dat <- vizdat %>%
  group_by(gender) %>% #grouping variable
  summarize_if(is.logical, sum, na.rm=F) %>%   #sum all logical cols (contacts)
  #convert to percentages for visualization
  mutate_at(.vars = varnames, #mutate at contact variables only
            .funs = funs( #use custom variables
              ifelse(gender=='male', #if index is male
                     . / num_for_percent('male', vizdat, 'gender'), #div by number of males
                     . / num_for_percent('female', vizdat, 'gender') #else divide by num females
              )
            )
  )


#TODO: combine these scripts into function for easy wrapping with other countries
#TODO: Wrap this up into a function for easy use

#colnames for reshaping
age_keycol <- "age_quint" #key column for age dat
gen_keycol <- "gender" #key column for gender dat
gathercols <- colnames(age_viz_dat)[-(1)] #remove first colname for age quintiles
valuecol <- "num_contacts" #new colname for values

#reshape data to long format for plotting as heatmap
age_viz_dat_long <- gather(age_viz_dat, age_keycol, valuecol, gathercols )
gender_viz_dat_long <-  gather(gender_viz_dat, gen_keycol, valuecol, gathercols)



#age matrix
age_heatmap <- ggplot(age_viz_dat_long, aes(x=age_quint, y=age_keycol)) +
  geom_tile(aes(fill=valuecol), color='gray') + #heapmap fill, color to add lines between cells
  scale_fill_viridis(name='Percentage of Respondents') + #pretty color scale
  theme_fivethirtyeight() + #pretty theme
  ggtitle(label = "Taxa Contacts by Age Quintile") + #plot title
  theme(plot.title = element_text(hjust=0.5),
        legend.text = element_text(angle = 45)) #adjust plot title position

age_heatmap

#gender matrix
gender_heatmap <- ggplot(gender_viz_dat_long, aes(x=gender, y=gen_keycol)) +
  geom_tile(aes(fill=valuecol), color='gray') + #heapmap fill, color to add lines between cells
  scale_fill_viridis(name = 'Percentage of Respondents') +
  theme_fivethirtyeight() +
  ggtitle(label= 'Taxa Contacts by Gender') +
  theme(plot.title = element_text(hjust=0.5),
        legend.text = element_text(angle=45))

gender_heatmap

#TODO: create static picture by concurrent sites

#TODO: create interactive viz using categories for visualizing data
