## Madelyn Hair
## November 2020

## script to make graphs and tables with the important
## grant information 

####### Install Packages #########
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(janitor)
library('colourpicker')
library('purrr')
library("Rmisc")
library(readr)
library(naniar)
######### Load Data #######
df<-read.csv("data/tidied_df.csv")
summary(df)


#####Clean Data ###########
# Correct Station Names

df$`Station.Description`<-as.factor(df$`Station.Description`)
df$station_name <- as.factor(df$station_name)
summary(df)

#Make -9 NA
df1 <- df %>%
  replace_with_na(replace = list(-9))

#Add order of Upstream-Downstream


###### Summary Tables #########
#Table by site of reasonable ranges 
reasonable_values <-df1 %>%
  group_by(Station.Description) %>%
  summarise(wat_low = quantile(wattemp_units, prob = 0.05, na.rm = TRUE),
            wat_high = quantile(wattemp_units, prob = 0.95, na.rm = TRUE),
            air_low = quantile(airtemp_units, prob = 0.05, na.rm = TRUE),
            air_high = quantile(airtemp_units, prob = 0.95, na.rm = TRUE),
            ecoli_count_low = quantile(e_coli_count, prob = 0.05, na.rm = TRUE),
            ecoli_count_high = quantile(e_coli_count, prob = 0.95, na.rm = TRUE),
            entero_low = quantile(enterococcus_bacteria_concentration_p_1690, prob = 0.05, na.rm = TRUE),
            entero_high = quantile(enterococcus_bacteria_concentration_p_1690, prob = 0.95, na.rm = TRUE),
            turb_low = quantile(turbidity_p_710, prob = 0.05, na.rm = TRUE),
            turb_high = quantile(turbidity_p_710, prob = 0.95, na.rm = TRUE),
            cond_low = quantile(conductivity_p_709, prob = 0.05, na.rm = TRUE),
            cond_high = quantile(conductivity_p_709, prob = 0.95, na.rm = TRUE)
            )


write.csv(reasonable_values,'tables/reasonable_data_ranges.csv')
## WOooo! This table shows the 90% most likely values for each
#parameter at each site. 


# Bacteria Summary Tables
tab_entero <- df1[!is.na(df1$entero_grade1), ] %>%
  group_by(`Station Description`) %>%
  summarise(ent_avg = mean(entero_grade1),
            n = length(station_id),
            sd = sd(entero_grade1),
            se = sd/sqrt(n))

tab_ecoli <- df1 [!is.na(df1$ecoli_grade1), ] %>%
  group_by(`Station Description`) %>%
  summarise(eco_avg = mean(ecoli_grade1),
            n = length(ecoli_grade1),
            sd = sd(ecoli_grade1),
            se = sd/sqrt(n))
write.csv(tab_ecoli, file = 'tables/ecoli_pass_rate_sitely.csv')
write.csv(tab_entero, file = 'tables/entero_pass_rate_sitely.csv')

