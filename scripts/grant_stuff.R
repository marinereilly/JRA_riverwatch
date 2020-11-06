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

######### Load Data #######
df<-read.csv("data/tidied_df.csv")
summary(df)


#####Clean Data ###########

summary(df)



#Add order of Upstream-Downstream


###### Summary Tables #########
# bacteria summary - overall by site + 2020 summary

tab_ecoli <- df [!is.na(df$ecoli_grade1), ] %>%
  dplyr::group_by(Station.Description) %>%
  dplyr::summarise(eco_avg = mean(ecoli_grade1),
            n = length(ecoli_grade1),
            sd = sd(ecoli_grade1),
            se = sd/sqrt(n))
ecoli_2020 <- df [!is.na(df$ecoli_grade1), ]%>%
  filter(year == 2020)%>%
  dplyr:: group_by(Station.Description) %>%
  dplyr:: summarise(eco_avg = mean(ecoli_grade1),
                    n = length(ecoli_grade1),
                    sd = sd(ecoli_grade1),
                    se = sd/sqrt(n))

overall_ecoli <- merge(x=tab_ecoli, y = ecoli_2020, 
                       by.x = 'Station.Description', by.y='Station.Description',
                       all = TRUE)
overall_ecoli <- overall_ecoli %>%
  dplyr::rename(ecoli_avg_total=eco_avg.x,
          n_total=n.x,
          sd_total= sd.x,
          se_total = se.x,
          ecoli_avg_2020 = eco_avg.y,
          n_2020 = n.y,
          sd_2020 = sd.y,
          se_2020 = se.y)

summary(overall_ecoli)
write.csv(overall_ecoli, file = 'tables/ecoli_rates_sitely.csv')
#same thing for ecoli
tab_entero <- df[!is.na(df$entero_grade1), ] %>%
  dplyr::group_by(Station.Description) %>%
  dplyr::summarise(ent_avg = mean(entero_grade1),
                   n = length(Station.Description),
                   sd = sd(entero_grade1),
                   se = sd/sqrt(n))
entero_2020 <- df [!is.na(df$entero_grade1), ]%>%
  filter(year == 2020)%>%
  dplyr:: group_by(Station.Description) %>%
  dplyr:: summarise(entero_avg = mean(entero_grade1),
                    n = length(entero_grade1),
                    sd = sd(entero_grade1),
                    se = sd/sqrt(n))
overall_entero <- merge(x=tab_entero, y = entero_2020, 
                       by.x = 'Station.Description', by.y='Station.Description',
                       all = TRUE)
overall_entero <- overall_entero %>%
  dplyr::rename(entero_avg_total=ent_avg,
                n_total=n.x,
                sd_total= sd.x,
                se_total = se.x,
                entero_avg_2020 = entero_avg,
                n_2020 = n.y,
                sd_2020 = sd.y,
                se_2020 = se.y)
write.csv (overall_entero, file = 'tables/entero_rates_sitely.csv')


#Yearly bacteria pass rates (overall)
df$year <- as.factor(df$year)
ecoli_yearly <- df [!is.na(df$ecoli_grade1), ]%>%
  dplyr:: group_by(year) %>%
  dplyr:: summarise(ecoli_avg = mean(ecoli_grade1),
                    n = length(ecoli_grade1),
                    sd = sd(ecoli_grade1),
                    se = sd/sqrt(n))
write.csv(ecoli_yearly, file = 'tables/ecoli_rates_yearly.csv')

entero_yearly <- df [!is.na(df$entero_grade1), ]%>%
  dplyr:: group_by(year) %>%
  dplyr:: summarise(entero_avg = mean(entero_grade1),
                    n = length(entero_grade1),
                    sd = sd(entero_grade1),
                    se = sd/sqrt(n))
write.csv(entero_yearly, file = 'tables/entero_rates_yearly.csv')

#Yearly bacteria rates per site 
ecoli_yearly_sitely <- df [!is.na(df$ecoli_grade1), ]%>%
  dplyr:: group_by(year, Station.Description) %>%
  dplyr:: summarise(ecoli_avg = mean(ecoli_grade1),
                    n = length(ecoli_grade1),
                    sd = sd(ecoli_grade1),
                    se = sd/sqrt(n))
write.csv(ecoli_yearly_sitely, file = 'tables/ecoli_rates_yearly&sitely.csv')

entero_yearly_sitely <- df [!is.na(df$entero_grade1), ]%>%
  dplyr:: group_by(year, Station.Description) %>%
  dplyr:: summarise(entero_avg = mean(entero_grade1),
                    n = length(entero_grade1),
                    sd = sd(entero_grade1),
                    se = sd/sqrt(n))
write.csv(entero_yearly_sitely, file = 'tables/entero_rates_yearly&sitely.csv')


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

