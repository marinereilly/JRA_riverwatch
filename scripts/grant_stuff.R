## Madelyn Hair
## November 2020

## script to make graphs and tables with the important
## grant information 

####### Install Packages #########
library("Rmisc")
library(tidyverse)
library(lubridate)
library(janitor)
library('colourpicker')
library('purrr')



######### Load Data #######
df<-read.csv("data/tidied_df.csv")
summary(df)


#####Clean Data ###########





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
reasonable_values <-df %>%
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

######## Making Graphs ###############

# Set Theme 
theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 8, angle = 90, 
                                             hjust = 1, vjust = 1),
                  plot.title = element_text(hjust = .5) )
)
# doing stuff 
df$day <- day(df$collection_date)
df$fake_date <- paste('1996', df$month, df$day)
summary(df$fake_date)
summary(df)
df1 <- df
df1$fake_date <- ymd(df$fake_date)
summary(df1)
df1$year <- as.factor(df1$year)

df1%>%
  ggplot(aes(x = fake_date, y = e_coli_count, colour = year)) +
  geom_point(size = 2)

#### Making multiplots for parameter and date
#make data long for mapping
df_long<-df1 %>% 
  select(fake_date,
         year,
         station_id,
         air_temp=airtemp_units,
         conductivity=conductivity_p_709,
         e_coli=e_coli_count,
         entero=enterococcus_bacteria_concentration_p_1690,
         salinity=salinity_p_1715,
         turbidity=turbidity_p_710,
         water_temp=wattemp_units) %>% 
  pivot_longer(cols=c("air_temp","conductivity","e_coli","entero","salinity",
                      "turbidity","water_temp"),
               values_to= "value",
               names_to= "parameter") %>% 
  drop_na(value)

df_long<-df_long %>% 
  mutate(plot_id=paste0(station_id,"_",parameter))

df_nest<-df_long %>% 
  group_by(plot_id) %>% 
  nest()

df_plots<-df_nest %>% 
  mutate(plots=map2(data,plot_id, ~ggplot(.x)+
                      ggtitle(.y)+
                      geom_point(aes(x=fake_date,y=value, color = year))+
                      labs(x = "Collection Date") +
                      theme_classic()))
df_plots$plots[[61]]

length(unique(df_long$plot_id))


## Save plots 
#Not all of these are saving? 
map2(paste0("./figures/conditions_by_date/", df_plots$plot_id, ".jpg"), 
     df_plots$plots, ggsave)
