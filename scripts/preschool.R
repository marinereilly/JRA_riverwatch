####Install Packages #########
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(janitor)

#######Load Data ##########
df<-readxl::read_xlsx("~/Desktop/JRA/HAIRWORKINGcopy.xlsx",
                      col_types = c("text","date","numeric","text",
                                    "numeric","text","numeric", "text",
                                    "numeric","text","numeric","numeric",
                                    "numeric","numeric","numeric","text",
                                    "text","numeric","numeric","numeric",
                                    "numeric","numeric","text","text",
                                    "logical", "numeric","text","text",
                                    "numeric","text","text","text","text")) %>% 
  clean_names()

#######Clean Data ##########
#Fix temperatures
good_temp<-df %>% 
  mutate(airtemp_units=
           case_when(
             air_temperature_p_707>=50  ~ (air_temperature_p_707-32)*5/9,
             air_temperature_p_707<50   ~ air_temperature_p_707
           ))
good_temp<-good_temp %>% 
  mutate(wattemp_units=
           case_when(
             water_temperature_p_708>=50  ~ ( water_temperature_p_708-32)*5/9,
             water_temperature_p_708<50   ~  water_temperature_p_708
           ))

######Check out data ########
summary(good_temp)


#Prep data for Air Temp Boxplot
airt<-filter(good_temp, station_id %in% c('A01', 'A02', 'A03', 'A08','C01', 'DC01', 'H01', 'H02',
                                  'J02', 'J03', 'J04', 'J05', 'J08', 'J09', 'J10', 'J15',
                                  'J15', 'J20', 'J21', 'J22', 'J23', 'J24', 'J25', 'J26',
                                  'J29', 'J30', 'J35', 'J40', 'M05', 'P05', 'R10', 'R20',
                                  'R23'))

summary(good_temp)
# Prep data for Water Temp Boxplot
watt<-filter(good_temp, station_id %in% c('A01', 'A02', 'A03', 'A08','C01', 'DC01', 'H01', 'H02',
                                   'J02', 'J03', 'J04', 'J05', 'J08', 'J09', 'J10', 'J15',
                                   'J15', 'J20', 'J21', 'J22', 'J23', 'J24', 'J25', 'J26',
                                   'J29', 'J30', 'J35', 'J40', 'M05', 'P05', 'R10', 'R20',
                                   'R23'))



######### Box Plots ###########
theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 8, angle = 90, 
                                            hjust = 1, vjust = 1),
                  axis.text.y = )
          )

# Air Temp per Site
airt %>%
  filter (!airtemp_units <= 0) %>%
  ggplot(aes(`station_id`, airtemp_units)) +
         geom_boxplot() + 
         labs(x = 'Station ID', y = 'Air Temp in C') +
  scale_y_continuous(breaks = seq(0,50,5))
  

ggsave('figures/air_temp_box.jpg')
        
# Water Temp per site
watt %>%
  filter(!wattemp_units <= 0) %>%
  ggplot(aes(`station_id`, wattemp_units)) +
  geom_boxplot()+ 
  labs (x = 'Station ID', y = 'Water Temperature C') +
  scale_y_continuous(breaks = seq(0,50,5))
ggsave('figures/wat_temp_box.jpg')

# Turbidity per site
good_temp %>%
  filter(!turbidity_p_710 == 1056) %>%
  filter(!turbidity_p_710 == -9) %>%
    ggplot(aes(`station_id`, turbidity_p_710)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0,250,20)) +
  labs(x = 'Station ID', y = 'Turbidity (Units)')

ggsave('figures/turb_box.jpg') 
# Can we recalculate these incorrect turbidity measurements?


# Conductivity per Site
good_temp %>%
  filter(!conductivity_p_709==22027) %>%
  ggplot(aes(`station_id`, conductivity_p_709)) + 
  geom_boxplot() +
  labs(x = 'Station ID', y = 'Conductivity (Units)') + 
  scale_y_continuous(breaks = seq(0,1500,100))
ggsave('figures/cond_box.jpg') 
# What are the units for conductivity? 
#What is the max conductivity that makes sense? Is it possible to convert the entries that are clearly false?
# Also why does removing the outlier remove several thousand entries?? 

#### Date Time! ########
df1 <- good_temp
df1$collection_date <- ymd_hms(df1$collection_date)
df1$month <- month(df1$collection_date)
df1$year <- year(df1$collection_date)
df1$year <- as.factor(df1$year)
df1$month <- as.factor(df1$month)
summary(df1$wattemp_units)
summary(df1)
#####Monthly #########
#summarise water temp by month
monthly_wat_temp <- df1 %>%
  filter(wattemp_units > 0) %>%
  group_by(month, station_id) %>%
  summarise(mean_wat = mean(wattemp_units, na.rm = TRUE),
            sd = sd(wattemp_units, na.rm = TRUE))

 # Bar graphs of monthly temperature across sites
monthly_wat_temp %>%
  ggplot(aes(x = month, y = mean_wat)) + 
  geom_bar(stat = 'identity', fill = 'blue') + 
  geom_errorbar(aes(ymin = mean_wat - sd, ymax = mean_wat+sd)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = 'Month', y = 'Water Temp C') + 
  facet_wrap(~`station_id`)
ggsave('figures/monthly_wat_temp.jpg')

#Summarize air temp by month
monthly_air_temp <- df1 %>%
  group_by(month, station_id) %>%
  summarise(mean_air = mean(airtemp_units, na.rm = TRUE),
            sd = sd(airtemp_units, na.rm = TRUE))

#Bar graph of monthly air temps across sites
monthly_air_temp %>%
  filter(!station_id == 'VDH-AP') %>%
  filter(!station_id == 'VDH-HB') %>%
  filter(!station_id == 'VDH-HTP') %>%
  filter(!station_id == 'VDH-KL') %>%
  ggplot(aes(x = month, y = mean_air)) + 
  geom_bar(stat = 'identity', fill = 'green4') + 
  geom_errorbar(aes(ymin = mean_air - sd, ymax = mean_air+sd)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = 'Month', y = 'Air Temp C') + 
  facet_wrap(~`station_id`)
ggsave('figures/monthly_air_temp.jpg')

#summarise turbidity by month
monthly_turb <- df1 %>%
  filter(!turbidity_p_710 < 0) %>%
  group_by(month, station_id) %>%
  summarise(mean_turb = mean(turbidity_p_710, na.rm = TRUE),
            sd = sd(turbidity_p_710, na.rm = TRUE))

monthly_turb %>%
  ggplot(aes(x = month, y = mean_turb)) + 
  geom_bar(stat = 'identity', fill = 'green4') + 
  geom_errorbar(aes(ymin = mean_turb - sd, ymax = mean_turb +sd)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylim(0,200) +
  labs(x = 'Month', y = 'Turbidity') + 
  facet_wrap(~`station_id`)
ggsave('figures/monthly_turb.jpg')

## summarise water temp by month
yearly_wat_temp <- df1 %>%
  filter(wattemp_units > 0) %>%
  group_by(year, station_id) %>%
  summarise(mean_wat = mean(wattemp_units, na.rm = TRUE),
            sd = sd(wattemp_units, na.rm = TRUE))

yearly_wat_temp %>%
  filter(!station_id == 'VDH-AP') %>%
  filter(!station_id == 'VDH-HB') %>%
  filter(!station_id == 'VDH-HTP') %>%
  filter(!station_id == 'VDH-KL') %>%
  ggplot(aes(x = year, y = mean_wat)) + 
  geom_bar(stat = 'identity', fill = 'blue') + 
  geom_errorbar(aes(ymin = mean_wat - sd, ymax = mean_wat+sd)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = 'Year', y = 'Water Temp C') + 
  facet_wrap(~`station_id`)
ggsave('figures/yearly_air_temp.jpg')

#####Making Hypothermia limit graphs #######

#######Summary Tables ########

####### Making Bacteria safety graphs#######

##### Daily Turbidity & Bacteria Graphs #####
