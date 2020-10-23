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

#### Date Time! ########
df1 <- good_temp
df1$collection_date <- ymd_hms(df1$collection_date)
df1$month <- month(df1$collection_date)
df1$year <- year(df1$collection_date)
df1$year <- as.factor(df1$year)
df1$month <- as.factor(df1$month)
summary(df1$wattemp_units)
summary(df1)

######### Box Plots ###########
theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 8, angle = 90, 
                                            hjust = 1, vjust = 1),
                  plot.title = element_text(hjust = .5) )
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



# Conductivity per Site
good_temp %>%
  filter(!conductivity_p_709==22027) %>%
  ggplot(aes(`station_id`, conductivity_p_709)) + 
  geom_boxplot() +
  labs(x = 'Station ID', y = 'Conductivity (Units)') + 
  scale_y_continuous(breaks = seq(0,1500,100))
ggsave('figures/cond_box.jpg') 
# What are the units for conductivity? 
#Is it possible to convert the entries in incorrect units?

summary(df)

######bacteria########
df%>%
  ggplot(aes(e_coli_concentration_p_711)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200) +
  labs(x = 'E. coli Concentration', title = 'Original Data')
ggsave('figures/OG_ecoli_count_hist.jpg')
df %>%
  ggplot(aes(e_coli_count_p_712)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200) +
  labs(x = 'E. coli Count', title = 'Original Data')
ggsave('figures/OG_ecoli_concent_hist.jpg')
#What a mess! E.coli count is the higher value

count(filter(df, e_coli_concentration_p_711 == 100))
count(filter(df, e_coli_count_p_712 == 100))



#Cleaning E Coli Counts/Concentrations!
df2<-df1 %>%
  mutate(e_coli_count = 
           case_when(
             e_coli_concentration_p_711 > e_coli_count_p_712 ~ e_coli_count_p_712 * 33.33,
             e_coli_concentration_p_711 < e_coli_count_p_712 ~ e_coli_count_p_712
           ))


df2<- df2 %>%
  mutate(e_coli_concentration = 
           case_when(
             e_coli_concentration_p_711 > e_coli_count_p_712 ~ e_coli_count_p_712/3,
             e_coli_concentration_p_711 < e_coli_count_p_712 ~ e_coli_concentration_p_711
           ))


count(filter(df2, e_coli_concentration == 34))
count(filter(df2, e_coli_count == 34))

df2%>%
  ggplot(aes(e_coli_count)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200)
ggsave('figures/good_ecoli_count_hist.jpg')
df2 %>%
  ggplot(aes(e_coli_concentration)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200)
ggsave('figures/OG_ecoli_concent_hist.jpg')
# This concentration might be slightly erroneous but 
# we will just use the ecoli count

summary(df2$e_coli_concentration)

#Create Pass/Fail for E.Coli
df2 <-df2 %>%
  mutate(ecoli_grade = case_when(e_coli_count >= 235 ~ 'F',
                                 e_coli_count< 235 ~ 'P'))
#Figure out Enterococcus
summary(df2$enterococcus_bacteria_concentration_p_1690)
df2%>%
  ggplot(aes(enterococcus_bacteria_concentration_p_1690)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200)

#Something seems wrong here?? Is there really this little data? 

#Create Pass/Fail for E. Enterococcus
df2 <- df2 %>%
  mutate(enterococcus_grade=
           case_when(enterococcus_bacteria_concentration_p_1690 >= 104 ~ 'F',
                     enterococcus_bacteria_concentration_p_1690 < 104 ~ 'P'))

#make pass/fail graphs per site
#need to make these graphs more aesthetic
df2 %>%
  filter(!is.na(ecoli_grade)) %>%
  ggplot(aes( x =station_id, fill = ecoli_grade)) +
  geom_bar(stat='count') +
  labs(x = "Station ID", y = 'Count', 
       fill = "Water Quality Grade",
       title = 'E. Coli Safety All Years') + 
  scale_fill_discrete(name = "Water Quality Grade",
                      labels = c('Fail', 'Pass')) +
  
ggsave('figures/ecoli_grade_count.jpg')
df2 %>%
  filter(!is.na(ecoli_grade)) %>%
  ggplot(aes( x =station_id, fill = ecoli_grade, )) +
  geom_bar(stat='count',position = 'fill') +
  labs(x = "Station ID", y = 'Proportion', 
       fill = "Water Quality Grade",
       title = 'E. Coli Safety All Years') + 
  scale_fill_discrete(name = "Water Quality Grade",
                      labels = c('Fail', 'Pass'))
ggsave('figures/ecoli_grade_prop.jpg')
df2 %>%
  filter(!is.na(enterococcus_grade)) %>%
  ggplot(aes( x =station_id, fill = enterococcus_grade)) +
  geom_bar(stat='count') +
  labs(x = "Station ID", y = 'Count', 
       fill = "Water Quality Grade",
       title = 'Enterococcus Safety All Years') + 
  scale_fill_discrete(name = "Water Quality Grade",
                      labels = c('Fail', 'Pass'))
ggsave('figures/entero_grade_count.jpg')

df2 %>%
  filter(!is.na(enterococcus_grade)) %>%
  ggplot(aes( x =station_id, fill = enterococcus_grade)) +
  geom_bar(stat='count', position = 'fill') +
  labs(x = "Station ID", y = 'Proportion', 
       fill = "Water Quality Grade",
       title = 'Enterococcus Safety All Years') + 
  scale_fill_discrete(name = "Water Quality Grade",
                      labels = c('Fail', 'Pass'))
ggsave('figures/entero_grade_prop.jpg')


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

#######Summary Tables ########

#####Making Hypothermia limit graphs #######

####### Making Bacteria safety graphs#######

##### Daily Turbidity & Bacteria Graphs #####
