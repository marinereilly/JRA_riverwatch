####Install Packages #########
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(janitor)
library('colourpicker')
library('purrr')
library(naniar)

#######Load Data ##########
df<-readxl::read_xlsx("data/HAIRWORKINGcopy1.xlsx", 
                       sheet = "Edited Data",
                      col_types = c("text","date","numeric","text",
                                    "numeric","text","numeric", "text",
                                    "numeric","text","numeric","numeric",
                                    "numeric","numeric","numeric","text",
                                    "text","numeric","numeric","numeric",
                                    "numeric","numeric","text","text",
                                    "logical", "numeric","text","text",
                                    "numeric","text","text","text","text",
                                    'text', 'date', 'text')) %>% 
  clean_names()

#######Clean Data ##########

df1<- df%>%
  distinct()
df1 <- df1 %>%
  replace_with_na(replace = list(bacteria_threshold_p_1716 = -9,
                                 duplicate_bacteria_concentration_f_751=-9,
                                 turbidity_p_710 = -9,
                                 water_temperature_p_708 = -9,
                                 collection_date = -9
                                 ))
summary(df)
#Fix temperatures

good_temp<-df1 %>% 
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
unique(df1$year)


#### Date Time! ########
df2 <- good_temp
df2$collection_date <- ymd_hms(df2$collection_date)
df2$month <- month(df2$collection_date)
df2$year <- year(df2$collection_date)
df2$year <- as.factor(df2$year)
df2$month <- as.factor(df2$month)


###Misc. editing and cleaning 
df2<-df2 %>%
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

df2 <-df2 %>%
  mutate(ecoli_grade = case_when(e_coli_count >= 235 ~ 'F',
                                 e_coli_count< 235 ~ 'P'))
#Create Pass/Fail for E. Enterococcus
df2 <- df2 %>%
  mutate(enterococcus_grade=
           case_when(enterococcus_bacteria_concentration_p_1690 >= 104 ~ 'F',
                     enterococcus_bacteria_concentration_p_1690 < 104 ~ 'P'))


theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 8, angle = 45, 
                                            hjust = 1, vjust = 1),
                  plot.title = element_text(hjust = .5) )
          )



df2$ecoli_grade <- as.factor(df2$ecoli_grade)
df2$station_id <- as.factor(df2$station_id)

#substitute pass/fails for 1 and 0
df3 <- df2 %>%
  mutate(ecoli_grade1 = 
           case_when(ecoli_grade %in% c('P') ~ (1),
                     ecoli_grade %in% c('F') ~ (0))) %>%
  mutate(entero_grade1 = 
           case_when(enterococcus_grade%in% c('P') ~ (1),
                     enterococcus_grade %in% c('F') ~ (0)))

df3$temp_tot = df3$wattemp_units + df3$airtemp_units

df3 <- df3 %>%
  mutate(hypo_safe = 
           case_when(temp_tot <= 37.778 ~ 'F',
                     temp_tot > 37.778 ~ 'P'))

df3 <- df3 %>%
  mutate(hypo_safe1 = 
           case_when(hypo_safe %in% c('P') ~ (1),
                     hypo_safe %in% c('F') ~ (0)))

##########Fix station names  #########
library(readr)
site_names <- read_csv("data/Riverwatch SiteID, Names, Location - Sheet1.csv") %>% 
  clean_names()


df4<-site_names %>% 
  rename('station_id'='station_number')%>% 
  left_join(df3,., by=c("station_id")) %>% 
  mutate(station_description=case_when(
    station_description=="Hopewell Rt. 10"            ~ "Hopewell Rt 10",
    station_description=="Farmville Main St. Bridge"  ~ "Farmville Main St Bridge",
    station_description=="Rockett\u0092s Landing"     ~ "Rocketts Landing",
    station_description == 'Rockett<92>s Landing' ~ 'Rocketts Landing',
    station_description == 'Rockett\x92s Landing' ~ "Rocketts Landing",
    TRUE                                              ~ station_description
  ))

df4$station_description[startsWith(df4$station_description, 'Rockett')] <- 'Rocketts Landing'
df4$station_description <- as.factor(df4$station_description)
summary(df4$station_description)
write.csv(df4, file = 'data/tidied_df.csv')


######Figure TIME######
# Air Temp per Site
df4 %>%
  filter (!airtemp_units <= 0) %>%
  ggplot(aes(`station_id`, airtemp_units)) +
         geom_boxplot() + 
         labs(x = 'Station ID', y = 'Air Temp in C') +
  scale_y_continuous(breaks = seq(0,50,5))
  

ggsave('figures/air_temp_box.jpg')
        
# Water Temp per site
df4 %>%
  filter(!wattemp_units <= 0) %>%
  ggplot(aes(`station_id`, wattemp_units)) +
  geom_boxplot()+ 
  labs (x = 'Station ID', y = 'Water Temperature C') +
  scale_y_continuous(breaks = seq(0,50,5))
ggsave('figures/wat_temp_box.jpg')

# Turbidity per site
df4 %>%
  filter(!turbidity_p_710 == 1056) %>%
  filter(!turbidity_p_710 == -9) %>%
    ggplot(aes(`station_id`, turbidity_p_710)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0,250,20)) +
  labs(x = 'Station ID', y = 'Turbidity (Units)')

ggsave('figures/turb_box.jpg') 



# Conductivity per Site
df4 %>%
  filter(!conductivity_p_709==22027) %>%
  filter(!conductivity_p_709 == -9) %>%
  ggplot(aes(`station_id`, conductivity_p_709)) + 
  geom_boxplot() +
  labs(x = 'Station ID', y = 'Conductivity (Units)') + 
  scale_y_continuous(breaks = seq(0,1500,100))
ggsave('figures/cond_box.jpg') 
# What are the units for conductivity? 
#Is it possible to convert the entries in incorrect units?



######bacteria########
df1%>%
  ggplot(aes(e_coli_concentration_p_711)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200) +
  labs(x = 'E. coli Concentration', title = 'Original Data')
ggsave('figures/OG_ecoli_concentration_hist.jpg')
df1 %>%
  ggplot(aes(e_coli_count_p_712)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200) +
  labs(x = 'E. Coli Count', title = 'Original Data')
ggsave('figures/OG_ecoli_count_hist.jpg')

#What a mess! E.coli count is the higher value




#Cleaning E Coli Counts/Concentrations!

df4%>%
  ggplot(aes(e_coli_count)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200) +
  labs(x = 'E. Coli Count', title = 'Tidied Data')
ggsave('figures/good_ecoli_count_hist.jpg')
df4 %>%
  ggplot(aes(e_coli_concentration)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200)
ggsave('figures/good_ecoli_concent_hist.jpg')
# This concentration might be slightly erroneous but 
# we will just use the ecoli count



#Create Pass/Fail for E.Coli

#Figure out Enterococcus
summary(df4$enterococcus_bacteria_concentration_p_1690)
df4%>%
  ggplot(aes(enterococcus_bacteria_concentration_p_1690)) +
  geom_histogram(binwidth = .2) +
  ylim(0,200) + 
  xlim(0,200)

#Something seems wrong here?? Is there really this little data? 



#make pass/fail graphs per site
df4 %>%
  filter(!is.na(ecoli_grade)) %>%
  filter(!is.na(station_name)) %>%
  ggplot(aes( x =station_name, fill = ecoli_grade)) +
  geom_bar(stat='count') +
  labs(x = "Site Name", y = 'Days Sampled', 
       fill = "Water Quality Grade",
       title = 'E. Coli Safety') + 
  scale_fill_manual(values = c('red3', '#188F30'),
                    name = "E. Coli Safety",
                    labels = c('Fail', 'Pass')) +
  theme(plot.margin = unit(c(.5,.5,.5,1.7), 'cm'))

ggsave('figures/ecoli_grade_count.jpg')


df4 %>%
  filter(!is.na(ecoli_grade)) %>%
  ggplot(aes( x =station_name, fill = ecoli_grade )) +
  geom_bar(stat='count',position = 'fill') +
  labs(x = "Site Name", y = 'Proportion of Days Sampled', 
       fill = "Water Quality Grade",
       title = 'E. Coli Safety') + 
  scale_fill_manual(values = c('red3', '#188F30'),
                    name = "E. Coli Safety",
                    labels = c('Fail', 'Pass')) +
  theme(plot.margin = unit(c(.5,.5,.5,1.7), 'cm'))
  
ggsave('figures/ecoli_grade_prop.jpg')

df4 %>%
  filter(!is.na(enterococcus_grade)) %>%
  ggplot(aes( x =station_id, fill = enterococcus_grade)) +
  geom_bar(stat='count') +
  labs(x = "Station ID", y = 'Count', 
       fill = "Water Quality Grade",
       title = 'Enterococcus Safety All Years') + 
  scale_fill_discrete(name = "Water Quality Grade",
                      labels = c('Fail', 'Pass'))
ggsave('figures/entero_grade_count.jpg')

df4 %>%
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
monthly_wat_temp <- df4 %>%
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
monthly_air_temp <- df4 %>%
  filter(!airtemp_units == -9) %>%
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
monthly_turb <- df4 %>%
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
yearly_wat_temp <- df4 %>%
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

#### Make data ready for multiplots
df_long<-df4 %>% 
  select(collection_date,
         station_name,
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
  mutate(plot_id=paste0(station_name,"_",parameter))

df_nest<-df_long %>% 
  group_by(plot_id) %>% 
  nest()

df_plots<-df_nest %>% 
  mutate(plots=map2(data,plot_id, ~ggplot(.x)+
                      ggtitle(.y)+
                      geom_point(aes(x=collection_date,y=value))+
                      labs(x = "Collection Date") +
                      theme_classic()))

df_plots$plots[[10]]
##I don't know how to make these more aesthetic, I think 
# this gets the point across but let me know if you want
#the graphs to be prettier



#use the map function with ggsave to save named figures. 

map2(paste0("./figures/station_plots/", df_plots$plot_id, ".jpg"), 
       df_plots$plots, ggsave)



#make summary tables
tab_entero <- df4[!is.na(df3$entero_grade1), ] %>%
  group_by(station_id) %>%
  summarise(ent_avg = mean(entero_grade1),
            n = length(station_id),
            sd = sd(entero_grade1),
            se = sd/sqrt(n))

tab_ecoli <- df4 [!is.na(df3$ecoli_grade1), ] %>%
  group_by(station_id) %>%
  summarise(eco_avg = mean(ecoli_grade1),
            n = length(ecoli_grade1),
            sd = sd(ecoli_grade1),
            se = sd/sqrt(n))
write.csv(tab_ecoli, file = 'tables/ecoli_pass_rate_sitely.csv')
write.csv(tab_entero, file = 'tables/entero_pass_rate_sitely.csv')
###### More Bacteria Safety Graphs #########

#####Overall E. Coli Safety Graph
tab_ecoli %>%
  ggplot(aes(x = station_id, y = eco_avg)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = eco_avg - se, ymax = eco_avg + se),
                width = .2,
                position=position_dodge(.9)) +
  labs(x = 'Station ID', y = 'E Coli Passing Rate',
       title = 'Overall E. Coli Safety')
ggsave('figures/overall_ecoli_safety.jpg')
# The overall passing rates of each site for E. Coli
# Closer to 1 is healthier! 

#### Yearly E. Coli Safety Table & Graph
tab_ecoli_yearly <- df4 [!is.na(df4$ecoli_grade1), ] %>%
  group_by(station_id, year) %>%
  summarise(eco_avg = mean(ecoli_grade1),
            n = length(ecoli_grade1),
            sd = sd(ecoli_grade1),
            se = sd/sqrt(n))

df_nest<-tab_ecoli_yearly %>% 
  group_by(station_id) %>% 
  nest()




######### Overall Enterococcus Safety Graph
tab_entero %>% 
  ggplot(aes(x = station_id, y = ent_avg)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = ent_avg - se, ymax = ent_avg + se),
                width = .2,
                position=position_dodge(.9)) +
  labs(x = 'Station ID', y = 'Enterococcus Passing Rate',
       title = 'Overall Enterococcus Safety')
ggsave('figures/overall_entero_safety.jpg')
  
#####Making Hypothermia limit graphs #######


df4 %>%
  filter(!is.na(hypo_safe)) %>%
  drop_na(station_name) %>%
  ggplot(aes( x =station_name, fill = hypo_safe)) +
  geom_bar(stat='count') +
  labs(x = "Site Name", y = 'Days Sampled', 
       fill = "Hypothermia Safety",
       title = 'Hypothermia Safety') + 
  scale_fill_manual(values = c('#8EC4ED', '#188F30'),
                      name = "Hypothermia Safety",
                      labels = c('Too Cold', 'Safe')) +
                      theme(plot.margin = unit(c(.5,.5,.5,1.7), 'cm'))
                      
ggsave('figures/hypo_grade_all.jpg')

df4 %>%
  filter(!is.na(hypo_safe)) %>%
  drop_na(month) %>%
  ggplot(aes( x =month, fill = hypo_safe)) +
  geom_bar(stat='count', position = 'fill') +
  labs(x = "Station Name", y = 'Days Sampled', 
       fill = "Hypothermia Safety",
       title = 'Hypothermia Safe Days Across All Years') + 
  scale_fill_manual(values = c('#8EC4ED', '#188F30'),
                    name = "Hypothermia Safety",
                    labels = c('Too Cold', 'Safe')
  )


df4 %>%
  filter(!is.na(hypo_safe)) %>%
  ggplot(aes( x =station_id, fill = hypo_safe)) +
  geom_bar(stat='count') +
  labs(x = "Station ID", y = 'Count', 
       fill = "Hypothermia Safety",
       title = 'Hypothermia Safety All Years') + 
  scale_fill_discrete(name = "Hypothermia Safety",
                      labels = c('Fail', 'Pass'))



# Summarize passing percent by station
tab_hypo <- df4[!is.na(df3$hypo_safe1), ] %>%
  group_by(station_id) %>%
  summarise(hypo_avg = mean(hypo_safe1),
            n = length(station_id),
            sd = sd(hypo_safe1),
            se = sd/sqrt(n))

tab_hypo %>%
  ggplot(aes(x = station_id, y = hypo_avg)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = hypo_avg - se, ymax = hypo_avg + se),
                width = .2,
                position=position_dodge(.9)) +
  labs(x = 'Station ID', y = 'Hypothermia Risk Passing Rate',
       title = 'Overall Hypothermia Safety')
ggsave('figures/overall_hypo_safety.jpg')
# closer to 1 is a passing rate, so more safe! 

tab_hypo_yearly <- df4[!is.na(df4$hypo_safe1), ] %>%
  group_by(year) %>%
  summarise(hypo_avg = mean(hypo_safe1),
            n = length(station_id),
            sd = sd(hypo_safe1),
            se = sd/sqrt(n))
tab_hypo_yearly %>%
  ggplot(aes(x = year, y = hypo_avg)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = hypo_avg - se, ymax = hypo_avg + se),
                width = .2,
                position=position_dodge(.9)) +
  labs(x = 'Year', y = 'Hypothermia Risk Passing Rate',
       title = 'Yearly Hypothermia Safety')
ggsave('figures/overall_hypo_safety.jpg')

tab_hypo_yearly_sitely <- df4[!is.na(df4$hypo_safe1), ] %>%
  group_by(station_id, year) %>%
  summarise(hypo_avg = mean(hypo_safe1),
            n = length(station_id),
            sd = sd(hypo_safe1),
            se = sd/sqrt(n))

 #can we nest this table by site and mapping/ create a 
# graph for each site with yearly hypo safety?

df4 %>%
  ggplot(aes(x = collection_date, y = temp_tot, color = station_id)) +
  geom_point(size = 2) + 
  geom_hline(yintercept = 37.778, col= 'red') +
  labs( x= 'Date', y = "Total Temperature (C)", 
        title = 'Hypothermia Safety')
ggsave('figures/hypo_safety_point.jpg')  


