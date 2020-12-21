library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

data<-readxl::read_xlsx("data/qa_data.xlsx",sheet = "edited data") %>%
  select(-`...34`,-`...35`) %>%
  janitor::clean_names()

wr_temp<-readr::read_csv("data/wr_import-template.csv")
cmc<-readr::read_csv("data/cmcBulkSamplesTemplate_JRA.csv")
cmc_temp<-slice(cmc,0)

#####Tertiary QA process #####
#####Maddie checked data against datasheets for 2019 and 2020 
#- still missing some sites datasheets
#this qa step is to remove -9 data, and check anything else.  
#I'm doing it in two parts - first just 2020, then everything else

#2020

data2<-data %>% 
  select(station_id,
         station_name,
         latitude,
         longitude,
         collection_date,
         air_temp_c=air_temperature_p_707,
         bacteria_threshold=bacteria_threshold_p_1716,
         conductivity_us=conductivity_p_709,
         duplicate_bacteria_concentration=duplicate_bacteria_concentration_f_751,
         e_coli_count=e_coli_count_p_712,
         e_coli_concentration=e_coli_concentration_p_711,
         entercoccus_concentration=enterococcus_bacteria_concentration_p_1690,
         salinity_ppt=salinity_p_1715,
         site_conditions_and_comments=site_conditions_and_comments_f_750,
         turbidity_ntu=turbidity_p_710,
         water_temp_c=water_temperature_p_708,
         meter_issue,
         validated,
         date_val,
         changes)

df2020<-data2 %>% 
  filter(collection_date> as.POSIXct("2020-01-01 00:00:00")) %>% 
  mutate(validated = replace(validated, validated == 'Mah', 'MAH')) %>% 
  filter(!station_id %in% c("DC01","VDH-HB","VDH-HTP","VDH-AP","VDH-KL",
                            "R10","H02","J04","J03","R23", "A02","R20","J09"))

table(df2020$validated)
table(df2020$station_id)

nmes<-df2020 %>% select(station_id,station_name) %>% 
  distinct()

unqa<-df2020 %>% 
  filter(is.na(validated))
unique(unqa$collection_date)
table(unqa$station_name)

a<-table(df2020$changes)



#pull osborne landing for Stacy
stacy<-data2 %>% 
  filter(station_id=="J09") %>% 
  select(-entercoccus_concentration,-salinity_ppt,-e_coli_count)
write.csv(stacy,"data/subset_for_stacey.csv")
rm(stacy)
#####convert for CMC upload####
