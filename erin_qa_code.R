library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

data<-readxl::read_excel("data/qa_data.xlsx",sheet = "edited data", 
                         guess_max = 3500) %>%
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
                            "R10","H02","J04","J03","R23", "A02","R20",
                            "J09","H01"))

table(df2020$validated)
table(df2020$station_id)

nmes<-df2020 %>% select(station_id,station_name) %>% 
  distinct()

unqa<-df2020 %>% 
  filter(is.na(validated))
unique(unqa$collection_date)
table(unqa$station_name)

a<-table(df2020$changes)
a

#basic plots
theme_set(theme_bw())
a<-df2020 %>% 
  ggplot(aes(x=collection_date,y=water_temp_c))+
  geom_point()+
  geom_point(data = filter(df2020, station_id=="A01"),color="red",size=3)
a


w<-df2020 %>% 
  filter(water_temp_c >40)
w
w$changes

#prepping data for CMC
colnames(cmc)
[1] "Source"        "Station"       "Date"          "Time"          "SampleDepth"  
[6] "SampleId"      "ParameterType" "ParameterName" "Value"         "Qualifier"    
[11] "Problem"       "Description"  

cmc_2020<-df2020 %>% 
  mutate(Station=paste0("JRA.",station_id), #Create CMC Station Name
         Date=date(collection_date),        #split datetime into 2 columns
         Time=format(collection_date,"%I:%M %p")) %>% 
  select(Station,Date,Time,                 #get rid of extra columns
         air_temp_c,conductivity_us,duplicate_bacteria_concentration,
         e_coli_concentration,entercoccus_concentration,salinity_ppt,
         turbidity_ntu,water_temp_c,
         COMMENTS=site_conditions_and_comments,Problem=meter_issue) %>% 
  mutate_if(is.numeric,as.character) %>%   
  pivot_longer(cols=c(4:12), 
               names_to="parameter",
               values_to="Value") %>%      #flip from wide to long
  mutate(Source="JRA",                     #Add extra columns
         SampleDepth=0.3,
         Qualifier= NA_real_) %>% 
  drop_na(Value) %>%                       #get rid of rows with no data in Value
  mutate(ParameterType=case_when(
    parameter=="COMMENTS"   ~ "Comments",
    TRUE                    ~ "WaterQuality"),
    SampleId=case_when(
      parameter=="duplicate_bacteria_concentration"   ~ 2,
      parameter=="COMMENTS"                           ~ NA_real_,
      TRUE                                            ~ 1
    )) %>%                                #create Parameter Type and add Duplicate values
  #Convert duplicate parameter to regular bacteria
  select(Source,Station,Date,Time,SampleDepth, SampleId, ParameterType,
         parameter,Value,Qualifier,Problem) %>% 
  mutate(parameter=case_when(
    parameter=="duplicate_bacteria_concentration" & 
      Station %in% c("JRA.J05",
                     "JRA.P05","JRA.C01")        ~ "entercoccus_concentration",
    parameter=="duplicate_bacteria_concentration"~ "e_coli_concentration",
    TRUE                                         ~ parameter)) %>% 
  #convert readable parameters to coded values - Note that one Robious value
  #is with Colilert rather than Coliscan
  mutate(ParameterName=case_when(
    parameter=="air_temp_c"                    ~ "AT.2",
    parameter=="conductivity_us"               ~ "CO.4 (us/cm)",
    parameter=="turbidity_ntu"                 ~ "WC.6",
    parameter=="water_temp_c"                  ~ "WT.2",
    parameter=="COMMENTS"                      ~ "COMMENTS",
    parameter=="salinity_ppt"                  ~ "",
    parameter=="entercoccus_concentration"     ~ "ENT.1",
    parameter=="e_coli_concentration" &
      Station=="JRA.J26"&
      Date== as.Date("2020-06-25")             ~ "ECOLI.4",
    parameter=="e_coli_concentration" &
      Station %in% c("JRA.M05","JRA.J35","JRA.A08",
                     "JRA.J30","JRA.J29","JRA.J25",
                     "JRA.J26")                ~ "ECOLI.1",
    TRUE                                       ~ "ECOLI.4"
  )) %>% 
  


CHECK THE NA SITES!!!!!
  ROBIOUS HAD ONE WEEK OF IDEXX INSTEAD OF COLISCAN




#pull osborne landing for Stacy
stacy<-data2 %>% 
  filter(station_id=="J09") %>% 
  select(-entercoccus_concentration,-salinity_ppt,-e_coli_count)
write.csv(stacy,"data/subset_for_stacey.csv")
rm(stacy)
#####convert for CMC upload####
