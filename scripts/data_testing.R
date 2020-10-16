###PAckages#####

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(janitor)
library(data.table)

###data loading####
df<-readxl::read_xlsx("data/HAIRWORKINGcopy.xlsx",
                      col_types = c("text","date","numeric","text",
                                    "numeric","text","numeric", "text",
                                    "numeric","text","numeric","numeric",
                                    "numeric","numeric","numeric","text",
                                    "text","numeric","numeric","numeric",
                                    "numeric","numeric","text","text",
                                    "logical", "numeric","text","text",
                                    "numeric","text","text","text","text")) %>% 
  clean_names()

####Plotting####

a<-df %>% 
  ggplot()+
  geom_point(aes(x=collection_date, 
                 y=air_temperature_p_707, color=station_id))+
  theme_classic()
a

##### Qa ######
bad_temp<-df %>% 
  filter(air_temperature_p_707>=50) 

good_temp_df<-df %>% 
  mutate(temp_units=
           case_when(
             air_temperature_p_707>=50  ~(air_temperature_p_707-32)*5/9,
             air_temperature_p_707<50   ~air_temperature_p_707
           ))
a<-good_temp_df %>% 
  ggplot()+
  geom_point(aes(x=collection_date, 
                 y=temp_units, color=station_id))+
  theme_classic()
a

##Im trying to see if I am connected to git
bad_temp
