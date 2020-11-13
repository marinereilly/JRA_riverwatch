library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(janitor)
library(readr)

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

site_names <- read_csv("data/Riverwatch SiteID, Names, Location - Sheet1.csv") %>% 
  clean_names()

colnames(df)
colnames(site_names)

df2<-site_names %>% 
  rename(station_id=station_number)%>% 
  left_join(df,., by=c("station_id")) %>% 
  mutate(station_description=case_when(
    station_description=="Hopewell Rt. 10"            ~ "Hopewell Rt 10",
    station_description=="Farmville Main St. Bridge"  ~ "Farmville Main St Bridge",
    station_description=="Rockett\u0092s Landing"     ~ "Rocketts Landing",
    TRUE                                              ~ station_description
  ))
