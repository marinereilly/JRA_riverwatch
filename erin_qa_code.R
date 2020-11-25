library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

data<-readxl::read_xlsx("data/qa_data.xlsx",sheet = "edited data") %>% 
  select(-`...34`,-`...35`) %>% 
  janitor::clean_names()

master<-readr::read_csv("data/wr_import-template.csv")

junk<-rbind(master,data)

which(colnames(data)!=colnames(master))
q<-colnames(data)
r<-colnames(master)
q<-sort(q)
r<-sort(r)
