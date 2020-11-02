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

######### Load Data #######
df<-read.csv("data/tidied_df.csv")
summary(df)
