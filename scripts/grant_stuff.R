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
library("Rmisc")
library(readr)
######### Load Data #######
df<-read.csv("data/tidied_df.csv")
summary(df)
site_names <- read_csv("data/Riverwatch SiteID, Names, Location - Sheet1.csv")
summary(site_names)

#####Clean Data ###########
# Correct Station Names
df1 <- merge(x=df, y = site_names, by.x = 'station_id', by.y='Station Number')
df1$`Station Description`<-as.factor(df1$`Station Description`)
df1$station_name <- as.factor(df1$station_name)
summary(df1 )


#Add order of Upstream-Downstream


###### Summary Tables #########
#Table by site of reasonable ranges 
sum <-df1 %>%
  group_by(station_id, month) %>%
  summarise(wat_avg = mean(wattemp_units, na.rm = TRUE)
            )
#### WHAT IS GOING ON HERE!?! WHY WONT THIS WORK! 



low_wat <- quantile(df1$wattemp_units, 
                 prob = 0.05, na.rm = TRUE)
high_wat <- quantile (df1$wattemp_units,
                      prob = 0.95, na.rm = TRUE)
tab <- df1 %>%
  group_by(station_id) %>%
  quantile (df1$wattemp_units,
            prob = 0.95, na.rm = TRUE)

wat_ranges <- do.call("rbind",
        tapply(df1$wattemp_units,                     # Specify numeric column
               df1$`Station Description`,            # Specify group variable
               quantile, na.rm = TRUE))
write.csv(wat_ranges,'tables/wat_ranges.csv')


#So I'm trying to create a table with station description 
# as each row and the 90% CI intervals for all the different
# parameters as columns. 
#The quantile function returns the right values but I 
# don't know how to input them into a compiled table without
# doing it step by step. 
# I also can't figure out how to properly group by station! 


  t.test(df1$wattemp_units)

  CI(wattemp_units, ci = 0.95) 

CI(df1$wattemp_units, ci= 0.95)
  CI(wattemp_units, ci = 0.95)


# Bacteria Summary Tables
tab_entero <- df1[!is.na(df1$entero_grade1), ] %>%
  group_by(`Station Description`) %>%
  summarise(ent_avg = mean(entero_grade1),
            n = length(station_id),
            sd = sd(entero_grade1),
            se = sd/sqrt(n))

tab_ecoli <- df1 [!is.na(df1$ecoli_grade1), ] %>%
  group_by(`Station Description`) %>%
  summarise(eco_avg = mean(ecoli_grade1),
            n = length(ecoli_grade1),
            sd = sd(ecoli_grade1),
            se = sd/sqrt(n))
write.csv(tab_ecoli, file = 'tables/ecoli_pass_rate_sitely.csv')
write.csv(tab_entero, file = 'tables/entero_pass_rate_sitely.csv')

