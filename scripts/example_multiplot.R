###PAckages#####

library(dplyr)
library(tidyr)
library(purrr)
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

#paredown data and make it long- do this with the cleaned data 
df2<-df %>% 
  select(collection_date,
         station_id,
         air_temp=air_temperature_p_707,
         conductivity=conductivity_p_709,
         e_coli=e_coli_concentration_p_711,
         entero=enterococcus_bacteria_concentration_p_1690,
         salinity=salinity_p_1715,
         turbidity=turbidity_p_710,
         water_temp=water_temperature_p_708) %>% 
  pivot_longer(cols=c("air_temp","conductivity","e_coli","entero","salinity",
                      "turbidity","water_temp"),
               values_to= "value",
               names_to= "parameter") %>% 
  drop_na(value)

#create plot ID - in this case just station/parameter
df2<-df2 %>% 
  mutate(plot_id=paste0(station_id,"_",parameter))

#nest the data by the plot id
df_nest<-df2 %>% 
  group_by(plot_id) %>% 
  nest()

#create plots - in the map function, the first value will go anywhere there
#is a .x, the second where there is a .y 
df_plots<-df_nest %>% 
  mutate(plots=map2(data,plot_id, ~ggplot(.x)+
                      ggtitle(.y)+
                      geom_point(aes(x=collection_date,y=value))+
                      xlab("Collection Date")+
                      theme_classic()))
#look at plot to see what adjustments need to be made
df_plots$plots[[1]]

#save plots
if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/station_plots")
map2(paste0("./figures/station_plots/", df_plots$plot_id, ".jpg"), 
     df_plots$plots, ggsave)





