library(purrr)
library(lubridate)
library(tidyr)

df<-data.table::fread("data/qa_data.csv") %>% 
  mutate(collection_date=ymd_hms(collection_date)) %>% 
  mutate(date=date(collection_date)) %>% 
  mutate(sample_name=paste0(station_id,"_",date))

photo_data<-df %>% 
  select(sample_name,`Current Site Conditions Photo [f:749]`,
         `Bacteria Sample Picture [f:748]`) %>% 
  pivot_longer(cols = c(`Current Site Conditions Photo [f:749]`,
                        `Bacteria Sample Picture [f:748]`),
               names_to= "photo_type",
               values_to="url") %>% 
  mutate(photo_type=case_when(
    photo_type=="Current Site Conditions Photo [f:749]"   ~ "site",
    photo_type=="Bacteria Sample Picture [f:748]"         ~ "bacteria"
  )) %>% 
  mutate(filename=paste0(photo_type,"_", sample_name),
         url=na_if(url,""),
         dest=paste0("batch_photo_downloads/",filename,".jpg"))%>% 
  drop_na(url)

Map(function(u, d) download.file(u, d, mode="wb"), photo_data$url, photo_data$dest)


