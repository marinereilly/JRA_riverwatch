all_summary<-all.data %>% 
  group_by(station_number, station_description, location, year) %>% 
  summarise(
    sample_number=n(),
    pass=sum(pass_fail=="Pass")) %>% 
  mutate(percent_pass=scales::percent(pass/sample_number, accuracy = 1),
         pass_per=pass/sample_number*100) %>% 
  ungroup() %>% 
  mutate(station_number=factor(station_number, levels = c(
    "H02","J03","DC01","J04","P05","J05","C01","C05","A01","A02","A03",
    "J08","J15","J10","J20","J21","J22","J23","J24","R10","R20","R23",
    "J26","J25","A08","J30","J35","J40","M05"
  ), ordered=T)) %>% 
  arrange(station_number)

library(ggplot2)

a<-all_summary %>% 
  ggplot()+
  geom_tile(aes(x=year, y=forcats::fct_inorder(station_description), fill=pass_per), color="black")+
  scale_fill_viridis_c(name = "Percent Passed")+
  theme_classic()+ggtitle("Historic Pass Rates By Site")+
  geom_text(aes(x=year, y=station_description,label=percent_pass))+
  xlab("Year")+ylab("Station")+scale_x_continuous(breaks = seq(2013,2019,1))+
  theme(plot.title.position = "plot")
a