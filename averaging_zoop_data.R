setwd("/Users/katieschroeder/Documents/GitHub/ECOL_4000_6000")
zoop <- read.csv("SRS_zoop.csv")


zoop_spring <- zoop %>% filter(month==3|month==4|month==5) %>% 
  group_by(bay,taxa) %>%
  summarize(average_abund = mean(abund))
  
zoop_winter <-zoop %>% filter(month==12|month==1|month==2)
  group_by(bay,taxa) %>%
  summarize(average_abund = mean(abund))

spring_wide <- zoop_spring %>% pivot_wider(names_from=taxa,values_from = average_abund)

spring_wide[is.na(spring_wide)] <- 0

env_spring <- zoop %>%
  filter(month==3|month==4|month==5) %>%
  group_by(bay) %>%
    summarize(
      mean_pH=mean(pH),
      mean_cond=mean(conductivity),
      canopy=unique(canopy)
    )

spring <- merge(spring_taxa,env_spring,by="bay") 