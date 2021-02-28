

library(tidyverse)

bfly <- read.csv("~/Desktop/Academic/Data/Prioritizr/roel_dat_check.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(bfly)


bfly.long <- bfly %>% gather(Bfly_sp,pres,Anthocharis_belia_Pollinator:Zygaena_exulans_Larval) %>%
  filter(pres != 0) %>% droplevels() %>% separate(Bfly_sp, c("Bfly_Genus", "Bfly_sp","Plant_Pol_Relationship"), sep="_") %>%
  unite(Bfly_Sp, Bfly_Genus:Bfly_sp, sep=" ") %>% select(-pres)



View(bfly.long)


write.csv(bfly.long, "~/Desktop/Academic/Data/Prioritizr/roel_bfly_long.csv")
