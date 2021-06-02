
library(tidyverse)

#set wd
setwd("~/Dropbox/Projects/Prioritizr/Data/")

puvspr_dat <- read.csv("/InputData/InputData/Comprehensive/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(puvspr_dat)

puvssp<-puvspr_dat %>% gather(species,amount,X1:X312) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/InputData/Comprehensive/puvsp.csv")


puvspr_dat <- read.csv("/InputData/InputData/Bird/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/InputData/Bird/puvsp.csv")


puvspr_dat <- read.csv("/InputData/Butterfly/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/InputData/Butterfly/puvsp.csv")

puvspr_dat <- read.csv("/InputData/PDiv_Fam/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/PDiv_Fam/puvsp.csv")

puvspr_dat <- read.csv("/InputData/PDiv_Gen/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/PDiv_Gen/puvsp.csv")


puvspr_dat <- read.csv("/InputData/PW_Butterfly_F/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/PW_Butterfly_F/puvsp.csv")

puvspr_dat <- read.csv("/InputData/PW_Butterfly_G/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/PW_Butterfly_G/puvsp.csv")


# Dominants
puvspr_dat <- read.csv("/InputData/Dominants/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X313) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"/InputData/Dominants/puvsp.csv")


# Comp Commercial

# Random


puvspr_dat <- read.csv("/InputData/Random/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(puvspr_dat)

puvssp<-puvspr_dat %>% gather(species,amount,X1:X313) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)


write.csv(puvssp,"/InputData/Random/puvsp.csv")


