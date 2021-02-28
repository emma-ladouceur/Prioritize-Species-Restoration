

puvspr_dat <- read.csv("~/Dropbox/Projects/Marxan/Data/Comprehensive/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(puvspr_dat)

puvssp<-puvspr_dat %>% gather(species,amount,X1:X312) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Dropbox/Projects/Marxan/Data/Comprehensive/puvsp.csv")


puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/Bird/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/Bird/puvsp.csv")


puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/Butterfly/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/Butterfly/puvsp.csv")

puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/PDiv_Fam/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/PDiv_Fam/puvsp.csv")

puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/PDiv_Gen/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/PDiv_Gen/puvsp.csv")


puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/PW_Butterfly_F/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/PW_Butterfly_F/puvsp.csv")

puvspr_dat <- read.csv("~/Desktop/Academic/Data/Prioritizr/PW_Butterfly_G/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X319) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Desktop/Academic/Data/Prioritizr/PW_Butterfly_G/puvsp.csv")


# Dominants
puvspr_dat <- read.csv("~/Dropbox/Projects/Marxan/Data/Dominants/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvssp<-puvspr_dat %>% gather(species,amount,X1:X313) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)

write.csv(puvssp,"~/Dropbox/Projects/Marxan/Data/Dominants/puvsp.csv")


# Comp Commercial

# Random


puvspr_dat <- read.csv("~/Dropbox/Projects/Marxan/Data/Random/puvsp_w.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(puvspr_dat)

puvssp<-puvspr_dat %>% gather(species,amount,X1:X313) %>%
  filter(amount != 0) %>%
  mutate(species = str_replace(species, "X", "")) %>%
  rename(pu = id) %>% arrange(pu)


write.csv(puvssp,"~/Dropbox/Projects/Marxan/Data/Random/puvsp.csv")


