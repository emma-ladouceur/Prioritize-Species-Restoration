

# In this script we:
# _________________________________________________________
# #############     Visualize solutions     ###############
# #############     Quantitatively compare  ###############
# #############     Produce Figures         ###############
# _________________________________________________________

# packages
library(tidyverse)
library(ggplot2)
library(viridis)

# data
solution_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/solution_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
random_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

master_dat<-bind_rows(solution_dat,random_dat)

head(master_dat)

dat1 <- master_dat %>% as_tibble() %>%
  group_by(id,objective) %>%
  filter(solution_n == "solution_1") %>%
  filter(solution != 0) 


# Figure 3
dat1$objective<- as.factor(dat1$objective)
levels(dat1$objective)

dat1$objective <- factor(dat1$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)",  "Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))

# alternative ordering
dat1$objective <- factor(dat1$objective, levels=c( "Comprehensive (n=37)", "Bird (n=5)","Lepidoptera Relationship (n=35)", "Pairwise Lepidoptera + Plant Rich Family (n=47)" ,"Pairwise Lepidoptera + Plant Rich Genus (n=119)",
                                                  "Dominants (n=37)",  "Plant Rich Family (n=34)", "Plant Rich Genus (n=115)", "Random"))
#alt color orders
# scale_color_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#66A61E","#E6AB02","#15983DFF","#666666")) +
#   scale_fill_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#66A61E","#E6AB02","#15983DFF","#666666")) +

ggplot(dat1,aes(x=objective, y=totalcf, color=objective),alpha=0.6,dotsize=0.5) + 
  geom_violin(aes(color = NA, fill=objective), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Attribute Sum',fill='Objective',color='Objective') + 
  scale_color_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#E6AB02","#66A61E","#15983DFF","#666666")) +
     scale_fill_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#E6AB02","#66A61E","#15983DFF","#666666")) +
  annotate("text", x= 3, y=77, label= "Prioritized Objectives", size= 5) +
  annotate("segment", x = 0.5, xend = 2, y = 77, yend = 77, colour = "black")+
  annotate("segment", x = 0.5, xend = 0.5, y = 77, yend = 75, colour = "black")+
  annotate("segment", x = 4, xend = 5.25, y = 77, yend = 77, colour = "black")+
  annotate("segment", x = 5.25, xend = 5.25, y = 77, yend = 75, colour = "black")+
  annotate("text", x= 7.5, y=77, label= "Comparison Objectives", size= 5) +
  annotate("segment", x = 5.75, xend = 6.5, y = 77, yend = 77, colour = "black")+
  annotate("segment", x = 5.75, xend = 5.75, y = 77, yend = 75, colour = "black")+
  annotate("segment", x = 8.5, xend = 9.25, y = 77, yend = 77, colour = "black")+
  annotate("segment", x = 9.25, xend = 9.25, y = 77, yend = 75, colour = "black")+
  theme_classic() + theme(legend.position="none",
                          text = element_text(size=(20))) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


# LANDSCAPE 9X14

# Expanded Fig 2 : Figure S1

head(dat1)
dat1$solution_amount <- as.factor(as.character(dat1$solution_amount))
levels(dat1$solution_amount)
head(dat1)
levels(dat1$objective)

dat.sep <- dat1 %>% filter(objective == "Bird (n=5)"  | objective == "Lepidoptera Relationship (n=35)" | 
                             objective == "Dominants (n=37)" | objective == "Plant Rich Genus (n=115)"   ) %>%
  mutate( dot = ".") %>% unite( solution_amount, dot, solution_amount, sep="")

head(dat.sep)

dat2 <- dat1 %>%  filter(!objective == "Bird (n=5)"  ) %>% filter( !objective == "Lepidoptera Relationship (n=35)"  ) %>% filter( 
  !objective == "Dominants (n=37)"  ) %>% filter( !objective == "Plant Rich Genus (n=115)"   ) %>% droplevels() %>%
  bind_rows(dat.sep)

head(dat2)
dat2$solution_amount <- factor(dat2$solution_amount, levels=c("5",".5","10","15","20","25","30","34","35",".35","37",".37","39","40","45","47","50","55","60","65","70","75","80","85","90","95","100","105","110","115",".115","116","119","120","121","125"))

dat2$objective <- factor(dat2$objective, levels=c( "Comprehensive (n=37)", "Bird (n=5)","Lepidoptera Relationship (n=35)", "Pairwise Lepidoptera + Plant Rich Family (n=47)" ,"Pairwise Lepidoptera + Plant Rich Genus (n=119)",
                                                   "Dominants (n=37)",  "Plant Rich Family (n=34)", "Plant Rich Genus (n=115)", "Random"))

ggplot(dat2,aes(x=solution_amount, y=totalcf, color=objective),alpha=0.6,dotsize=0.5) + 
  geom_violin(aes(color = NA, fill=objective), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = 'Number of plant species',y = 'Attribute Sum',fill='Objective',color='Objective') + 
  # scale_color_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  # scale_fill_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  scale_color_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#E6AB02","#66A61E","#15983DFF","#666666")) +
  scale_fill_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#E6AB02","#66A61E","#15983DFF","#666666")) +
  theme_classic() + theme(legend.position="bottom",
                          text = element_text(size=(18))) +   guides(col = guide_legend(ncol = 3))

# LANDSCAPE 9X14

# Figure 4 : Irreplaceability
head(master_dat)
master_dat$sf <- as.numeric(master_dat$sf)
importance_dat <- master_dat %>%  mutate( Importance = ifelse(master_dat$sf == 0, 'Redundant',
                                ifelse(master_dat$sf >= 1 & master_dat$sf <= 99, 'Variable',
                                       ifelse(master_dat$sf >= 100, 'Irreplaceable', 'other'))) 
                                )  %>% as_tibble() %>%
  group_by(id,objective) %>%
  filter(solution_n == "solution_1") %>% filter(!objective == "Random")


#importance_dat$objective <- factor(importance_dat$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)",  "Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)"))

importance_dat$objective <- factor(importance_dat$objective, levels=c( "Comprehensive (n=37)", "Bird (n=5)","Lepidoptera Relationship (n=35)", "Pairwise Lepidoptera + Plant Rich Family (n=47)" ,"Pairwise Lepidoptera + Plant Rich Genus (n=119)",
                                                   "Dominants (n=37)",  "Plant Rich Family (n=34)", "Plant Rich Genus (n=115)"))


importance_dat$Importance <- factor(importance_dat$Importance, levels=c("Irreplaceable","Variable", "Redundant"))

# Figure 3
ggplot(importance_dat,aes(x=Importance, y=totalcf,color=Importance),alpha=0.6,dotsize=0.5) + 
  facet_wrap(facets= "objective") +
  geom_violin(aes(color = NA, fill=Importance), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Attribute Sum',fill='',color='') + 
  scale_color_manual(values=c("#7F8624FF" ,"#52194CFF","#751029FF")) +
  scale_fill_manual(values=c("#7F8624FF","#52194CFF","#751029FF")) +
  theme_classic() + theme(legend.position="bottom") + theme(axis.title.x=element_blank(),
                                                            axis.text.x=element_blank(),
                                                            axis.ticks.x=element_blank(),
                                                            text = element_text(size=(14)))

#LANDSCAPE 8.50 X 12

# FIGURE 5
# MIXED LAYERS CLOUD + RANDOM MEAN + QUANTILES 
edata<- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_objective_features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
rdata<- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rdata.l <- rdata %>% gather(quantile,percent_targs,mean.held:upper.held)

rdata.l$category <- factor(rdata.l$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome", "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))

edata$category <- factor(edata$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome",  "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))

#edata$objective <- factor(edata$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)","Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))

edata$objective <- factor(edata$objective, levels=c( "Comprehensive (n=37)", "Bird (n=5)","Lepidoptera Relationship (n=35)", "Pairwise Lepidoptera + Plant Rich Family (n=47)" ,"Pairwise Lepidoptera + Plant Rich Genus (n=119)",
                                                                       "Dominants (n=37)",  "Plant Rich Family (n=34)", "Plant Rich Genus (n=115)", "Random"))


# FIGURE 5
ggplot() +
  geom_jitter(data=rdata.l, aes(x=species, y=percent_targs),color="#666666", size=5,width = 1, height = 0.5, alpha=0.4) +
  geom_jitter(data=edata, aes(x=species, y=percent_targs,color=objective), size=6,width = 1, height = 0.5, alpha=0.8) +
  #scale_colour_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF","#15983DFF","#1F78B4","#666666")) + 
  scale_color_manual(values=c("#D95F02","#E7298A","#7570B3","#16A08CFF", "#1F78B4","#E6AB02","#66A61E","#15983DFF","#666666")) +
  facet_grid(. ~ category)+ facet_wrap(~category, labeller = labeller(groupwrap = label_wrap_gen(10)))+
  labs(x = 'Number of Plant Species',y = 'Percentage of Targets Met',color='Objective') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+ 
  theme(axis.text.x=element_text(angle=45, hjust=1),
        text = element_text(size=(20))) +   guides(col = guide_legend(ncol = 3))


# Figure S2 : Random Cloud
pdata <- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(pdata)
pdata$category <- as.factor(pdata$category)
levels(pdata$category)


pdata$category <- factor(pdata$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome", "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))


# Figure S2
ggplot(pdata, aes(x=species, y=percent_targs)) +
  geom_jitter(aes(color=species), size=6,width = 0.5, height = 0.5,alpha=0.7)+
  facet_grid(. ~ category)+ facet_wrap(~category, labeller = labeller(groupwrap = label_wrap_gen(10)))+
  scale_color_viridis()+
  labs(x = 'Number of Plant Species',y = 'Percentage of Targets Met',color=' Number of Plant Species') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) 




