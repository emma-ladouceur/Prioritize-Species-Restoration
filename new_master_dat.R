


# After Roel's corrections

# new master dat
# December 2020


# changed some that Roel identified as 'remove' based on elevation to 'keepel' - this is because these species
# could occur in these subalpine habitats. i told roel min elevation was 1800 (based on JVS paper defs), 
#but in reality they range down to 1600- so i kept these   species
# also Roel noted that Parnassius corybas feeds on saxifragaceae- should it be added as a sax poll? probs not since my
# definition is 'species specific' plant and poll relationships



library(openxlsx)
library(tidyverse)

bfly_correct <- read.xlsx("~/Dropbox/Projects/Marxan/Data/Lepidoptera checking RvK.xlsx")

orig_master_dat <- read.xlsx("~/Dropbox/Projects/Marxan/Data/marx_master_1C.xlsx")


colnames(bfly_correct)

# clean up data
bfly_new <- bfly_correct %>% filter(!Recommendation == "remove") %>% droplevels() %>% # remove all bfly sp that Roel flags
  mutate( Plant_Pol_Relationship = ifelse(Recommendation == "add" , # if 'add' then fill in  plant_pol relationship
                                          conflict.in.resource.use, Plant_Pol_Relationship)) %>%
  mutate( Plant_Pol_Relationship = ifelse(Recommendation == "change resource" , # if 'change resource' then fill in  new plant_pol relationship
                                          conflict.in.resource.use, Plant_Pol_Relationship)) %>% 
  dplyr::select(species.id, plant.family, plant.species,Bfly_Sp,Plant_Pol_Relationship,`butterfly/moth`,Elevation,`Distribution.(only.if.not.in.Alps)`, Source)


View(bfly_new)

# write new clean copy of corrected butterflies and moths
write.csv(bfly_new, "~/Dropbox/Projects/Marxan/Data/bfly_new.csv")


View(bfly_new)

bfly_new$pres<-"1"

bfly_format <- bfly_new %>% dplyr::select(species.id,plant.family, plant.species,Bfly_Sp,Plant_Pol_Relationship,pres) %>%
  mutate(Bfly_Sp = str_replace(Bfly_Sp, " ", "_")) %>% # replace space with underscore
unite( Bfly, Plant_Pol_Relationship, Bfly_Sp,  sep="_") %>% # unite butterfly and relationship, out relationship first to alphabetically group
 mutate( id = species.id , # rename columns to match master dat
         f = plant.family ,
       s =  plant.species ) %>%
  dplyr::select(id,f,s,Bfly,pres) %>% # dplyr::select newly named columns
spread(Bfly, pres) %>% # spread to wide format, fill NA with 0
  mutate(
    across(everything(), ~replace_na(.x, 0)) # replace NA with 0
  )

colnames(bfly_format)
View(bfly_format)

colnames(orig_master_dat)

orig_master_dat$pres<-"1"
orig_master_dat$pres2<-"1"

master_clean <- orig_master_dat %>% 
  dplyr::select(-c( "Anthocharis_belia_Pollinator" : "Zygaena_exulans_Caterpillar", "totalcf" ,  "Achillea"  : "Viola"  ))  %>%  # remove all original butterflies and moths and total cf, and plant genus (redo it)
  separate(s, c("Genus", "Species"), sep=" ", remove= FALSE) 


colnames(master_clean)


 master_new <- master_clean %>% left_join(bfly_format) %>%
   spread(Genus, pres) %>%
   spread(f, pres2) %>%
   left_join(master_clean) %>%
   mutate(
     across(everything(), ~replace_na(.x, 0)) # replace NA with 0
   ) %>% dplyr::select(-pres, -Species, -Genus, -pres2)

 
colnames(master_new)
head(master_new)

write.csv(master_new, "~/Dropbox/Projects/Marxan/Data/master_new.csv")


master_new <- read.csv("~/Dropbox/Projects/Marxan/Data/master_new.csv")

 work_c <- master_new %>% dplyr::select(-c(Species, Genus,f, pres, pres2,growthform,specialists, freq6150,freq6170,freq6210,freq6230,freq651020,produced,avseedplantramet)) 

 
 
 View(work_c)   

 
 pu <- work_c %>% dplyr::select(id,s) %>%
   mutate( plant.sp = s) %>%
   dplyr::select(-plant.sp)

 pu$cost<- "1"
 pu$status<-"0"
 
 View(pu)
write.csv(pu, "~/Dropbox/Projects/Marxan/Data/Comprehensive/pu.csv")


colnames(work_c)

spec_prep <- work_c %>% gather(name, pres, c( "nitrogenfix":"Violaceae"  )) 
  
spec_n  <- spec_prep %>%
  dplyr::select(name) %>% distinct() %>%
   mutate(t_id = 1:n())


View(spec_n)

spec_id <- spec_prep %>% left_join(spec_n) %>%
  filter(pres != 0) 

pu_vs_sp <- spec_id %>% dplyr::select(-name) %>%
  spread(t_id, pres) %>%
  mutate(
    across(everything(), ~replace_na(.x, 0)) ) %>% # replace NA with 0
dplyr::select(-s)

write_csv(pu_vs_sp, "~/Dropbox/Projects/Marxan/Data/Comprehensive/puvsp_w.csv")



spec <- spec_n %>% mutate( id = t_id) %>%
  dplyr::select(-t_id)
  
View(spec)
 write_csv(spec, "~/Dropbox/Projects/Marxan/Data/Comprehensive/spec.csv ")

 
 
 
 # Dominants
 
 
 master_new <- read.csv("~/Dropbox/Projects/Marxan/Data/Dominants/master_new_d.csv")
 
 
 work_c <- master_new %>% dplyr::select(-c(growthform,specialists, freq6170,freq6150,freq6210,freq6230,freq651020,produced,avseedplantramet)) 
 
 
 colnames(work_c)
 
 spec_prep <- work_c %>% gather(name, pres, c( "dominant":"Violaceae"  )) 
 
 spec_n  <- spec_prep %>%
   dplyr::select(name) %>% distinct() %>%
   mutate(t_id = 1:n())
 
 
 View(spec_n)
 
 spec_id <- spec_prep %>% left_join(spec_n) %>%
   filter(pres != 0) 
 
 View(spec_id)
 
 pu_vs_sp <- spec_id %>% dplyr::select(-name,-Species) %>%
   spread(t_id, pres) %>%
   mutate(
     across(everything(), ~replace_na(.x, 0)) ) %>% # replace NA with 0
   dplyr::select(-s,-X)
 
 
 colnames(pu_vs_sp)
 
 write_csv(pu_vs_sp, "~/Dropbox/Projects/Marxan/Data/Dominants/puvsp_w.csv")
 
 
 
 spec <- spec_n %>% mutate( id = t_id) %>%
   dplyr::select(-t_id)
 
 View(spec)
 write_csv(spec, "~/Dropbox/Projects/Marxan/Data/Dominants/spec.csv ")

 
 # Randoms
 
 
 
 master_new <- read.csv("~/Dropbox/Projects/Marxan/Data/Random/master_new_r.csv")
 
 
 work_c <- master_new %>% dplyr::select(-c(growthform,specialists, freq6170,freq6150,freq6210,freq6230,freq651020,produced,avseedplantramet)) 
 
 
 
 colnames(work_c)
 
 spec_prep <- work_c %>% gather(name, pres, c( "random":"Violaceae"  )) 
 
 spec_n  <- spec_prep %>%
   dplyr::select(name) %>% distinct() %>%
   mutate(t_id = 1:n())
 
 
 View(spec_n)
 
 spec_id <- spec_prep %>% left_join(spec_n) %>%
   filter(pres != 0) 
 
 View(spec_id)
 
 pu_vs_sp <- spec_id %>% dplyr::select(-name,-Species) %>%
   spread(t_id, pres) %>%
   mutate(
     across(everything(), ~replace_na(.x, 0)) ) %>% # replace NA with 0
   dplyr::select(-s,-X)
 
 
 colnames(pu_vs_sp)
 
 write_csv(pu_vs_sp, "~/Dropbox/Projects/Marxan/Data/Random/puvsp_w.csv")
 
 
 
 spec <- spec_n %>% mutate( id = t_id) %>%
   dplyr::select(-t_id)
 
 View(spec)
 write_csv(spec, "~/Dropbox/Projects/Marxan/Data/Dominants/spec.csv ")
 
  
