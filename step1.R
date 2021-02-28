

# load packages
library(tidyverse)
library(prioritizr)
library(data.table)
library(gurobi)
library(slam)
library(purrr)

# Comprehensive Objective
# load in the tabular planning unit data
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# setwd("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/")
# save(pu_dat, spec_dat, puvsp_dat, file = "prioritizr-data.rda")


p1 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0)  %>% # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) #%>%


# print problem
print(p1)

# print number of planning units
number_of_planning_units(p1)
# print number of features
number_of_features(p1)

s1 <- solve(p1)

# print solution
print(s1)


## calculate feature representation
#solution_col_names <- grep("solution", names(s1), value = TRUE)

solution_col_names <-  s1 %>% dplyr::select(contains("solution")) %>%
colnames() 


fr_all <-  solution_col_names %>% map( ~feature_representation(p1, s1[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')



comp_prep <- as.data.frame(s1) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

View(comp_prep)

 comp_s <- comp_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(comp_prep) %>%
   mutate(objective = "Comprehensive (n=37)",
           solution_amount = "37") %>% group_by(solution_n) %>%
     dplyr::select(-X) %>% arrange(solution_n,s)
    #nest(solution_df = c("id","s","solution", "sf","cost", "status", "totalcf"))
 
   
View(comp_s)  
  # fr_all <- purrr::map_at(solution_col_names, feature_representation(p1, s1)
  #   )
  #   
    

 
comp_s$cost <- as.numeric(comp_s$cost)
 # Features Dataset
#nested attempt
# comp_test <- comp_s %>%
#       purrr::map(function(solution_df) feature_representation(p1, solution)) 

feats <- comp_s %>% group_by(solution_n) %>%
  dplyr::select(solution_n, solution) %>%
  mutate(feature_representation(p1, solution) )



comp_1 <- feature_representation(p1, comp_s[, "solution_1"])
comp_1$run <- "solution_1"
comp_2 <- feature_representation(p1, comp_s[, "solution_2"])
comp_2$run <- "solution_2"
comp_3 <- feature_representation(p1, comp_s[, "solution_3"])
comp_3$run <- "solution_3"
comp_4 <- feature_representation(p1, comp_s[, "solution_4"])
comp_4$run <- "solution_4"
comp_5 <- feature_representation(p1, comp_s[, "solution_5"])
comp_5$run <- "solution_5"
comp_6 <- feature_representation(p1, comp_s[, "solution_6"])
comp_6$run <- "solution_6"
comp_7 <- feature_representation(p1, comp_s[, "solution_7"])
comp_7$run <- "solution_7"
comp_8 <- feature_representation(p1, comp_s[, "solution_8"])
comp_8$run <- "solution_8"
comp_9 <- feature_representation(p1, comp_s[, "solution_9"])
comp_9$run <- "solution_9"
comp_10 <- feature_representation(p1, comp_s[, "solution_10"])
comp_10$run <- "solution_10"
comp_11 <- feature_representation(p1, comp_s[, "solution_11"])
comp_11$run <- "solution_11"
comp_12 <- feature_representation(p1, comp_s[, "solution_12"])
comp_12$run <- "solution_12"
comp_13 <- feature_representation(p1, comp_s[, "solution_13"])
comp_13$run <- "solution_13"
comp_14 <- feature_representation(p1, comp_s[, "solution_14"])
comp_14$run <- "solution_14"
comp_15 <- feature_representation(p1, comp_s[, "solution_15"])
comp_15$run <- "solution_15"
comp_16 <- feature_representation(p1, comp_s[, "solution_16"])
comp_16$run <- "solution_16"
comp_17 <- feature_representation(p1, comp_s[, "solution_17"])
comp_17$run <- "solution_17"
comp_18 <- feature_representation(p1, comp_s[, "solution_18"])
comp_18$run <- "solution_18"
comp_19 <- feature_representation(p1, comp_s[, "solution_19"])
comp_19$run <- "solution_19"
comp_20 <- feature_representation(p1, comp_s[, "solution_20"])
comp_20$run <- "solution_20"
comp_21 <- feature_representation(p1, comp_s[, "solution_21"])
comp_21$run <- "solution_21"
comp_22 <- feature_representation(p1, comp_s[, "solution_22"])
comp_22$run <- "solution_22"
comp_23 <- feature_representation(p1, comp_s[, "solution_23"])
comp_23$run <- "solution_23"
comp_24 <- feature_representation(p1, comp_s[, "solution_24"])
comp_24$run <- "solution_24"
comp_25 <- feature_representation(p1, comp_s[, "solution_25"])
comp_25$run <- "solution_25"
comp_26 <- feature_representation(p1, comp_s[, "solution_26"])
comp_26$run <- "solution_26"
comp_27 <- feature_representation(p1, comp_s[, "solution_27"])
comp_27$run <- "solution_27"
comp_28 <- feature_representation(p1, comp_s[, "solution_28"])
comp_28$run <- "solution_28"
comp_29 <- feature_representation(p1, comp_s[, "solution_29"])
comp_29$run <- "solution_29"
comp_r<-bind_rows(comp_1,comp_2,comp_3,comp_4,comp_5,comp_6,comp_7,comp_8,comp_9,comp_10,comp_11,comp_12,comp_13,comp_14,comp_15,comp_16,comp_17,comp_18,comp_19,comp_20,comp_21,comp_22,comp_23,comp_24,comp_25,comp_26,comp_27,comp_28,comp_29)
comp_r$objective <- "Comprehensive (n=37)"
comp_r$species<-"37"

feature_a <- feature_abundances(p1, na.rm = FALSE)

View(feature_a)

compr<-left_join(comp_r,feature_a)
View(compr)
View(comp_s)


#Butterfly
#planning unit
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p2 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0)  %>% # optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s2 <- solve(p2)
print(attr(s2, "objective"))
print(s2)
bfly<-as.data.frame(s2) %>% as_tibble()

colnames(bfly)
bfly_s <- bfly %>% mutate(sf = rowSums(.[7:8])) 

bfly_s$objective<-"Butterfly (n=35)"
bfly_s$solution<-"35"
bfly_s$total_solution<-"2"

bfly_1 <- feature_representation(p2, bfly_s[, "solution_1"])
bfly_1$run <- "solution_1"
bfly_2 <- feature_representation(p2, bfly_s[, "solution_2"])
bfly_2$run <- "solution_2"
bfly_r<-bind_rows(bfly_1,bfly_2)
bfly_r$objective <- "Butterfly (n=35)"
bfly_r$species<-"35"

feature_a <- feature_abundances(p2, na.rm = FALSE)

bflyr<-left_join(bfly_r,feature_a)
View(bflyr)

#Bird
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p3 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s3 <- solve(p3)
print(attr(s3, "objective"))

bird<-as.data.frame(s3) %>% as_tibble()

colnames(bird)
bird_s <- bird %>% mutate(sf = rowSums(.[7:30])) 

bird_s$objective<-"Bird (n=5)"
bird_s$solution<-"5"
bird_s$total_solution<-"24"

head(bird_s)

bird_r <- feature_representation(p3, bird_s[, "solution_1"])
bird_r$objective <- "Bird (n=6)"
head(bfly_r)

# Features Dataset
bird_1 <- feature_representation(p3, bird_s[, "solution_1"])
bird_1$run <- "solution_1"
bird_2 <- feature_representation(p3, bird_s[, "solution_2"])
bird_2$run <- "solution_2"
bird_3 <- feature_representation(p3, bird_s[, "solution_3"])
bird_3$run <- "solution_3"
bird_4 <- feature_representation(p3, bird_s[, "solution_4"])
bird_4$run <- "solution_4"
bird_5 <- feature_representation(p3, bird_s[, "solution_5"])
bird_5$run <- "solution_5"
bird_6 <- feature_representation(p3, bird_s[, "solution_6"])
bird_6$run <- "solution_6"
bird_7 <- feature_representation(p3, bird_s[, "solution_7"])
bird_7$run <- "solution_7"
bird_8 <- feature_representation(p3, bird_s[, "solution_8"])
bird_8$run <- "solution_8"
bird_9 <- feature_representation(p3, bird_s[, "solution_9"])
bird_9$run <- "solution_9"
bird_10 <- feature_representation(p3, bird_s[, "solution_10"])
bird_10$run <- "solution_10"
bird_11 <- feature_representation(p3, bird_s[, "solution_11"])
bird_11$run <- "solution_11"
bird_12 <- feature_representation(p3, bird_s[, "solution_12"])
bird_12$run <- "solution_12"
bird_13 <- feature_representation(p3, bird_s[, "solution_13"])
bird_13$run <- "solution_13"
bird_14 <- feature_representation(p3, bird_s[, "solution_14"])
bird_14$run <- "solution_14"
bird_15 <- feature_representation(p3, bird_s[, "solution_15"])
bird_15$run <- "solution_15"
bird_16 <- feature_representation(p3, bird_s[, "solution_16"])
bird_16$run <- "solution_16"
bird_17 <- feature_representation(p3, bird_s[, "solution_17"])
bird_17$run <- "solution_17"
bird_18 <- feature_representation(p3, bird_s[, "solution_18"])
bird_18$run <- "solution_18"
bird_19 <- feature_representation(p3, bird_s[, "solution_19"])
bird_19$run <- "solution_19"
bird_20 <- feature_representation(p3, bird_s[, "solution_20"])
bird_20$run <- "solution_20"
bird_21 <- feature_representation(p3, bird_s[, "solution_21"])
bird_21$run <- "solution_21"
bird_22 <- feature_representation(p3, bird_s[, "solution_22"])
bird_22$run <- "solution_22"
bird_23 <- feature_representation(p3, bird_s[, "solution_23"])
bird_23$run <- "solution_23"

bird_r<-bind_rows(bird_1,bird_2,bird_3,bird_4,bird_5,bird_6,bird_7,bird_8,bird_9,bird_10,bird_11,bird_12,bird_13,bird_14,bird_15,bird_16,bird_17,bird_18,bird_19,bird_20,bird_21,bird_22,bird_23)
bird_r$objective <- "Bird (n=5)"
bird_r$species<-"5"


feature_a <- feature_abundances(p3, na.rm = FALSE)

birdr<-left_join(bird_r,feature_a)
View(birdr)

#Dominants
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p4 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s4 <- solve(p4)

doms<-as.data.frame(s4) %>% as_tibble()

colnames(doms)
dom_s <- doms %>% mutate(sf = rowSums(.[6])) 

dom_s$objective<-"Dominants (n=37)"
dom_s$solution<-"37"
dom_s$total_solution<-"1"

head(dom_s)

dom_r <- feature_representation(p4, dom_s[, "solution_1"])
dom_r$objective <- "Dominants (n=37)"
dom_r$species<-"37"
head(dom_r)

feature_a <- feature_abundances(p4, na.rm = FALSE)

domr<-left_join(dom_r,feature_a)
View(domr)

View(dom_s)

#P Div Fam
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p5 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s5 <- solve(p5)
print(attr(s5, "objective"))

pdivf<-as.data.frame(s5) %>% as_tibble()

colnames(pdivf)
pdivf_s <- pdivf %>% mutate(sf = rowSums(.[7:106])) 

pdivf_s$objective<-"Plant Div Family (n=34)"
pdivf_s$solution<-"34"
pdivf_s$total_solution<-"100"

View(pdivf_s)


# Features Dataset
pdivf_1 <- feature_representation(p5, pdivf_s[, "solution_1"])
pdivf_1$run <- "solution_1"
pdivf_2 <- feature_representation(p5, pdivf_s[, "solution_2"])
pdivf_2$run <- "solution_2"
pdivf_3 <- feature_representation(p5, pdivf_s[, "solution_3"])
pdivf_3$run <- "solution_3"
pdivf_4 <- feature_representation(p5, pdivf_s[, "solution_4"])
pdivf_4$run <- "solution_4"
pdivf_5 <- feature_representation(p5, pdivf_s[, "solution_5"])
pdivf_5$run <- "solution_5"
pdivf_6 <- feature_representation(p5, pdivf_s[, "solution_6"])
pdivf_6$run <- "solution_6"
pdivf_7 <- feature_representation(p5, pdivf_s[, "solution_7"])
pdivf_7$run <- "solution_7"
pdivf_8 <- feature_representation(p5, pdivf_s[, "solution_8"])
pdivf_8$run <- "solution_8"
pdivf_9 <- feature_representation(p5, pdivf_s[, "solution_9"])
pdivf_9$run <- "solution_9"
pdivf_10 <- feature_representation(p5, pdivf_s[, "solution_10"])
pdivf_10$run <- "solution_10"
pdivf_11 <- feature_representation(p5, pdivf_s[, "solution_11"])
pdivf_11$run <- "solution_11"
pdivf_12 <- feature_representation(p5, pdivf_s[, "solution_12"])
pdivf_12$run <- "solution_12"
pdivf_13 <- feature_representation(p5, pdivf_s[, "solution_13"])
pdivf_13$run <- "solution_13"
pdivf_14 <- feature_representation(p5, pdivf_s[, "solution_14"])
pdivf_14$run <- "solution_14"
pdivf_15 <- feature_representation(p5, pdivf_s[, "solution_15"])
pdivf_15$run <- "solution_15"
pdivf_16 <- feature_representation(p5, pdivf_s[, "solution_16"])
pdivf_16$run <- "solution_16"
pdivf_17 <- feature_representation(p5, pdivf_s[, "solution_17"])
pdivf_17$run <- "solution_17"
pdivf_18 <- feature_representation(p5, pdivf_s[, "solution_18"])
pdivf_18$run <- "solution_18"
pdivf_19 <- feature_representation(p5, pdivf_s[, "solution_19"])
pdivf_19$run <- "solution_19"
pdivf_20 <- feature_representation(p5, pdivf_s[, "solution_20"])
pdivf_20$run <- "solution_20"
pdivf_21 <- feature_representation(p5, pdivf_s[, "solution_21"])
pdivf_21$run <- "solution_21"
pdivf_22 <- feature_representation(p5, pdivf_s[, "solution_22"])
pdivf_22$run <- "solution_22"
pdivf_23 <- feature_representation(p5, pdivf_s[, "solution_23"])
pdivf_23$run <- "solution_23"
pdivf_24 <- feature_representation(p5, pdivf_s[, "solution_24"])
pdivf_24$run <- "solution_24"
pdivf_25 <- feature_representation(p5, pdivf_s[, "solution_25"])
pdivf_25$run <- "solution_25"
pdivf_26 <- feature_representation(p5, pdivf_s[, "solution_26"])
pdivf_26$run <- "solution_26"
pdivf_27 <- feature_representation(p5, pdivf_s[, "solution_27"])
pdivf_27$run <- "solution_27"
pdivf_28 <- feature_representation(p5, pdivf_s[, "solution_28"])
pdivf_28$run <- "solution_28"
pdivf_29 <- feature_representation(p5, pdivf_s[, "solution_29"])
pdivf_29$run <- "solution_29"
pdivf_30 <- feature_representation(p5, pdivf_s[, "solution_30"])
pdivf_30$run <- "solution_30"
pdivf_31 <- feature_representation(p5, pdivf_s[, "solution_31"])
pdivf_31$run <- "solution_31"
pdivf_32 <- feature_representation(p5, pdivf_s[, "solution_32"])
pdivf_32$run <- "solution_32"
pdivf_33 <- feature_representation(p5, pdivf_s[, "solution_33"])
pdivf_33$run <- "solution_33"
pdivf_34 <- feature_representation(p5, pdivf_s[, "solution_34"])
pdivf_34$run <- "solution_34"
pdivf_35 <- feature_representation(p5, pdivf_s[, "solution_35"])
pdivf_35$run <- "solution_35"
pdivf_36 <- feature_representation(p5, pdivf_s[, "solution_36"])
pdivf_36$run <- "solution_36"
pdivf_37 <- feature_representation(p5, pdivf_s[, "solution_37"])
pdivf_37$run <- "solution_37"
pdivf_38 <- feature_representation(p5, pdivf_s[, "solution_38"])
pdivf_38$run <- "solution_38"
pdivf_39 <- feature_representation(p5, pdivf_s[, "solution_39"])
pdivf_39$run <- "solution_39"
pdivf_40 <- feature_representation(p5, pdivf_s[, "solution_40"])
pdivf_40$run <- "solution_40"
pdivf_41 <- feature_representation(p5, pdivf_s[, "solution_41"])
pdivf_41$run <- "solution_41"
pdivf_42 <- feature_representation(p5, pdivf_s[, "solution_42"])
pdivf_42$run <- "solution_42"
pdivf_43 <- feature_representation(p5, pdivf_s[, "solution_43"])
pdivf_43$run <- "solution_43"
pdivf_44 <- feature_representation(p5, pdivf_s[, "solution_44"])
pdivf_44$run <- "solution_44"
pdivf_45 <- feature_representation(p5, pdivf_s[, "solution_45"])
pdivf_45$run <- "solution_45"
pdivf_46 <- feature_representation(p5, pdivf_s[, "solution_46"])
pdivf_46$run <- "solution_46"
pdivf_47 <- feature_representation(p5, pdivf_s[, "solution_47"])
pdivf_47$run <- "solution_47"
pdivf_48 <- feature_representation(p5, pdivf_s[, "solution_48"])
pdivf_48$run <- "solution_48"
pdivf_49 <- feature_representation(p5, pdivf_s[, "solution_49"])
pdivf_49$run <- "solution_49"
pdivf_50 <- feature_representation(p5, pdivf_s[, "solution_50"])
pdivf_50$run <- "solution_50"
pdivf_51 <- feature_representation(p5, pdivf_s[, "solution_51"])
pdivf_51$run <- "solution_51"
pdivf_52 <- feature_representation(p5, pdivf_s[, "solution_52"])
pdivf_52$run <- "solution_52"
pdivf_53 <- feature_representation(p5, pdivf_s[, "solution_53"])
pdivf_53$run <- "solution_53"
pdivf_54 <- feature_representation(p5, pdivf_s[, "solution_54"])
pdivf_54$run <- "solution_54"
pdivf_55 <- feature_representation(p5, pdivf_s[, "solution_55"])
pdivf_55$run <- "solution_55"
pdivf_56 <- feature_representation(p5, pdivf_s[, "solution_56"])
pdivf_56$run <- "solution_56"
pdivf_57 <- feature_representation(p5, pdivf_s[, "solution_57"])
pdivf_57$run <- "solution_57"
pdivf_58 <- feature_representation(p5, pdivf_s[, "solution_58"])
pdivf_58$run <- "solution_58"
pdivf_59 <- feature_representation(p5, pdivf_s[, "solution_59"])
pdivf_59$run <- "solution_59"
pdivf_60 <- feature_representation(p5, pdivf_s[, "solution_60"])
pdivf_60$run <- "solution_60"
pdivf_61 <- feature_representation(p5, pdivf_s[, "solution_61"])
pdivf_61$run <- "solution_61"
pdivf_62 <- feature_representation(p5, pdivf_s[, "solution_62"])
pdivf_62$run <- "solution_62"
pdivf_63 <- feature_representation(p5, pdivf_s[, "solution_63"])
pdivf_63$run <- "solution_63"
pdivf_64 <- feature_representation(p5, pdivf_s[, "solution_64"])
pdivf_64$run <- "solution_64"
pdivf_65 <- feature_representation(p5, pdivf_s[, "solution_65"])
pdivf_65$run <- "solution_65"
pdivf_66 <- feature_representation(p5, pdivf_s[, "solution_66"])
pdivf_66$run <- "solution_66"
pdivf_67 <- feature_representation(p5, pdivf_s[, "solution_67"])
pdivf_67$run <- "solution_67"
pdivf_68 <- feature_representation(p5, pdivf_s[, "solution_68"])
pdivf_68$run <- "solution_68"
pdivf_69 <- feature_representation(p5, pdivf_s[, "solution_69"])
pdivf_69$run <- "solution_69"
pdivf_70 <- feature_representation(p5, pdivf_s[, "solution_70"])
pdivf_70$run <- "solution_70"
pdivf_71 <- feature_representation(p5, pdivf_s[, "solution_71"])
pdivf_71$run <- "solution_71"
pdivf_72 <- feature_representation(p5, pdivf_s[, "solution_72"])
pdivf_72$run <- "solution_72"
pdivf_73 <- feature_representation(p5, pdivf_s[, "solution_73"])
pdivf_73$run <- "solution_73"
pdivf_74 <- feature_representation(p5, pdivf_s[, "solution_74"])
pdivf_74$run <- "solution_74"
pdivf_75 <- feature_representation(p5, pdivf_s[, "solution_75"])
pdivf_75$run <- "solution_75"
pdivf_76 <- feature_representation(p5, pdivf_s[, "solution_76"])
pdivf_76$run <- "solution_76"
pdivf_77 <- feature_representation(p5, pdivf_s[, "solution_77"])
pdivf_77$run <- "solution_77"
pdivf_78 <- feature_representation(p5, pdivf_s[, "solution_78"])
pdivf_78$run <- "solution_78"
pdivf_79 <- feature_representation(p5, pdivf_s[, "solution_79"])
pdivf_79$run <- "solution_79"
pdivf_80 <- feature_representation(p5, pdivf_s[, "solution_80"])
pdivf_80$run <- "solution_80"
pdivf_81 <- feature_representation(p5, pdivf_s[, "solution_81"])
pdivf_81$run <- "solution_81"
pdivf_82 <- feature_representation(p5, pdivf_s[, "solution_82"])
pdivf_82$run <- "solution_82"
pdivf_83 <- feature_representation(p5, pdivf_s[, "solution_83"])
pdivf_83$run <- "solution_83"
pdivf_84 <- feature_representation(p5, pdivf_s[, "solution_84"])
pdivf_84$run <- "solution_84"
pdivf_85 <- feature_representation(p5, pdivf_s[, "solution_85"])
pdivf_85$run <- "solution_85"
pdivf_86 <- feature_representation(p5, pdivf_s[, "solution_86"])
pdivf_86$run <- "solution_86"
pdivf_87 <- feature_representation(p5, pdivf_s[, "solution_87"])
pdivf_87$run <- "solution_87"
pdivf_88 <- feature_representation(p5, pdivf_s[, "solution_88"])
pdivf_88$run <- "solution_88"
pdivf_89 <- feature_representation(p5, pdivf_s[, "solution_89"])
pdivf_89$run <- "solution_89"
pdivf_90 <- feature_representation(p5, pdivf_s[, "solution_90"])
pdivf_90$run <- "solution_90"
pdivf_91 <- feature_representation(p5, pdivf_s[, "solution_91"])
pdivf_91$run <- "solution_91"
pdivf_92 <- feature_representation(p5, pdivf_s[, "solution_92"])
pdivf_92$run <- "solution_92"
pdivf_93 <- feature_representation(p5, pdivf_s[, "solution_93"])
pdivf_93$run <- "solution_93"
pdivf_94 <- feature_representation(p5, pdivf_s[, "solution_94"])
pdivf_94$run <- "solution_94"
pdivf_95 <- feature_representation(p5, pdivf_s[, "solution_95"])
pdivf_95$run <- "solution_95"
pdivf_96 <- feature_representation(p5, pdivf_s[, "solution_96"])
pdivf_96$run <- "solution_96"
pdivf_97 <- feature_representation(p5, pdivf_s[, "solution_97"])
pdivf_97$run <- "solution_97"
pdivf_98 <- feature_representation(p5, pdivf_s[, "solution_98"])
pdivf_98$run <- "solution_98"
pdivf_99 <- feature_representation(p5, pdivf_s[, "solution_99"])
pdivf_99$run <- "solution_99"
pdivf_100 <- feature_representation(p5, pdivf_s[, "solution_100"])
pdivf_100$run <- "solution_100"
pdivf_r<-bind_rows(pdivf_1,pdivf_2,pdivf_3,pdivf_4,pdivf_5,pdivf_6,pdivf_7,pdivf_8,pdivf_9,pdivf_10,pdivf_11,pdivf_12,pdivf_13,pdivf_14,pdivf_15,pdivf_16,pdivf_17,pdivf_18,pdivf_19,pdivf_20,pdivf_21,pdivf_22,pdivf_23,pdivf_24,pdivf_25,pdivf_26,pdivf_27,pdivf_28,pdivf_29,pdivf_30,pdivf_31,pdivf_32,pdivf_33,pdivf_34,pdivf_35,pdivf_36,pdivf_37,pdivf_38,pdivf_39,pdivf_40,pdivf_41,pdivf_42,pdivf_43,pdivf_44,pdivf_45,pdivf_46,pdivf_47,pdivf_48,pdivf_49,pdivf_50,pdivf_51,pdivf_52,pdivf_53,pdivf_54,pdivf_55,pdivf_56,pdivf_57,pdivf_58,pdivf_59,pdivf_60,pdivf_61,pdivf_62,pdivf_63,pdivf_64,pdivf_65,pdivf_66,pdivf_67,pdivf_68,pdivf_69,pdivf_70,pdivf_71,pdivf_72,pdivf_73,pdivf_74,pdivf_75,pdivf_76,pdivf_77,pdivf_78,pdivf_79,pdivf_80,pdivf_81,pdivf_82,pdivf_83,pdivf_84,pdivf_85,pdivf_86,pdivf_87,pdivf_88,pdivf_89,pdivf_90,pdivf_91,pdivf_92,pdivf_93,pdivf_94,pdivf_95,pdivf_96,pdivf_97,pdivf_98,pdivf_99,pdivf_100)
pdivf_r$objective <- "Plant Div Family (n=34)"
pdivf_r$species<-"34"


feature_a <- feature_abundances(p5, na.rm = FALSE)

pdivfr<-left_join(pdivf_r,feature_a)
View(pdivfr)

#P Div Gen
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p6 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s6 <- solve(p6)
print(attr(s6, "objective"))

pdivg<-as.data.frame(s6) %>% as_tibble()

colnames(pdivg)
pdivg_s <- pdivg %>% mutate(sf = rowSums(.[7:106])) 

pdivg_s$objective<-"Plant Div Genus (n=115)"
pdivg_s$solution<-"115"
pdivg_s$total_solution<-"100"


# Features Dataset
pdivg_1 <- feature_representation(p6, pdivg_s[, "solution_1"])
pdivg_1$run <- "solution_1"
pdivg_2 <- feature_representation(p6, pdivg_s[, "solution_2"])
pdivg_2$run <- "solution_2"
pdivg_3 <- feature_representation(p6, pdivg_s[, "solution_3"])
pdivg_3$run <- "solution_3"
pdivg_4 <- feature_representation(p6, pdivg_s[, "solution_4"])
pdivg_4$run <- "solution_4"
pdivg_5 <- feature_representation(p6, pdivg_s[, "solution_5"])
pdivg_5$run <- "solution_5"
pdivg_6 <- feature_representation(p6, pdivg_s[, "solution_6"])
pdivg_6$run <- "solution_6"
pdivg_7 <- feature_representation(p6, pdivg_s[, "solution_7"])
pdivg_7$run <- "solution_7"
pdivg_8 <- feature_representation(p6, pdivg_s[, "solution_8"])
pdivg_8$run <- "solution_8"
pdivg_9 <- feature_representation(p6, pdivg_s[, "solution_9"])
pdivg_9$run <- "solution_9"
pdivg_10 <- feature_representation(p6, pdivg_s[, "solution_10"])
pdivg_10$run <- "solution_10"
pdivg_11 <- feature_representation(p6, pdivg_s[, "solution_11"])
pdivg_11$run <- "solution_11"
pdivg_12 <- feature_representation(p6, pdivg_s[, "solution_12"])
pdivg_12$run <- "solution_12"
pdivg_13 <- feature_representation(p6, pdivg_s[, "solution_13"])
pdivg_13$run <- "solution_13"
pdivg_14 <- feature_representation(p6, pdivg_s[, "solution_14"])
pdivg_14$run <- "solution_14"
pdivg_15 <- feature_representation(p6, pdivg_s[, "solution_15"])
pdivg_15$run <- "solution_15"
pdivg_16 <- feature_representation(p6, pdivg_s[, "solution_16"])
pdivg_16$run <- "solution_16"
pdivg_17 <- feature_representation(p6, pdivg_s[, "solution_17"])
pdivg_17$run <- "solution_17"
pdivg_18 <- feature_representation(p6, pdivg_s[, "solution_18"])
pdivg_18$run <- "solution_18"
pdivg_19 <- feature_representation(p6, pdivg_s[, "solution_19"])
pdivg_19$run <- "solution_19"
pdivg_20 <- feature_representation(p6, pdivg_s[, "solution_20"])
pdivg_20$run <- "solution_20"
pdivg_21 <- feature_representation(p6, pdivg_s[, "solution_21"])
pdivg_21$run <- "solution_21"
pdivg_22 <- feature_representation(p6, pdivg_s[, "solution_22"])
pdivg_22$run <- "solution_22"
pdivg_23 <- feature_representation(p6, pdivg_s[, "solution_23"])
pdivg_23$run <- "solution_23"
pdivg_24 <- feature_representation(p6, pdivg_s[, "solution_24"])
pdivg_24$run <- "solution_24"
pdivg_25 <- feature_representation(p6, pdivg_s[, "solution_25"])
pdivg_25$run <- "solution_25"
pdivg_26 <- feature_representation(p6, pdivg_s[, "solution_26"])
pdivg_26$run <- "solution_26"
pdivg_27 <- feature_representation(p6, pdivg_s[, "solution_27"])
pdivg_27$run <- "solution_27"
pdivg_28 <- feature_representation(p6, pdivg_s[, "solution_28"])
pdivg_28$run <- "solution_28"
pdivg_29 <- feature_representation(p6, pdivg_s[, "solution_29"])
pdivg_29$run <- "solution_29"
pdivg_30 <- feature_representation(p6, pdivg_s[, "solution_30"])
pdivg_30$run <- "solution_30"
pdivg_31 <- feature_representation(p6, pdivg_s[, "solution_31"])
pdivg_31$run <- "solution_31"
pdivg_32 <- feature_representation(p6, pdivg_s[, "solution_32"])
pdivg_32$run <- "solution_32"
pdivg_33 <- feature_representation(p6, pdivg_s[, "solution_33"])
pdivg_33$run <- "solution_33"
pdivg_34 <- feature_representation(p6, pdivg_s[, "solution_34"])
pdivg_34$run <- "solution_34"
pdivg_35 <- feature_representation(p6, pdivg_s[, "solution_35"])
pdivg_35$run <- "solution_35"
pdivg_36 <- feature_representation(p6, pdivg_s[, "solution_36"])
pdivg_36$run <- "solution_36"
pdivg_37 <- feature_representation(p6, pdivg_s[, "solution_37"])
pdivg_37$run <- "solution_37"
pdivg_38 <- feature_representation(p6, pdivg_s[, "solution_38"])
pdivg_38$run <- "solution_38"
pdivg_39 <- feature_representation(p6, pdivg_s[, "solution_39"])
pdivg_39$run <- "solution_39"
pdivg_40 <- feature_representation(p6, pdivg_s[, "solution_40"])
pdivg_40$run <- "solution_40"
pdivg_41 <- feature_representation(p6, pdivg_s[, "solution_41"])
pdivg_41$run <- "solution_41"
pdivg_42 <- feature_representation(p6, pdivg_s[, "solution_42"])
pdivg_42$run <- "solution_42"
pdivg_43 <- feature_representation(p6, pdivg_s[, "solution_43"])
pdivg_43$run <- "solution_43"
pdivg_44 <- feature_representation(p6, pdivg_s[, "solution_44"])
pdivg_44$run <- "solution_44"
pdivg_45 <- feature_representation(p6, pdivg_s[, "solution_45"])
pdivg_45$run <- "solution_45"
pdivg_46 <- feature_representation(p6, pdivg_s[, "solution_46"])
pdivg_46$run <- "solution_46"
pdivg_47 <- feature_representation(p6, pdivg_s[, "solution_47"])
pdivg_47$run <- "solution_47"
pdivg_48 <- feature_representation(p6, pdivg_s[, "solution_48"])
pdivg_48$run <- "solution_48"
pdivg_49 <- feature_representation(p6, pdivg_s[, "solution_49"])
pdivg_49$run <- "solution_49"
pdivg_50 <- feature_representation(p6, pdivg_s[, "solution_50"])
pdivg_50$run <- "solution_50"
pdivg_51 <- feature_representation(p6, pdivg_s[, "solution_51"])
pdivg_51$run <- "solution_51"
pdivg_52 <- feature_representation(p6, pdivg_s[, "solution_52"])
pdivg_52$run <- "solution_52"
pdivg_53 <- feature_representation(p6, pdivg_s[, "solution_53"])
pdivg_53$run <- "solution_53"
pdivg_54 <- feature_representation(p6, pdivg_s[, "solution_54"])
pdivg_54$run <- "solution_54"
pdivg_55 <- feature_representation(p6, pdivg_s[, "solution_55"])
pdivg_55$run <- "solution_55"
pdivg_56 <- feature_representation(p6, pdivg_s[, "solution_56"])
pdivg_56$run <- "solution_56"
pdivg_57 <- feature_representation(p6, pdivg_s[, "solution_57"])
pdivg_57$run <- "solution_57"
pdivg_58 <- feature_representation(p6, pdivg_s[, "solution_58"])
pdivg_58$run <- "solution_58"
pdivg_59 <- feature_representation(p6, pdivg_s[, "solution_59"])
pdivg_59$run <- "solution_59"
pdivg_60 <- feature_representation(p6, pdivg_s[, "solution_60"])
pdivg_60$run <- "solution_60"
pdivg_61 <- feature_representation(p6, pdivg_s[, "solution_61"])
pdivg_61$run <- "solution_61"
pdivg_62 <- feature_representation(p6, pdivg_s[, "solution_62"])
pdivg_62$run <- "solution_62"
pdivg_63 <- feature_representation(p6, pdivg_s[, "solution_63"])
pdivg_63$run <- "solution_63"
pdivg_64 <- feature_representation(p6, pdivg_s[, "solution_64"])
pdivg_64$run <- "solution_64"
pdivg_65 <- feature_representation(p6, pdivg_s[, "solution_65"])
pdivg_65$run <- "solution_65"
pdivg_66 <- feature_representation(p6, pdivg_s[, "solution_66"])
pdivg_66$run <- "solution_66"
pdivg_67 <- feature_representation(p6, pdivg_s[, "solution_67"])
pdivg_67$run <- "solution_67"
pdivg_68 <- feature_representation(p6, pdivg_s[, "solution_68"])
pdivg_68$run <- "solution_68"
pdivg_69 <- feature_representation(p6, pdivg_s[, "solution_69"])
pdivg_69$run <- "solution_69"
pdivg_70 <- feature_representation(p6, pdivg_s[, "solution_70"])
pdivg_70$run <- "solution_70"
pdivg_71 <- feature_representation(p6, pdivg_s[, "solution_71"])
pdivg_71$run <- "solution_71"
pdivg_72 <- feature_representation(p6, pdivg_s[, "solution_72"])
pdivg_72$run <- "solution_72"
pdivg_73 <- feature_representation(p6, pdivg_s[, "solution_73"])
pdivg_73$run <- "solution_73"
pdivg_74 <- feature_representation(p6, pdivg_s[, "solution_74"])
pdivg_74$run <- "solution_74"
pdivg_75 <- feature_representation(p6, pdivg_s[, "solution_75"])
pdivg_75$run <- "solution_75"
pdivg_76 <- feature_representation(p6, pdivg_s[, "solution_76"])
pdivg_76$run <- "solution_76"
pdivg_77 <- feature_representation(p6, pdivg_s[, "solution_77"])
pdivg_77$run <- "solution_77"
pdivg_78 <- feature_representation(p6, pdivg_s[, "solution_78"])
pdivg_78$run <- "solution_78"
pdivg_79 <- feature_representation(p6, pdivg_s[, "solution_79"])
pdivg_79$run <- "solution_79"
pdivg_80 <- feature_representation(p6, pdivg_s[, "solution_80"])
pdivg_80$run <- "solution_80"
pdivg_81 <- feature_representation(p6, pdivg_s[, "solution_81"])
pdivg_81$run <- "solution_81"
pdivg_82 <- feature_representation(p6, pdivg_s[, "solution_82"])
pdivg_82$run <- "solution_82"
pdivg_83 <- feature_representation(p6, pdivg_s[, "solution_83"])
pdivg_83$run <- "solution_83"
pdivg_84 <- feature_representation(p6, pdivg_s[, "solution_84"])
pdivg_84$run <- "solution_84"
pdivg_85 <- feature_representation(p6, pdivg_s[, "solution_85"])
pdivg_85$run <- "solution_85"
pdivg_86 <- feature_representation(p6, pdivg_s[, "solution_86"])
pdivg_86$run <- "solution_86"
pdivg_87 <- feature_representation(p6, pdivg_s[, "solution_87"])
pdivg_87$run <- "solution_87"
pdivg_88 <- feature_representation(p6, pdivg_s[, "solution_88"])
pdivg_88$run <- "solution_88"
pdivg_89 <- feature_representation(p6, pdivg_s[, "solution_89"])
pdivg_89$run <- "solution_89"
pdivg_90 <- feature_representation(p6, pdivg_s[, "solution_90"])
pdivg_90$run <- "solution_90"
pdivg_91 <- feature_representation(p6, pdivg_s[, "solution_91"])
pdivg_91$run <- "solution_91"
pdivg_92 <- feature_representation(p6, pdivg_s[, "solution_92"])
pdivg_92$run <- "solution_92"
pdivg_93 <- feature_representation(p6, pdivg_s[, "solution_93"])
pdivg_93$run <- "solution_93"
pdivg_94 <- feature_representation(p6, pdivg_s[, "solution_94"])
pdivg_94$run <- "solution_94"
pdivg_95 <- feature_representation(p6, pdivg_s[, "solution_95"])
pdivg_95$run <- "solution_95"
pdivg_96 <- feature_representation(p6, pdivg_s[, "solution_96"])
pdivg_96$run <- "solution_96"
pdivg_97 <- feature_representation(p6, pdivg_s[, "solution_97"])
pdivg_97$run <- "solution_97"
pdivg_98 <- feature_representation(p6, pdivg_s[, "solution_98"])
pdivg_98$run <- "solution_98"
pdivg_99 <- feature_representation(p6, pdivg_s[, "solution_99"])
pdivg_99$run <- "solution_99"
pdivg_100 <- feature_representation(p6, pdivg_s[, "solution_100"])
pdivg_100$run <- "solution_100"
pdivg_r<-bind_rows(pdivg_1,pdivg_2,pdivg_3,pdivg_4,pdivg_5,pdivg_6,pdivg_7,pdivg_8,pdivg_9,pdivg_10,pdivg_11,pdivg_12,pdivg_13,pdivg_14,pdivg_15,pdivg_16,pdivg_17,pdivg_18,pdivg_19,pdivg_20,pdivg_21,pdivg_22,pdivg_23,pdivg_24,pdivg_25,pdivg_26,pdivg_27,pdivg_28,pdivg_29,pdivg_30,pdivg_31,pdivg_32,pdivg_33,pdivg_34,pdivg_35,pdivg_36,pdivg_37,pdivg_38,pdivg_39,pdivg_40,pdivg_41,pdivg_42,pdivg_43,pdivg_44,pdivg_45,pdivg_46,pdivg_47,pdivg_48,pdivg_49,pdivg_50,pdivg_51,pdivg_52,pdivg_53,pdivg_54,pdivg_55,pdivg_56,pdivg_57,pdivg_58,pdivg_59,pdivg_60,pdivg_61,pdivg_62,pdivg_63,pdivg_64,pdivg_65,pdivg_66,pdivg_67,pdivg_68,pdivg_69,pdivg_70,pdivg_71,pdivg_72,pdivg_73,pdivg_74,pdivg_75,pdivg_76,pdivg_77,pdivg_78,pdivg_79,pdivg_80,pdivg_81,pdivg_82,pdivg_83,pdivg_84,pdivg_85,pdivg_86,pdivg_87,pdivg_88,pdivg_89,pdivg_90,pdivg_91,pdivg_92,pdivg_93,pdivg_94,pdivg_95,pdivg_96,pdivg_97,pdivg_98,pdivg_99,pdivg_100)
pdivg_r$objective <- "Plant Div Genus (n=115)"
pdivg_r$species<-"115"


feature_a <- feature_abundances(p6, na.rm = FALSE)

pdivgr<-left_join(pdivg_r,feature_a)
View(pdivgr)


#PW_Butterfly_F
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p7 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s7 <- solve(p7)
print(attr(s7, "objective"))

pwf<-as.data.frame(s7) %>% as_tibble()

colnames(pwf)
pwf_s <- pwf %>% mutate(sf = rowSums(.[7:101])) 

pwf_s$objective<-"Pairwise Butterfly Div Family (n=47)"
pwf_s$solution<-"47"
pwf_s$total_solution<-"95"

pwf_r <- feature_representation(p7, pwf_s[, "solution_1"])
pwf_r$objective <- "Pairwise Butterfly Div Family (n=47)"
head(pwf_r)


# Features Dataset
pwf_1 <- feature_representation(p7, pwf_s[, "solution_1"])
pwf_1$run <- "solution_1"
pwf_2 <- feature_representation(p7, pwf_s[, "solution_2"])
pwf_2$run <- "solution_2"
pwf_3 <- feature_representation(p7, pwf_s[, "solution_3"])
pwf_3$run <- "solution_3"
pwf_4 <- feature_representation(p7, pwf_s[, "solution_4"])
pwf_4$run <- "solution_4"
pwf_5 <- feature_representation(p7, pwf_s[, "solution_5"])
pwf_5$run <- "solution_5"
pwf_6 <- feature_representation(p7, pwf_s[, "solution_6"])
pwf_6$run <- "solution_6"
pwf_7 <- feature_representation(p7, pwf_s[, "solution_7"])
pwf_7$run <- "solution_7"
pwf_8 <- feature_representation(p7, pwf_s[, "solution_8"])
pwf_8$run <- "solution_8"
pwf_9 <- feature_representation(p7, pwf_s[, "solution_9"])
pwf_9$run <- "solution_9"
pwf_10 <- feature_representation(p7, pwf_s[, "solution_10"])
pwf_10$run <- "solution_10"
pwf_11 <- feature_representation(p7, pwf_s[, "solution_11"])
pwf_11$run <- "solution_11"
pwf_12 <- feature_representation(p7, pwf_s[, "solution_12"])
pwf_12$run <- "solution_12"
pwf_13 <- feature_representation(p7, pwf_s[, "solution_13"])
pwf_13$run <- "solution_13"
pwf_14 <- feature_representation(p7, pwf_s[, "solution_14"])
pwf_14$run <- "solution_14"
pwf_15 <- feature_representation(p7, pwf_s[, "solution_15"])
pwf_15$run <- "solution_15"
pwf_16 <- feature_representation(p7, pwf_s[, "solution_16"])
pwf_16$run <- "solution_16"
pwf_17 <- feature_representation(p7, pwf_s[, "solution_17"])
pwf_17$run <- "solution_17"
pwf_18 <- feature_representation(p7, pwf_s[, "solution_18"])
pwf_18$run <- "solution_18"
pwf_19 <- feature_representation(p7, pwf_s[, "solution_19"])
pwf_19$run <- "solution_19"
pwf_20 <- feature_representation(p7, pwf_s[, "solution_20"])
pwf_20$run <- "solution_20"
pwf_21 <- feature_representation(p7, pwf_s[, "solution_21"])
pwf_21$run <- "solution_21"
pwf_22 <- feature_representation(p7, pwf_s[, "solution_22"])
pwf_22$run <- "solution_22"
pwf_23 <- feature_representation(p7, pwf_s[, "solution_23"])
pwf_23$run <- "solution_23"
pwf_24 <- feature_representation(p7, pwf_s[, "solution_24"])
pwf_24$run <- "solution_24"
pwf_25 <- feature_representation(p7, pwf_s[, "solution_25"])
pwf_25$run <- "solution_25"
pwf_26 <- feature_representation(p7, pwf_s[, "solution_26"])
pwf_26$run <- "solution_26"
pwf_27 <- feature_representation(p7, pwf_s[, "solution_27"])
pwf_27$run <- "solution_27"
pwf_28 <- feature_representation(p7, pwf_s[, "solution_28"])
pwf_28$run <- "solution_28"
pwf_29 <- feature_representation(p7, pwf_s[, "solution_29"])
pwf_29$run <- "solution_29"
pwf_30 <- feature_representation(p7, pwf_s[, "solution_30"])
pwf_30$run <- "solution_30"
pwf_31 <- feature_representation(p7, pwf_s[, "solution_31"])
pwf_31$run <- "solution_31"
pwf_32 <- feature_representation(p7, pwf_s[, "solution_32"])
pwf_32$run <- "solution_32"
pwf_33 <- feature_representation(p7, pwf_s[, "solution_33"])
pwf_33$run <- "solution_33"
pwf_34 <- feature_representation(p7, pwf_s[, "solution_34"])
pwf_34$run <- "solution_34"
pwf_35 <- feature_representation(p7, pwf_s[, "solution_35"])
pwf_35$run <- "solution_35"
pwf_36 <- feature_representation(p7, pwf_s[, "solution_36"])
pwf_36$run <- "solution_36"
pwf_37 <- feature_representation(p7, pwf_s[, "solution_37"])
pwf_37$run <- "solution_37"
pwf_38 <- feature_representation(p7, pwf_s[, "solution_38"])
pwf_38$run <- "solution_38"
pwf_39 <- feature_representation(p7, pwf_s[, "solution_39"])
pwf_39$run <- "solution_39"
pwf_40 <- feature_representation(p7, pwf_s[, "solution_40"])
pwf_40$run <- "solution_40"
pwf_41 <- feature_representation(p7, pwf_s[, "solution_41"])
pwf_41$run <- "solution_41"
pwf_42 <- feature_representation(p7, pwf_s[, "solution_42"])
pwf_42$run <- "solution_42"
pwf_43 <- feature_representation(p7, pwf_s[, "solution_43"])
pwf_43$run <- "solution_43"
pwf_44 <- feature_representation(p7, pwf_s[, "solution_44"])
pwf_44$run <- "solution_44"
pwf_45 <- feature_representation(p7, pwf_s[, "solution_45"])
pwf_45$run <- "solution_45"
pwf_46 <- feature_representation(p7, pwf_s[, "solution_46"])
pwf_46$run <- "solution_46"
pwf_47 <- feature_representation(p7, pwf_s[, "solution_47"])
pwf_47$run <- "solution_47"
pwf_48 <- feature_representation(p7, pwf_s[, "solution_48"])
pwf_48$run <- "solution_48"
pwf_49 <- feature_representation(p7, pwf_s[, "solution_49"])
pwf_49$run <- "solution_49"
pwf_50 <- feature_representation(p7, pwf_s[, "solution_50"])
pwf_50$run <- "solution_50"
pwf_51 <- feature_representation(p7, pwf_s[, "solution_51"])
pwf_51$run <- "solution_51"
pwf_52 <- feature_representation(p7, pwf_s[, "solution_52"])
pwf_52$run <- "solution_52"
pwf_53 <- feature_representation(p7, pwf_s[, "solution_53"])
pwf_53$run <- "solution_53"
pwf_54 <- feature_representation(p7, pwf_s[, "solution_54"])
pwf_54$run <- "solution_54"
pwf_55 <- feature_representation(p7, pwf_s[, "solution_55"])
pwf_55$run <- "solution_55"
pwf_56 <- feature_representation(p7, pwf_s[, "solution_56"])
pwf_56$run <- "solution_56"
pwf_57 <- feature_representation(p7, pwf_s[, "solution_57"])
pwf_57$run <- "solution_57"
pwf_58 <- feature_representation(p7, pwf_s[, "solution_58"])
pwf_58$run <- "solution_58"
pwf_59 <- feature_representation(p7, pwf_s[, "solution_59"])
pwf_59$run <- "solution_59"
pwf_60 <- feature_representation(p7, pwf_s[, "solution_60"])
pwf_60$run <- "solution_60"
pwf_61 <- feature_representation(p7, pwf_s[, "solution_61"])
pwf_61$run <- "solution_61"
pwf_62 <- feature_representation(p7, pwf_s[, "solution_62"])
pwf_62$run <- "solution_62"
pwf_63 <- feature_representation(p7, pwf_s[, "solution_63"])
pwf_63$run <- "solution_63"
pwf_64 <- feature_representation(p7, pwf_s[, "solution_64"])
pwf_64$run <- "solution_64"
pwf_65 <- feature_representation(p7, pwf_s[, "solution_65"])
pwf_65$run <- "solution_65"
pwf_66 <- feature_representation(p7, pwf_s[, "solution_66"])
pwf_66$run <- "solution_66"
pwf_67 <- feature_representation(p7, pwf_s[, "solution_67"])
pwf_67$run <- "solution_67"
pwf_68 <- feature_representation(p7, pwf_s[, "solution_68"])
pwf_68$run <- "solution_68"
pwf_69 <- feature_representation(p7, pwf_s[, "solution_69"])
pwf_69$run <- "solution_69"
pwf_70 <- feature_representation(p7, pwf_s[, "solution_70"])
pwf_70$run <- "solution_70"
pwf_71 <- feature_representation(p7, pwf_s[, "solution_71"])
pwf_71$run <- "solution_71"
pwf_72 <- feature_representation(p7, pwf_s[, "solution_72"])
pwf_72$run <- "solution_72"
pwf_73 <- feature_representation(p7, pwf_s[, "solution_73"])
pwf_73$run <- "solution_73"
pwf_74 <- feature_representation(p7, pwf_s[, "solution_74"])
pwf_74$run <- "solution_74"
pwf_75 <- feature_representation(p7, pwf_s[, "solution_75"])
pwf_75$run <- "solution_75"
pwf_76 <- feature_representation(p7, pwf_s[, "solution_76"])
pwf_76$run <- "solution_76"
pwf_77 <- feature_representation(p7, pwf_s[, "solution_77"])
pwf_77$run <- "solution_77"
pwf_78 <- feature_representation(p7, pwf_s[, "solution_78"])
pwf_78$run <- "solution_78"
pwf_79 <- feature_representation(p7, pwf_s[, "solution_79"])
pwf_79$run <- "solution_79"
pwf_80 <- feature_representation(p7, pwf_s[, "solution_80"])
pwf_80$run <- "solution_80"
pwf_81 <- feature_representation(p7, pwf_s[, "solution_81"])
pwf_81$run <- "solution_81"
pwf_82 <- feature_representation(p7, pwf_s[, "solution_82"])
pwf_82$run <- "solution_82"
pwf_83 <- feature_representation(p7, pwf_s[, "solution_83"])
pwf_83$run <- "solution_83"
pwf_84 <- feature_representation(p7, pwf_s[, "solution_84"])
pwf_84$run <- "solution_84"
pwf_85 <- feature_representation(p7, pwf_s[, "solution_85"])
pwf_85$run <- "solution_85"
pwf_86 <- feature_representation(p7, pwf_s[, "solution_86"])
pwf_86$run <- "solution_86"
pwf_87 <- feature_representation(p7, pwf_s[, "solution_87"])
pwf_87$run <- "solution_87"
pwf_88 <- feature_representation(p7, pwf_s[, "solution_88"])
pwf_88$run <- "solution_88"
pwf_89 <- feature_representation(p7, pwf_s[, "solution_89"])
pwf_89$run <- "solution_89"
pwf_90 <- feature_representation(p7, pwf_s[, "solution_90"])
pwf_90$run <- "solution_90"
pwf_91 <- feature_representation(p7, pwf_s[, "solution_91"])
pwf_91$run <- "solution_91"
pwf_92 <- feature_representation(p7, pwf_s[, "solution_92"])
pwf_92$run <- "solution_92"
pwf_93 <- feature_representation(p7, pwf_s[, "solution_93"])
pwf_93$run <- "solution_93"
pwf_94 <- feature_representation(p7, pwf_s[, "solution_94"])
pwf_94$run <- "solution_94"
pwf_95 <- feature_representation(p7, pwf_s[, "solution_95"])
pwf_95$run <- "solution_95"

pwf_r<-bind_rows(pwf_1,pwf_2,pwf_3,pwf_4,pwf_5,pwf_6,pwf_7,pwf_8,pwf_9,pwf_10,pwf_11,pwf_12,pwf_13,pwf_14,pwf_15,pwf_16,pwf_17,pwf_18,pwf_19,pwf_20,pwf_21,pwf_22,pwf_23,pwf_24,pwf_25,pwf_26,pwf_27,pwf_28,pwf_29,pwf_30,pwf_31,pwf_32,pwf_33,pwf_34,pwf_35,pwf_36,pwf_37,pwf_38,pwf_39,pwf_40,pwf_41,pwf_42,pwf_43,pwf_44,pwf_45,pwf_46,pwf_47,pwf_48,pwf_49,pwf_50,pwf_51,pwf_52,pwf_53,pwf_54,pwf_55,pwf_56,pwf_57,pwf_58,pwf_59,pwf_60,pwf_61,pwf_62,pwf_63,pwf_64,pwf_65,pwf_66,pwf_67,pwf_68,pwf_69,pwf_70,pwf_71,pwf_72,pwf_73,pwf_74,pwf_75,pwf_76,pwf_77,pwf_78,pwf_79,pwf_80,pwf_81,pwf_82,pwf_83,pwf_84,pwf_85,pwf_86,pwf_87,pwf_88,pwf_89,pwf_90,pwf_91,pwf_92,pwf_93,pwf_94,pwf_95)
pwf_r$objective <- "Pairwise Butterfly Div Family (n=47)"
pwf_r$species<-"47"


feature_a <- feature_abundances(p7, na.rm = FALSE)

pwfr<-left_join(pwf_r,feature_a)
View(pwfr)


#PW_Butterfly_G
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# create problem
p8 <- problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

s8 <- solve(p8)

pwg<-as.data.frame(s8) %>% as_tibble()

colnames(pwg)
pwg_s <- pwg %>% mutate(sf = rowSums(.[7:106])) 

pwg_s$objective<-"Pairwise Butterfly Div Genus (n=119)"
pwg_s$solution<-"119"
pwg_s$total_solution<-"100"

pwg_r <- feature_representation(p8, pwg_s[, "solution_1"])
pwg_r$objective <- "Pairwise Butterfly Div Genus (n=119)"
head(pwg_r)

# Features Dataset
pwg_1 <- feature_representation(p8, pwg_s[, "solution_1"])
pwg_1$run <- "solution_1"
pwg_2 <- feature_representation(p8, pwg_s[, "solution_2"])
pwg_2$run <- "solution_2"
pwg_3 <- feature_representation(p8, pwg_s[, "solution_3"])
pwg_3$run <- "solution_3"
pwg_4 <- feature_representation(p8, pwg_s[, "solution_4"])
pwg_4$run <- "solution_4"
pwg_5 <- feature_representation(p8, pwg_s[, "solution_5"])
pwg_5$run <- "solution_5"
pwg_6 <- feature_representation(p8, pwg_s[, "solution_6"])
pwg_6$run <- "solution_6"
pwg_7 <- feature_representation(p8, pwg_s[, "solution_7"])
pwg_7$run <- "solution_7"
pwg_8 <- feature_representation(p8, pwg_s[, "solution_8"])
pwg_8$run <- "solution_8"
pwg_9 <- feature_representation(p8, pwg_s[, "solution_9"])
pwg_9$run <- "solution_9"
pwg_10 <- feature_representation(p8, pwg_s[, "solution_10"])
pwg_10$run <- "solution_10"
pwg_11 <- feature_representation(p8, pwg_s[, "solution_11"])
pwg_11$run <- "solution_11"
pwg_12 <- feature_representation(p8, pwg_s[, "solution_12"])
pwg_12$run <- "solution_12"
pwg_13 <- feature_representation(p8, pwg_s[, "solution_13"])
pwg_13$run <- "solution_13"
pwg_14 <- feature_representation(p8, pwg_s[, "solution_14"])
pwg_14$run <- "solution_14"
pwg_15 <- feature_representation(p8, pwg_s[, "solution_15"])
pwg_15$run <- "solution_15"
pwg_16 <- feature_representation(p8, pwg_s[, "solution_16"])
pwg_16$run <- "solution_16"
pwg_17 <- feature_representation(p8, pwg_s[, "solution_17"])
pwg_17$run <- "solution_17"
pwg_18 <- feature_representation(p8, pwg_s[, "solution_18"])
pwg_18$run <- "solution_18"
pwg_19 <- feature_representation(p8, pwg_s[, "solution_19"])
pwg_19$run <- "solution_19"
pwg_20 <- feature_representation(p8, pwg_s[, "solution_20"])
pwg_20$run <- "solution_20"
pwg_21 <- feature_representation(p8, pwg_s[, "solution_21"])
pwg_21$run <- "solution_21"
pwg_22 <- feature_representation(p8, pwg_s[, "solution_22"])
pwg_22$run <- "solution_22"
pwg_23 <- feature_representation(p8, pwg_s[, "solution_23"])
pwg_23$run <- "solution_23"
pwg_24 <- feature_representation(p8, pwg_s[, "solution_24"])
pwg_24$run <- "solution_24"
pwg_25 <- feature_representation(p8, pwg_s[, "solution_25"])
pwg_25$run <- "solution_25"
pwg_26 <- feature_representation(p8, pwg_s[, "solution_26"])
pwg_26$run <- "solution_26"
pwg_27 <- feature_representation(p8, pwg_s[, "solution_27"])
pwg_27$run <- "solution_27"
pwg_28 <- feature_representation(p8, pwg_s[, "solution_28"])
pwg_28$run <- "solution_28"
pwg_29 <- feature_representation(p8, pwg_s[, "solution_29"])
pwg_29$run <- "solution_29"
pwg_30 <- feature_representation(p8, pwg_s[, "solution_30"])
pwg_30$run <- "solution_30"
pwg_31 <- feature_representation(p8, pwg_s[, "solution_31"])
pwg_31$run <- "solution_31"
pwg_32 <- feature_representation(p8, pwg_s[, "solution_32"])
pwg_32$run <- "solution_32"
pwg_33 <- feature_representation(p8, pwg_s[, "solution_33"])
pwg_33$run <- "solution_33"
pwg_34 <- feature_representation(p8, pwg_s[, "solution_34"])
pwg_34$run <- "solution_34"
pwg_35 <- feature_representation(p8, pwg_s[, "solution_35"])
pwg_35$run <- "solution_35"
pwg_36 <- feature_representation(p8, pwg_s[, "solution_36"])
pwg_36$run <- "solution_36"
pwg_37 <- feature_representation(p8, pwg_s[, "solution_37"])
pwg_37$run <- "solution_37"
pwg_38 <- feature_representation(p8, pwg_s[, "solution_38"])
pwg_38$run <- "solution_38"
pwg_39 <- feature_representation(p8, pwg_s[, "solution_39"])
pwg_39$run <- "solution_39"
pwg_40 <- feature_representation(p8, pwg_s[, "solution_40"])
pwg_40$run <- "solution_40"
pwg_41 <- feature_representation(p8, pwg_s[, "solution_41"])
pwg_41$run <- "solution_41"
pwg_42 <- feature_representation(p8, pwg_s[, "solution_42"])
pwg_42$run <- "solution_42"
pwg_43 <- feature_representation(p8, pwg_s[, "solution_43"])
pwg_43$run <- "solution_43"
pwg_44 <- feature_representation(p8, pwg_s[, "solution_44"])
pwg_44$run <- "solution_44"
pwg_45 <- feature_representation(p8, pwg_s[, "solution_45"])
pwg_45$run <- "solution_45"
pwg_46 <- feature_representation(p8, pwg_s[, "solution_46"])
pwg_46$run <- "solution_46"
pwg_47 <- feature_representation(p8, pwg_s[, "solution_47"])
pwg_47$run <- "solution_47"
pwg_48 <- feature_representation(p8, pwg_s[, "solution_48"])
pwg_48$run <- "solution_48"
pwg_49 <- feature_representation(p8, pwg_s[, "solution_49"])
pwg_49$run <- "solution_49"
pwg_50 <- feature_representation(p8, pwg_s[, "solution_50"])
pwg_50$run <- "solution_50"
pwg_51 <- feature_representation(p8, pwg_s[, "solution_51"])
pwg_51$run <- "solution_51"
pwg_52 <- feature_representation(p8, pwg_s[, "solution_52"])
pwg_52$run <- "solution_52"
pwg_53 <- feature_representation(p8, pwg_s[, "solution_53"])
pwg_53$run <- "solution_53"
pwg_54 <- feature_representation(p8, pwg_s[, "solution_54"])
pwg_54$run <- "solution_54"
pwg_55 <- feature_representation(p8, pwg_s[, "solution_55"])
pwg_55$run <- "solution_55"
pwg_56 <- feature_representation(p8, pwg_s[, "solution_56"])
pwg_56$run <- "solution_56"
pwg_57 <- feature_representation(p8, pwg_s[, "solution_57"])
pwg_57$run <- "solution_57"
pwg_58 <- feature_representation(p8, pwg_s[, "solution_58"])
pwg_58$run <- "solution_58"
pwg_59 <- feature_representation(p8, pwg_s[, "solution_59"])
pwg_59$run <- "solution_59"
pwg_60 <- feature_representation(p8, pwg_s[, "solution_60"])
pwg_60$run <- "solution_60"
pwg_61 <- feature_representation(p8, pwg_s[, "solution_61"])
pwg_61$run <- "solution_61"
pwg_62 <- feature_representation(p8, pwg_s[, "solution_62"])
pwg_62$run <- "solution_62"
pwg_63 <- feature_representation(p8, pwg_s[, "solution_63"])
pwg_63$run <- "solution_63"
pwg_64 <- feature_representation(p8, pwg_s[, "solution_64"])
pwg_64$run <- "solution_64"
pwg_65 <- feature_representation(p8, pwg_s[, "solution_65"])
pwg_65$run <- "solution_65"
pwg_66 <- feature_representation(p8, pwg_s[, "solution_66"])
pwg_66$run <- "solution_66"
pwg_67 <- feature_representation(p8, pwg_s[, "solution_67"])
pwg_67$run <- "solution_67"
pwg_68 <- feature_representation(p8, pwg_s[, "solution_68"])
pwg_68$run <- "solution_68"
pwg_69 <- feature_representation(p8, pwg_s[, "solution_69"])
pwg_69$run <- "solution_69"
pwg_70 <- feature_representation(p8, pwg_s[, "solution_70"])
pwg_70$run <- "solution_70"
pwg_71 <- feature_representation(p8, pwg_s[, "solution_71"])
pwg_71$run <- "solution_71"
pwg_72 <- feature_representation(p8, pwg_s[, "solution_72"])
pwg_72$run <- "solution_72"
pwg_73 <- feature_representation(p8, pwg_s[, "solution_73"])
pwg_73$run <- "solution_73"
pwg_74 <- feature_representation(p8, pwg_s[, "solution_74"])
pwg_74$run <- "solution_74"
pwg_75 <- feature_representation(p8, pwg_s[, "solution_75"])
pwg_75$run <- "solution_75"
pwg_76 <- feature_representation(p8, pwg_s[, "solution_76"])
pwg_76$run <- "solution_76"
pwg_77 <- feature_representation(p8, pwg_s[, "solution_77"])
pwg_77$run <- "solution_77"
pwg_78 <- feature_representation(p8, pwg_s[, "solution_78"])
pwg_78$run <- "solution_78"
pwg_79 <- feature_representation(p8, pwg_s[, "solution_79"])
pwg_79$run <- "solution_79"
pwg_80 <- feature_representation(p8, pwg_s[, "solution_80"])
pwg_80$run <- "solution_80"
pwg_81 <- feature_representation(p8, pwg_s[, "solution_81"])
pwg_81$run <- "solution_81"
pwg_82 <- feature_representation(p8, pwg_s[, "solution_82"])
pwg_82$run <- "solution_82"
pwg_83 <- feature_representation(p8, pwg_s[, "solution_83"])
pwg_83$run <- "solution_83"
pwg_84 <- feature_representation(p8, pwg_s[, "solution_84"])
pwg_84$run <- "solution_84"
pwg_85 <- feature_representation(p8, pwg_s[, "solution_85"])
pwg_85$run <- "solution_85"
pwg_86 <- feature_representation(p8, pwg_s[, "solution_86"])
pwg_86$run <- "solution_86"
pwg_87 <- feature_representation(p8, pwg_s[, "solution_87"])
pwg_87$run <- "solution_87"
pwg_88 <- feature_representation(p8, pwg_s[, "solution_88"])
pwg_88$run <- "solution_88"
pwg_89 <- feature_representation(p8, pwg_s[, "solution_89"])
pwg_89$run <- "solution_89"
pwg_90 <- feature_representation(p8, pwg_s[, "solution_90"])
pwg_90$run <- "solution_90"
pwg_91 <- feature_representation(p8, pwg_s[, "solution_91"])
pwg_91$run <- "solution_91"
pwg_92 <- feature_representation(p8, pwg_s[, "solution_92"])
pwg_92$run <- "solution_92"
pwg_93 <- feature_representation(p8, pwg_s[, "solution_93"])
pwg_93$run <- "solution_93"
pwg_94 <- feature_representation(p8, pwg_s[, "solution_94"])
pwg_94$run <- "solution_94"
pwg_95 <- feature_representation(p8, pwg_s[, "solution_95"])
pwg_95$run <- "solution_95"
pwg_96 <- feature_representation(p8, pwg_s[, "solution_96"])
pwg_96$run <- "solution_96"
pwg_97 <- feature_representation(p8, pwg_s[, "solution_97"])
pwg_97$run <- "solution_97"
pwg_98 <- feature_representation(p8, pwg_s[, "solution_98"])
pwg_98$run <- "solution_98"
pwg_99 <- feature_representation(p8, pwg_s[, "solution_99"])
pwg_99$run <- "solution_99"
pwg_100 <- feature_representation(p8, pwg_s[, "solution_100"])
pwg_100$run <- "solution_100"
pwg_r<-bind_rows(pwg_1,pwg_2,pwg_3,pwg_4,pwg_5,pwg_6,pwg_7,pwg_8,pwg_9,pwg_10,pwg_11,pwg_12,pwg_13,pwg_14,pwg_15,pwg_16,pwg_17,pwg_18,pwg_19,pwg_20,pwg_21,pwg_22,pwg_23,pwg_24,pwg_25,pwg_26,pwg_27,pwg_28,pwg_29,pwg_30,pwg_31,pwg_32,pwg_33,pwg_34,pwg_35,pwg_36,pwg_37,pwg_38,pwg_39,pwg_40,pwg_41,pwg_42,pwg_43,pwg_44,pwg_45,pwg_46,pwg_47,pwg_48,pwg_49,pwg_50,pwg_51,pwg_52,pwg_53,pwg_54,pwg_55,pwg_56,pwg_57,pwg_58,pwg_59,pwg_60,pwg_61,pwg_62,pwg_63,pwg_64,pwg_65,pwg_66,pwg_67,pwg_68,pwg_69,pwg_70,pwg_71,pwg_72,pwg_73,pwg_74,pwg_75,pwg_76,pwg_77,pwg_78,pwg_79,pwg_80,pwg_81,pwg_82,pwg_83,pwg_84,pwg_85,pwg_86,pwg_87,pwg_88,pwg_89,pwg_90,pwg_91,pwg_92,pwg_93,pwg_94,pwg_95,pwg_96,pwg_97,pwg_98,pwg_99,pwg_100)
pwg_r$objective <- "Pairwise Butterfly Div Genus (n=119)"
pwg_r$species<-"119"


feature_a <- feature_abundances(p8, na.rm = FALSE)

pwgr<-left_join(pwg_r,feature_a)
View(pwgr)

# Master Feature Dataset for Objectives
objective.features <- bind_rows(compr,bflyr,birdr,domr,pdivfr,pdivgr,pwfr,pwgr)
write.csv(objective.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/objective.features.csv")



# Random
#5
pu_dat5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r5 <- problem(pu_dat5, spec_dat5, cost_column = "cost", rij = puvsp_dat5) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat5$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr5 <- solve(r5)

sr5<-as.data.frame(sr5) %>% as_tibble()

sr5$objective<-"Random"
sr5$solution<-"5"

# Features Dataset
sr5_1 <- feature_representation(r5, sr5[, "solution_1"])
sr5_1$run <- "solution_1"
sr5_2 <- feature_representation(r5, sr5[, "solution_2"])
sr5_2$run <- "solution_2"
sr5_3 <- feature_representation(r5, sr5[, "solution_3"])
sr5_3$run <- "solution_3"
sr5_4 <- feature_representation(r5, sr5[, "solution_4"])
sr5_4$run <- "solution_4"
sr5_5 <- feature_representation(r5, sr5[, "solution_5"])
sr5_5$run <- "solution_5"
sr5_6 <- feature_representation(r5, sr5[, "solution_6"])
sr5_6$run <- "solution_6"
sr5_7 <- feature_representation(r5, sr5[, "solution_7"])
sr5_7$run <- "solution_7"
sr5_8 <- feature_representation(r5, sr5[, "solution_8"])
sr5_8$run <- "solution_8"
sr5_9 <- feature_representation(r5, sr5[, "solution_9"])
sr5_9$run <- "solution_9"
sr5_10 <- feature_representation(r5, sr5[, "solution_10"])
sr5_10$run <- "solution_10"
sr5_11 <- feature_representation(r5, sr5[, "solution_11"])
sr5_11$run <- "solution_11"
sr5_12 <- feature_representation(r5, sr5[, "solution_12"])
sr5_12$run <- "solution_12"
sr5_13 <- feature_representation(r5, sr5[, "solution_13"])
sr5_13$run <- "solution_13"
sr5_14 <- feature_representation(r5, sr5[, "solution_14"])
sr5_14$run <- "solution_14"
sr5_15 <- feature_representation(r5, sr5[, "solution_15"])
sr5_15$run <- "solution_15"
sr5_16 <- feature_representation(r5, sr5[, "solution_16"])
sr5_16$run <- "solution_16"
sr5_17 <- feature_representation(r5, sr5[, "solution_17"])
sr5_17$run <- "solution_17"
sr5_18 <- feature_representation(r5, sr5[, "solution_18"])
sr5_18$run <- "solution_18"
sr5_19 <- feature_representation(r5, sr5[, "solution_19"])
sr5_19$run <- "solution_19"
sr5_20 <- feature_representation(r5, sr5[, "solution_20"])
sr5_20$run <- "solution_20"
sr5_21 <- feature_representation(r5, sr5[, "solution_21"])
sr5_21$run <- "solution_21"
sr5_22 <- feature_representation(r5, sr5[, "solution_22"])
sr5_22$run <- "solution_22"
sr5_23 <- feature_representation(r5, sr5[, "solution_23"])
sr5_23$run <- "solution_23"
sr5_24 <- feature_representation(r5, sr5[, "solution_24"])
sr5_24$run <- "solution_24"
sr5_25 <- feature_representation(r5, sr5[, "solution_25"])
sr5_25$run <- "solution_25"
sr5_26 <- feature_representation(r5, sr5[, "solution_26"])
sr5_26$run <- "solution_26"
sr5_27 <- feature_representation(r5, sr5[, "solution_27"])
sr5_27$run <- "solution_27"
sr5_28 <- feature_representation(r5, sr5[, "solution_28"])
sr5_28$run <- "solution_28"
sr5_29 <- feature_representation(r5, sr5[, "solution_29"])
sr5_29$run <- "solution_29"
sr5_30 <- feature_representation(r5, sr5[, "solution_30"])
sr5_30$run <- "solution_30"
sr5_31 <- feature_representation(r5, sr5[, "solution_31"])
sr5_31$run <- "solution_31"
sr5_32 <- feature_representation(r5, sr5[, "solution_32"])
sr5_32$run <- "solution_32"
sr5_33 <- feature_representation(r5, sr5[, "solution_33"])
sr5_33$run <- "solution_33"
sr5_34 <- feature_representation(r5, sr5[, "solution_34"])
sr5_34$run <- "solution_34"
sr5_35 <- feature_representation(r5, sr5[, "solution_35"])
sr5_35$run <- "solution_35"
sr5_36 <- feature_representation(r5, sr5[, "solution_36"])
sr5_36$run <- "solution_36"
sr5_37 <- feature_representation(r5, sr5[, "solution_37"])
sr5_37$run <- "solution_37"
sr5_38 <- feature_representation(r5, sr5[, "solution_38"])
sr5_38$run <- "solution_38"
sr5_39 <- feature_representation(r5, sr5[, "solution_39"])
sr5_39$run <- "solution_39"
sr5_40 <- feature_representation(r5, sr5[, "solution_40"])
sr5_40$run <- "solution_40"
sr5_41 <- feature_representation(r5, sr5[, "solution_41"])
sr5_41$run <- "solution_41"
sr5_42 <- feature_representation(r5, sr5[, "solution_42"])
sr5_42$run <- "solution_42"
sr5_43 <- feature_representation(r5, sr5[, "solution_43"])
sr5_43$run <- "solution_43"
sr5_44 <- feature_representation(r5, sr5[, "solution_44"])
sr5_44$run <- "solution_44"
sr5_45 <- feature_representation(r5, sr5[, "solution_45"])
sr5_45$run <- "solution_45"
sr5_46 <- feature_representation(r5, sr5[, "solution_46"])
sr5_46$run <- "solution_46"
sr5_47 <- feature_representation(r5, sr5[, "solution_47"])
sr5_47$run <- "solution_47"
sr5_48 <- feature_representation(r5, sr5[, "solution_48"])
sr5_48$run <- "solution_48"
sr5_49 <- feature_representation(r5, sr5[, "solution_49"])
sr5_49$run <- "solution_49"
sr5_50 <- feature_representation(r5, sr5[, "solution_50"])
sr5_50$run <- "solution_50"
sr5_51 <- feature_representation(r5, sr5[, "solution_51"])
sr5_51$run <- "solution_51"
sr5_52 <- feature_representation(r5, sr5[, "solution_52"])
sr5_52$run <- "solution_52"
sr5_53 <- feature_representation(r5, sr5[, "solution_53"])
sr5_53$run <- "solution_53"
sr5_54 <- feature_representation(r5, sr5[, "solution_54"])
sr5_54$run <- "solution_54"
sr5_55 <- feature_representation(r5, sr5[, "solution_55"])
sr5_55$run <- "solution_55"
sr5_56 <- feature_representation(r5, sr5[, "solution_56"])
sr5_56$run <- "solution_56"
sr5_57 <- feature_representation(r5, sr5[, "solution_57"])
sr5_57$run <- "solution_57"
sr5_58 <- feature_representation(r5, sr5[, "solution_58"])
sr5_58$run <- "solution_58"
sr5_59 <- feature_representation(r5, sr5[, "solution_59"])
sr5_59$run <- "solution_59"
sr5_60 <- feature_representation(r5, sr5[, "solution_60"])
sr5_60$run <- "solution_60"
sr5_61 <- feature_representation(r5, sr5[, "solution_61"])
sr5_61$run <- "solution_61"
sr5_62 <- feature_representation(r5, sr5[, "solution_62"])
sr5_62$run <- "solution_62"
sr5_63 <- feature_representation(r5, sr5[, "solution_63"])
sr5_63$run <- "solution_63"
sr5_64 <- feature_representation(r5, sr5[, "solution_64"])
sr5_64$run <- "solution_64"
sr5_65 <- feature_representation(r5, sr5[, "solution_65"])
sr5_65$run <- "solution_65"
sr5_66 <- feature_representation(r5, sr5[, "solution_66"])
sr5_66$run <- "solution_66"
sr5_67 <- feature_representation(r5, sr5[, "solution_67"])
sr5_67$run <- "solution_67"
sr5_68 <- feature_representation(r5, sr5[, "solution_68"])
sr5_68$run <- "solution_68"
sr5_69 <- feature_representation(r5, sr5[, "solution_69"])
sr5_69$run <- "solution_69"
sr5_70 <- feature_representation(r5, sr5[, "solution_70"])
sr5_70$run <- "solution_70"
sr5_71 <- feature_representation(r5, sr5[, "solution_71"])
sr5_71$run <- "solution_71"
sr5_72 <- feature_representation(r5, sr5[, "solution_72"])
sr5_72$run <- "solution_72"
sr5_73 <- feature_representation(r5, sr5[, "solution_73"])
sr5_73$run <- "solution_73"
sr5_74 <- feature_representation(r5, sr5[, "solution_74"])
sr5_74$run <- "solution_74"
sr5_75 <- feature_representation(r5, sr5[, "solution_75"])
sr5_75$run <- "solution_75"
sr5_76 <- feature_representation(r5, sr5[, "solution_76"])
sr5_76$run <- "solution_76"
sr5_77 <- feature_representation(r5, sr5[, "solution_77"])
sr5_77$run <- "solution_77"
sr5_78 <- feature_representation(r5, sr5[, "solution_78"])
sr5_78$run <- "solution_78"
sr5_79 <- feature_representation(r5, sr5[, "solution_79"])
sr5_79$run <- "solution_79"
sr5_80 <- feature_representation(r5, sr5[, "solution_80"])
sr5_80$run <- "solution_80"
sr5_81 <- feature_representation(r5, sr5[, "solution_81"])
sr5_81$run <- "solution_81"
sr5_82 <- feature_representation(r5, sr5[, "solution_82"])
sr5_82$run <- "solution_82"
sr5_83 <- feature_representation(r5, sr5[, "solution_83"])
sr5_83$run <- "solution_83"
sr5_84 <- feature_representation(r5, sr5[, "solution_84"])
sr5_84$run <- "solution_84"
sr5_85 <- feature_representation(r5, sr5[, "solution_85"])
sr5_85$run <- "solution_85"
sr5_86 <- feature_representation(r5, sr5[, "solution_86"])
sr5_86$run <- "solution_86"
sr5_87 <- feature_representation(r5, sr5[, "solution_87"])
sr5_87$run <- "solution_87"
sr5_88 <- feature_representation(r5, sr5[, "solution_88"])
sr5_88$run <- "solution_88"
sr5_89 <- feature_representation(r5, sr5[, "solution_89"])
sr5_89$run <- "solution_89"
sr5_90 <- feature_representation(r5, sr5[, "solution_90"])
sr5_90$run <- "solution_90"
sr5_91 <- feature_representation(r5, sr5[, "solution_91"])
sr5_91$run <- "solution_91"
sr5_92 <- feature_representation(r5, sr5[, "solution_92"])
sr5_92$run <- "solution_92"
sr5_93 <- feature_representation(r5, sr5[, "solution_93"])
sr5_93$run <- "solution_93"
sr5_94 <- feature_representation(r5, sr5[, "solution_94"])
sr5_94$run <- "solution_94"
sr5_95 <- feature_representation(r5, sr5[, "solution_95"])
sr5_95$run <- "solution_95"
sr5_96 <- feature_representation(r5, sr5[, "solution_96"])
sr5_96$run <- "solution_96"
sr5_97 <- feature_representation(r5, sr5[, "solution_97"])
sr5_97$run <- "solution_97"
sr5_98 <- feature_representation(r5, sr5[, "solution_98"])
sr5_98$run <- "solution_98"
sr5_99 <- feature_representation(r5, sr5[, "solution_99"])
sr5_99$run <- "solution_99"
sr5_100 <- feature_representation(r5, sr5[, "solution_100"])
sr5_100$run <- "solution_100"
sr5_r<-bind_rows(sr5_1,sr5_2,sr5_3,sr5_4,sr5_5,sr5_6,sr5_7,sr5_8,sr5_9,sr5_10,sr5_11,sr5_12,sr5_13,sr5_14,sr5_15,sr5_16,sr5_17,sr5_18,sr5_19,sr5_20,sr5_21,sr5_22,sr5_23,sr5_24,sr5_25,sr5_26,sr5_27,sr5_28,sr5_29,sr5_30,sr5_31,sr5_32,sr5_33,sr5_34,sr5_35,sr5_36,sr5_37,sr5_38,sr5_39,sr5_40,sr5_41,sr5_42,sr5_43,sr5_44,sr5_45,sr5_46,sr5_47,sr5_48,sr5_49,sr5_50,sr5_51,sr5_52,sr5_53,sr5_54,sr5_55,sr5_56,sr5_57,sr5_58,sr5_59,sr5_60,sr5_61,sr5_62,sr5_63,sr5_64,sr5_65,sr5_66,sr5_67,sr5_68,sr5_69,sr5_70,sr5_71,sr5_72,sr5_73,sr5_74,sr5_75,sr5_76,sr5_77,sr5_78,sr5_79,sr5_80,sr5_81,sr5_82,sr5_83,sr5_84,sr5_85,sr5_86,sr5_87,sr5_88,sr5_89,sr5_90,sr5_91,sr5_92,sr5_93,sr5_94,sr5_95,sr5_96,sr5_97,sr5_98,sr5_99,sr5_100)
sr5_r$objective <- "Random"
sr5_r$species<-"5"


feature_a <- feature_abundances(r5, na.rm = FALSE)

sr5r<-left_join(sr5_r,feature_a)
View(sr5r)


#10
pu_dat10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r10 <- problem(pu_dat10, spec_dat10, cost_column = "cost", rij = puvsp_dat10) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat10$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  #add_pool_portfolio(method = 2, number_solutions = 100) # return 100 solutions
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # or no duplicates

sr10 <- solve(r10)

sr10<-as.data.frame(sr10) %>% as_tibble()

sr10$objective<-"Random"
sr10$solution<-"10"

View(sr10)

# Features Dataset
sr10_1 <- feature_representation(r10, sr10[, "solution_1"])
sr10_1$run <- "solution_1"
sr10_2 <- feature_representation(r10, sr10[, "solution_2"])
sr10_2$run <- "solution_2"
sr10_3 <- feature_representation(r10, sr10[, "solution_3"])
sr10_3$run <- "solution_3"
sr10_4 <- feature_representation(r10, sr10[, "solution_4"])
sr10_4$run <- "solution_4"
sr10_5 <- feature_representation(r10, sr10[, "solution_5"])
sr10_5$run <- "solution_5"
sr10_6 <- feature_representation(r10, sr10[, "solution_6"])
sr10_6$run <- "solution_6"
sr10_7 <- feature_representation(r10, sr10[, "solution_7"])
sr10_7$run <- "solution_7"
sr10_8 <- feature_representation(r10, sr10[, "solution_8"])
sr10_8$run <- "solution_8"
sr10_9 <- feature_representation(r10, sr10[, "solution_9"])
sr10_9$run <- "solution_9"
sr10_10 <- feature_representation(r10, sr10[, "solution_10"])
sr10_10$run <- "solution_10"
sr10_11 <- feature_representation(r10, sr10[, "solution_11"])
sr10_11$run <- "solution_11"
sr10_12 <- feature_representation(r10, sr10[, "solution_12"])
sr10_12$run <- "solution_12"
sr10_13 <- feature_representation(r10, sr10[, "solution_13"])
sr10_13$run <- "solution_13"
sr10_14 <- feature_representation(r10, sr10[, "solution_14"])
sr10_14$run <- "solution_14"
sr10_15 <- feature_representation(r10, sr10[, "solution_15"])
sr10_15$run <- "solution_15"
sr10_16 <- feature_representation(r10, sr10[, "solution_16"])
sr10_16$run <- "solution_16"
sr10_17 <- feature_representation(r10, sr10[, "solution_17"])
sr10_17$run <- "solution_17"
sr10_18 <- feature_representation(r10, sr10[, "solution_18"])
sr10_18$run <- "solution_18"
sr10_19 <- feature_representation(r10, sr10[, "solution_19"])
sr10_19$run <- "solution_19"
sr10_20 <- feature_representation(r10, sr10[, "solution_20"])
sr10_20$run <- "solution_20"
sr10_21 <- feature_representation(r10, sr10[, "solution_21"])
sr10_21$run <- "solution_21"
sr10_22 <- feature_representation(r10, sr10[, "solution_22"])
sr10_22$run <- "solution_22"
sr10_23 <- feature_representation(r10, sr10[, "solution_23"])
sr10_23$run <- "solution_23"
sr10_24 <- feature_representation(r10, sr10[, "solution_24"])
sr10_24$run <- "solution_24"
sr10_25 <- feature_representation(r10, sr10[, "solution_25"])
sr10_25$run <- "solution_25"
sr10_26 <- feature_representation(r10, sr10[, "solution_26"])
sr10_26$run <- "solution_26"
sr10_27 <- feature_representation(r10, sr10[, "solution_27"])
sr10_27$run <- "solution_27"
sr10_28 <- feature_representation(r10, sr10[, "solution_28"])
sr10_28$run <- "solution_28"
sr10_29 <- feature_representation(r10, sr10[, "solution_29"])
sr10_29$run <- "solution_29"
sr10_30 <- feature_representation(r10, sr10[, "solution_30"])
sr10_30$run <- "solution_30"
sr10_31 <- feature_representation(r10, sr10[, "solution_31"])
sr10_31$run <- "solution_31"
sr10_32 <- feature_representation(r10, sr10[, "solution_32"])
sr10_32$run <- "solution_32"
sr10_33 <- feature_representation(r10, sr10[, "solution_33"])
sr10_33$run <- "solution_33"
sr10_34 <- feature_representation(r10, sr10[, "solution_34"])
sr10_34$run <- "solution_34"
sr10_35 <- feature_representation(r10, sr10[, "solution_35"])
sr10_35$run <- "solution_35"
sr10_36 <- feature_representation(r10, sr10[, "solution_36"])
sr10_36$run <- "solution_36"
sr10_37 <- feature_representation(r10, sr10[, "solution_37"])
sr10_37$run <- "solution_37"
sr10_38 <- feature_representation(r10, sr10[, "solution_38"])
sr10_38$run <- "solution_38"
sr10_39 <- feature_representation(r10, sr10[, "solution_39"])
sr10_39$run <- "solution_39"
sr10_40 <- feature_representation(r10, sr10[, "solution_40"])
sr10_40$run <- "solution_40"
sr10_41 <- feature_representation(r10, sr10[, "solution_41"])
sr10_41$run <- "solution_41"
sr10_42 <- feature_representation(r10, sr10[, "solution_42"])
sr10_42$run <- "solution_42"
sr10_43 <- feature_representation(r10, sr10[, "solution_43"])
sr10_43$run <- "solution_43"
sr10_44 <- feature_representation(r10, sr10[, "solution_44"])
sr10_44$run <- "solution_44"
sr10_45 <- feature_representation(r10, sr10[, "solution_45"])
sr10_45$run <- "solution_45"
sr10_46 <- feature_representation(r10, sr10[, "solution_46"])
sr10_46$run <- "solution_46"
sr10_47 <- feature_representation(r10, sr10[, "solution_47"])
sr10_47$run <- "solution_47"
sr10_48 <- feature_representation(r10, sr10[, "solution_48"])
sr10_48$run <- "solution_48"
sr10_49 <- feature_representation(r10, sr10[, "solution_49"])
sr10_49$run <- "solution_49"
sr10_50 <- feature_representation(r10, sr10[, "solution_50"])
sr10_50$run <- "solution_50"
sr10_51 <- feature_representation(r10, sr10[, "solution_51"])
sr10_51$run <- "solution_51"
sr10_52 <- feature_representation(r10, sr10[, "solution_52"])
sr10_52$run <- "solution_52"
sr10_53 <- feature_representation(r10, sr10[, "solution_53"])
sr10_53$run <- "solution_53"
sr10_54 <- feature_representation(r10, sr10[, "solution_54"])
sr10_54$run <- "solution_54"
sr10_55 <- feature_representation(r10, sr10[, "solution_55"])
sr10_55$run <- "solution_55"
sr10_56 <- feature_representation(r10, sr10[, "solution_56"])
sr10_56$run <- "solution_56"
sr10_57 <- feature_representation(r10, sr10[, "solution_57"])
sr10_57$run <- "solution_57"
sr10_58 <- feature_representation(r10, sr10[, "solution_58"])
sr10_58$run <- "solution_58"
sr10_59 <- feature_representation(r10, sr10[, "solution_59"])
sr10_59$run <- "solution_59"
sr10_60 <- feature_representation(r10, sr10[, "solution_60"])
sr10_60$run <- "solution_60"
sr10_61 <- feature_representation(r10, sr10[, "solution_61"])
sr10_61$run <- "solution_61"
sr10_62 <- feature_representation(r10, sr10[, "solution_62"])
sr10_62$run <- "solution_62"
sr10_63 <- feature_representation(r10, sr10[, "solution_63"])
sr10_63$run <- "solution_63"
sr10_64 <- feature_representation(r10, sr10[, "solution_64"])
sr10_64$run <- "solution_64"
sr10_65 <- feature_representation(r10, sr10[, "solution_65"])
sr10_65$run <- "solution_65"
sr10_66 <- feature_representation(r10, sr10[, "solution_66"])
sr10_66$run <- "solution_66"
sr10_67 <- feature_representation(r10, sr10[, "solution_67"])
sr10_67$run <- "solution_67"
sr10_68 <- feature_representation(r10, sr10[, "solution_68"])
sr10_68$run <- "solution_68"
sr10_69 <- feature_representation(r10, sr10[, "solution_69"])
sr10_69$run <- "solution_69"
sr10_70 <- feature_representation(r10, sr10[, "solution_70"])
sr10_70$run <- "solution_70"
sr10_71 <- feature_representation(r10, sr10[, "solution_71"])
sr10_71$run <- "solution_71"
sr10_72 <- feature_representation(r10, sr10[, "solution_72"])
sr10_72$run <- "solution_72"
sr10_73 <- feature_representation(r10, sr10[, "solution_73"])
sr10_73$run <- "solution_73"
sr10_74 <- feature_representation(r10, sr10[, "solution_74"])
sr10_74$run <- "solution_74"
sr10_75 <- feature_representation(r10, sr10[, "solution_75"])
sr10_75$run <- "solution_75"
sr10_76 <- feature_representation(r10, sr10[, "solution_76"])
sr10_76$run <- "solution_76"
sr10_77 <- feature_representation(r10, sr10[, "solution_77"])
sr10_77$run <- "solution_77"
sr10_78 <- feature_representation(r10, sr10[, "solution_78"])
sr10_78$run <- "solution_78"
sr10_79 <- feature_representation(r10, sr10[, "solution_79"])
sr10_79$run <- "solution_79"
sr10_80 <- feature_representation(r10, sr10[, "solution_80"])
sr10_80$run <- "solution_80"
sr10_81 <- feature_representation(r10, sr10[, "solution_81"])
sr10_81$run <- "solution_81"
sr10_82 <- feature_representation(r10, sr10[, "solution_82"])
sr10_82$run <- "solution_82"
sr10_83 <- feature_representation(r10, sr10[, "solution_83"])
sr10_83$run <- "solution_83"
sr10_84 <- feature_representation(r10, sr10[, "solution_84"])
sr10_84$run <- "solution_84"
sr10_85 <- feature_representation(r10, sr10[, "solution_85"])
sr10_85$run <- "solution_85"
sr10_86 <- feature_representation(r10, sr10[, "solution_86"])
sr10_86$run <- "solution_86"
sr10_87 <- feature_representation(r10, sr10[, "solution_87"])
sr10_87$run <- "solution_87"
sr10_88 <- feature_representation(r10, sr10[, "solution_88"])
sr10_88$run <- "solution_88"
sr10_89 <- feature_representation(r10, sr10[, "solution_89"])
sr10_89$run <- "solution_89"
sr10_90 <- feature_representation(r10, sr10[, "solution_90"])
sr10_90$run <- "solution_90"
sr10_91 <- feature_representation(r10, sr10[, "solution_91"])
sr10_91$run <- "solution_91"
sr10_92 <- feature_representation(r10, sr10[, "solution_92"])
sr10_92$run <- "solution_92"
sr10_93 <- feature_representation(r10, sr10[, "solution_93"])
sr10_93$run <- "solution_93"
sr10_94 <- feature_representation(r10, sr10[, "solution_94"])
sr10_94$run <- "solution_94"
sr10_95 <- feature_representation(r10, sr10[, "solution_95"])
sr10_95$run <- "solution_95"
sr10_96 <- feature_representation(r10, sr10[, "solution_96"])
sr10_96$run <- "solution_96"
sr10_97 <- feature_representation(r10, sr10[, "solution_97"])
sr10_97$run <- "solution_97"
sr10_98 <- feature_representation(r10, sr10[, "solution_98"])
sr10_98$run <- "solution_98"
sr10_99 <- feature_representation(r10, sr10[, "solution_99"])
sr10_99$run <- "solution_99"
sr10_100 <- feature_representation(r10, sr10[, "solution_100"])
sr10_100$run <- "solution_100"
sr10_r<-bind_rows(sr10_1,sr10_2,sr10_3,sr10_4,sr10_5,sr10_6,sr10_7,sr10_8,sr10_9,sr10_10,sr10_11,sr10_12,sr10_13,sr10_14,sr10_15,sr10_16,sr10_17,sr10_18,sr10_19,sr10_20,sr10_21,sr10_22,sr10_23,sr10_24,sr10_25,sr10_26,sr10_27,sr10_28,sr10_29,sr10_30,sr10_31,sr10_32,sr10_33,sr10_34,sr10_35,sr10_36,sr10_37,sr10_38,sr10_39,sr10_40,sr10_41,sr10_42,sr10_43,sr10_44,sr10_45,sr10_46,sr10_47,sr10_48,sr10_49,sr10_50,sr10_51,sr10_52,sr10_53,sr10_54,sr10_55,sr10_56,sr10_57,sr10_58,sr10_59,sr10_60,sr10_61,sr10_62,sr10_63,sr10_64,sr10_65,sr10_66,sr10_67,sr10_68,sr10_69,sr10_70,sr10_71,sr10_72,sr10_73,sr10_74,sr10_75,sr10_76,sr10_77,sr10_78,sr10_79,sr10_80,sr10_81,sr10_82,sr10_83,sr10_84,sr10_85,sr10_86,sr10_87,sr10_88,sr10_89,sr10_90,sr10_91,sr10_92,sr10_93,sr10_94,sr10_95,sr10_96,sr10_97,sr10_98,sr10_99,sr10_100)
sr10_r$objective <- "Random"
sr10_r$species<-"10"


feature_a <- feature_abundances(r10, na.rm = FALSE)

sr10r<-left_join(sr10_r,feature_a)

View(sr10r)
#15
pu_dat15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r15 <- problem(pu_dat15, spec_dat15, cost_column = "cost", rij = puvsp_dat15) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat15$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr15 <- solve(r15)

sr15<-as.data.frame(sr15) %>% as_tibble()

sr15$objective<-"Random"
sr15$solution<-"15"

# Features Dataset
sr15_1 <- feature_representation(r15, sr15[, "solution_1"])
sr15_1$run <- "solution_1"
sr15_2 <- feature_representation(r15, sr15[, "solution_2"])
sr15_2$run <- "solution_2"
sr15_3 <- feature_representation(r15, sr15[, "solution_3"])
sr15_3$run <- "solution_3"
sr15_4 <- feature_representation(r15, sr15[, "solution_4"])
sr15_4$run <- "solution_4"
sr15_5 <- feature_representation(r15, sr15[, "solution_5"])
sr15_5$run <- "solution_5"
sr15_6 <- feature_representation(r15, sr15[, "solution_6"])
sr15_6$run <- "solution_6"
sr15_7 <- feature_representation(r15, sr15[, "solution_7"])
sr15_7$run <- "solution_7"
sr15_8 <- feature_representation(r15, sr15[, "solution_8"])
sr15_8$run <- "solution_8"
sr15_9 <- feature_representation(r15, sr15[, "solution_9"])
sr15_9$run <- "solution_9"
sr15_10 <- feature_representation(r15, sr15[, "solution_10"])
sr15_10$run <- "solution_10"
sr15_11 <- feature_representation(r15, sr15[, "solution_11"])
sr15_11$run <- "solution_11"
sr15_12 <- feature_representation(r15, sr15[, "solution_12"])
sr15_12$run <- "solution_12"
sr15_13 <- feature_representation(r15, sr15[, "solution_13"])
sr15_13$run <- "solution_13"
sr15_14 <- feature_representation(r15, sr15[, "solution_14"])
sr15_14$run <- "solution_14"
sr15_15 <- feature_representation(r15, sr15[, "solution_15"])
sr15_15$run <- "solution_15"
sr15_16 <- feature_representation(r15, sr15[, "solution_16"])
sr15_16$run <- "solution_16"
sr15_17 <- feature_representation(r15, sr15[, "solution_17"])
sr15_17$run <- "solution_17"
sr15_18 <- feature_representation(r15, sr15[, "solution_18"])
sr15_18$run <- "solution_18"
sr15_19 <- feature_representation(r15, sr15[, "solution_19"])
sr15_19$run <- "solution_19"
sr15_20 <- feature_representation(r15, sr15[, "solution_20"])
sr15_20$run <- "solution_20"
sr15_21 <- feature_representation(r15, sr15[, "solution_21"])
sr15_21$run <- "solution_21"
sr15_22 <- feature_representation(r15, sr15[, "solution_22"])
sr15_22$run <- "solution_22"
sr15_23 <- feature_representation(r15, sr15[, "solution_23"])
sr15_23$run <- "solution_23"
sr15_24 <- feature_representation(r15, sr15[, "solution_24"])
sr15_24$run <- "solution_24"
sr15_25 <- feature_representation(r15, sr15[, "solution_25"])
sr15_25$run <- "solution_25"
sr15_26 <- feature_representation(r15, sr15[, "solution_26"])
sr15_26$run <- "solution_26"
sr15_27 <- feature_representation(r15, sr15[, "solution_27"])
sr15_27$run <- "solution_27"
sr15_28 <- feature_representation(r15, sr15[, "solution_28"])
sr15_28$run <- "solution_28"
sr15_29 <- feature_representation(r15, sr15[, "solution_29"])
sr15_29$run <- "solution_29"
sr15_30 <- feature_representation(r15, sr15[, "solution_30"])
sr15_30$run <- "solution_30"
sr15_31 <- feature_representation(r15, sr15[, "solution_31"])
sr15_31$run <- "solution_31"
sr15_32 <- feature_representation(r15, sr15[, "solution_32"])
sr15_32$run <- "solution_32"
sr15_33 <- feature_representation(r15, sr15[, "solution_33"])
sr15_33$run <- "solution_33"
sr15_34 <- feature_representation(r15, sr15[, "solution_34"])
sr15_34$run <- "solution_34"
sr15_35 <- feature_representation(r15, sr15[, "solution_35"])
sr15_35$run <- "solution_35"
sr15_36 <- feature_representation(r15, sr15[, "solution_36"])
sr15_36$run <- "solution_36"
sr15_37 <- feature_representation(r15, sr15[, "solution_37"])
sr15_37$run <- "solution_37"
sr15_38 <- feature_representation(r15, sr15[, "solution_38"])
sr15_38$run <- "solution_38"
sr15_39 <- feature_representation(r15, sr15[, "solution_39"])
sr15_39$run <- "solution_39"
sr15_40 <- feature_representation(r15, sr15[, "solution_40"])
sr15_40$run <- "solution_40"
sr15_41 <- feature_representation(r15, sr15[, "solution_41"])
sr15_41$run <- "solution_41"
sr15_42 <- feature_representation(r15, sr15[, "solution_42"])
sr15_42$run <- "solution_42"
sr15_43 <- feature_representation(r15, sr15[, "solution_43"])
sr15_43$run <- "solution_43"
sr15_44 <- feature_representation(r15, sr15[, "solution_44"])
sr15_44$run <- "solution_44"
sr15_45 <- feature_representation(r15, sr15[, "solution_45"])
sr15_45$run <- "solution_45"
sr15_46 <- feature_representation(r15, sr15[, "solution_46"])
sr15_46$run <- "solution_46"
sr15_47 <- feature_representation(r15, sr15[, "solution_47"])
sr15_47$run <- "solution_47"
sr15_48 <- feature_representation(r15, sr15[, "solution_48"])
sr15_48$run <- "solution_48"
sr15_49 <- feature_representation(r15, sr15[, "solution_49"])
sr15_49$run <- "solution_49"
sr15_50 <- feature_representation(r15, sr15[, "solution_50"])
sr15_50$run <- "solution_50"
sr15_51 <- feature_representation(r15, sr15[, "solution_51"])
sr15_51$run <- "solution_51"
sr15_52 <- feature_representation(r15, sr15[, "solution_52"])
sr15_52$run <- "solution_52"
sr15_53 <- feature_representation(r15, sr15[, "solution_53"])
sr15_53$run <- "solution_53"
sr15_54 <- feature_representation(r15, sr15[, "solution_54"])
sr15_54$run <- "solution_54"
sr15_55 <- feature_representation(r15, sr15[, "solution_55"])
sr15_55$run <- "solution_55"
sr15_56 <- feature_representation(r15, sr15[, "solution_56"])
sr15_56$run <- "solution_56"
sr15_57 <- feature_representation(r15, sr15[, "solution_57"])
sr15_57$run <- "solution_57"
sr15_58 <- feature_representation(r15, sr15[, "solution_58"])
sr15_58$run <- "solution_58"
sr15_59 <- feature_representation(r15, sr15[, "solution_59"])
sr15_59$run <- "solution_59"
sr15_60 <- feature_representation(r15, sr15[, "solution_60"])
sr15_60$run <- "solution_60"
sr15_61 <- feature_representation(r15, sr15[, "solution_61"])
sr15_61$run <- "solution_61"
sr15_62 <- feature_representation(r15, sr15[, "solution_62"])
sr15_62$run <- "solution_62"
sr15_63 <- feature_representation(r15, sr15[, "solution_63"])
sr15_63$run <- "solution_63"
sr15_64 <- feature_representation(r15, sr15[, "solution_64"])
sr15_64$run <- "solution_64"
sr15_65 <- feature_representation(r15, sr15[, "solution_65"])
sr15_65$run <- "solution_65"
sr15_66 <- feature_representation(r15, sr15[, "solution_66"])
sr15_66$run <- "solution_66"
sr15_67 <- feature_representation(r15, sr15[, "solution_67"])
sr15_67$run <- "solution_67"
sr15_68 <- feature_representation(r15, sr15[, "solution_68"])
sr15_68$run <- "solution_68"
sr15_69 <- feature_representation(r15, sr15[, "solution_69"])
sr15_69$run <- "solution_69"
sr15_70 <- feature_representation(r15, sr15[, "solution_70"])
sr15_70$run <- "solution_70"
sr15_71 <- feature_representation(r15, sr15[, "solution_71"])
sr15_71$run <- "solution_71"
sr15_72 <- feature_representation(r15, sr15[, "solution_72"])
sr15_72$run <- "solution_72"
sr15_73 <- feature_representation(r15, sr15[, "solution_73"])
sr15_73$run <- "solution_73"
sr15_74 <- feature_representation(r15, sr15[, "solution_74"])
sr15_74$run <- "solution_74"
sr15_75 <- feature_representation(r15, sr15[, "solution_75"])
sr15_75$run <- "solution_75"
sr15_76 <- feature_representation(r15, sr15[, "solution_76"])
sr15_76$run <- "solution_76"
sr15_77 <- feature_representation(r15, sr15[, "solution_77"])
sr15_77$run <- "solution_77"
sr15_78 <- feature_representation(r15, sr15[, "solution_78"])
sr15_78$run <- "solution_78"
sr15_79 <- feature_representation(r15, sr15[, "solution_79"])
sr15_79$run <- "solution_79"
sr15_80 <- feature_representation(r15, sr15[, "solution_80"])
sr15_80$run <- "solution_80"
sr15_81 <- feature_representation(r15, sr15[, "solution_81"])
sr15_81$run <- "solution_81"
sr15_82 <- feature_representation(r15, sr15[, "solution_82"])
sr15_82$run <- "solution_82"
sr15_83 <- feature_representation(r15, sr15[, "solution_83"])
sr15_83$run <- "solution_83"
sr15_84 <- feature_representation(r15, sr15[, "solution_84"])
sr15_84$run <- "solution_84"
sr15_85 <- feature_representation(r15, sr15[, "solution_85"])
sr15_85$run <- "solution_85"
sr15_86 <- feature_representation(r15, sr15[, "solution_86"])
sr15_86$run <- "solution_86"
sr15_87 <- feature_representation(r15, sr15[, "solution_87"])
sr15_87$run <- "solution_87"
sr15_88 <- feature_representation(r15, sr15[, "solution_88"])
sr15_88$run <- "solution_88"
sr15_89 <- feature_representation(r15, sr15[, "solution_89"])
sr15_89$run <- "solution_89"
sr15_90 <- feature_representation(r15, sr15[, "solution_90"])
sr15_90$run <- "solution_90"
sr15_91 <- feature_representation(r15, sr15[, "solution_91"])
sr15_91$run <- "solution_91"
sr15_92 <- feature_representation(r15, sr15[, "solution_92"])
sr15_92$run <- "solution_92"
sr15_93 <- feature_representation(r15, sr15[, "solution_93"])
sr15_93$run <- "solution_93"
sr15_94 <- feature_representation(r15, sr15[, "solution_94"])
sr15_94$run <- "solution_94"
sr15_95 <- feature_representation(r15, sr15[, "solution_95"])
sr15_95$run <- "solution_95"
sr15_96 <- feature_representation(r15, sr15[, "solution_96"])
sr15_96$run <- "solution_96"
sr15_97 <- feature_representation(r15, sr15[, "solution_97"])
sr15_97$run <- "solution_97"
sr15_98 <- feature_representation(r15, sr15[, "solution_98"])
sr15_98$run <- "solution_98"
sr15_99 <- feature_representation(r15, sr15[, "solution_99"])
sr15_99$run <- "solution_99"
sr15_100 <- feature_representation(r15, sr15[, "solution_100"])
sr15_100$run <- "solution_100"
sr15_r<-bind_rows(sr15_1,sr15_2,sr15_3,sr15_4,sr15_5,sr15_6,sr15_7,sr15_8,sr15_9,sr15_10,sr15_11,sr15_12,sr15_13,sr15_14,sr15_15,sr15_16,sr15_17,sr15_18,sr15_19,sr15_20,sr15_21,sr15_22,sr15_23,sr15_24,sr15_25,sr15_26,sr15_27,sr15_28,sr15_29,sr15_30,sr15_31,sr15_32,sr15_33,sr15_34,sr15_35,sr15_36,sr15_37,sr15_38,sr15_39,sr15_40,sr15_41,sr15_42,sr15_43,sr15_44,sr15_45,sr15_46,sr15_47,sr15_48,sr15_49,sr15_50,sr15_51,sr15_52,sr15_53,sr15_54,sr15_55,sr15_56,sr15_57,sr15_58,sr15_59,sr15_60,sr15_61,sr15_62,sr15_63,sr15_64,sr15_65,sr15_66,sr15_67,sr15_68,sr15_69,sr15_70,sr15_71,sr15_72,sr15_73,sr15_74,sr15_75,sr15_76,sr15_77,sr15_78,sr15_79,sr15_80,sr15_81,sr15_82,sr15_83,sr15_84,sr15_85,sr15_86,sr15_87,sr15_88,sr15_89,sr15_90,sr15_91,sr15_92,sr15_93,sr15_94,sr15_95,sr15_96,sr15_97,sr15_98,sr15_99,sr15_100)
sr15_r$objective <- "Random"
sr15_r$species<-"15"


feature_a <- feature_abundances(r15, na.rm = FALSE)

sr15r<-left_join(sr15_r,feature_a)

#20
pu_dat20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r20 <- problem(pu_dat20, spec_dat20, cost_column = "cost", rij = puvsp_dat20) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat20$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr20 <- solve(r20)

sr20<-as.data.frame(sr20) %>% as_tibble()

sr20$objective<-"Random"
sr20$solution<-"20"

# Features Dataset
sr20_1 <- feature_representation(r20, sr20[, "solution_1"])
sr20_1$run <- "solution_1"
sr20_2 <- feature_representation(r20, sr20[, "solution_2"])
sr20_2$run <- "solution_2"
sr20_3 <- feature_representation(r20, sr20[, "solution_3"])
sr20_3$run <- "solution_3"
sr20_4 <- feature_representation(r20, sr20[, "solution_4"])
sr20_4$run <- "solution_4"
sr20_5 <- feature_representation(r20, sr20[, "solution_5"])
sr20_5$run <- "solution_5"
sr20_6 <- feature_representation(r20, sr20[, "solution_6"])
sr20_6$run <- "solution_6"
sr20_7 <- feature_representation(r20, sr20[, "solution_7"])
sr20_7$run <- "solution_7"
sr20_8 <- feature_representation(r20, sr20[, "solution_8"])
sr20_8$run <- "solution_8"
sr20_9 <- feature_representation(r20, sr20[, "solution_9"])
sr20_9$run <- "solution_9"
sr20_10 <- feature_representation(r20, sr20[, "solution_10"])
sr20_10$run <- "solution_10"
sr20_11 <- feature_representation(r20, sr20[, "solution_11"])
sr20_11$run <- "solution_11"
sr20_12 <- feature_representation(r20, sr20[, "solution_12"])
sr20_12$run <- "solution_12"
sr20_13 <- feature_representation(r20, sr20[, "solution_13"])
sr20_13$run <- "solution_13"
sr20_14 <- feature_representation(r20, sr20[, "solution_14"])
sr20_14$run <- "solution_14"
sr20_15 <- feature_representation(r20, sr20[, "solution_15"])
sr20_15$run <- "solution_15"
sr20_16 <- feature_representation(r20, sr20[, "solution_16"])
sr20_16$run <- "solution_16"
sr20_17 <- feature_representation(r20, sr20[, "solution_17"])
sr20_17$run <- "solution_17"
sr20_18 <- feature_representation(r20, sr20[, "solution_18"])
sr20_18$run <- "solution_18"
sr20_19 <- feature_representation(r20, sr20[, "solution_19"])
sr20_19$run <- "solution_19"
sr20_20 <- feature_representation(r20, sr20[, "solution_20"])
sr20_20$run <- "solution_20"
sr20_21 <- feature_representation(r20, sr20[, "solution_21"])
sr20_21$run <- "solution_21"
sr20_22 <- feature_representation(r20, sr20[, "solution_22"])
sr20_22$run <- "solution_22"
sr20_23 <- feature_representation(r20, sr20[, "solution_23"])
sr20_23$run <- "solution_23"
sr20_24 <- feature_representation(r20, sr20[, "solution_24"])
sr20_24$run <- "solution_24"
sr20_25 <- feature_representation(r20, sr20[, "solution_25"])
sr20_25$run <- "solution_25"
sr20_26 <- feature_representation(r20, sr20[, "solution_26"])
sr20_26$run <- "solution_26"
sr20_27 <- feature_representation(r20, sr20[, "solution_27"])
sr20_27$run <- "solution_27"
sr20_28 <- feature_representation(r20, sr20[, "solution_28"])
sr20_28$run <- "solution_28"
sr20_29 <- feature_representation(r20, sr20[, "solution_29"])
sr20_29$run <- "solution_29"
sr20_30 <- feature_representation(r20, sr20[, "solution_30"])
sr20_30$run <- "solution_30"
sr20_31 <- feature_representation(r20, sr20[, "solution_31"])
sr20_31$run <- "solution_31"
sr20_32 <- feature_representation(r20, sr20[, "solution_32"])
sr20_32$run <- "solution_32"
sr20_33 <- feature_representation(r20, sr20[, "solution_33"])
sr20_33$run <- "solution_33"
sr20_34 <- feature_representation(r20, sr20[, "solution_34"])
sr20_34$run <- "solution_34"
sr20_35 <- feature_representation(r20, sr20[, "solution_35"])
sr20_35$run <- "solution_35"
sr20_36 <- feature_representation(r20, sr20[, "solution_36"])
sr20_36$run <- "solution_36"
sr20_37 <- feature_representation(r20, sr20[, "solution_37"])
sr20_37$run <- "solution_37"
sr20_38 <- feature_representation(r20, sr20[, "solution_38"])
sr20_38$run <- "solution_38"
sr20_39 <- feature_representation(r20, sr20[, "solution_39"])
sr20_39$run <- "solution_39"
sr20_40 <- feature_representation(r20, sr20[, "solution_40"])
sr20_40$run <- "solution_40"
sr20_41 <- feature_representation(r20, sr20[, "solution_41"])
sr20_41$run <- "solution_41"
sr20_42 <- feature_representation(r20, sr20[, "solution_42"])
sr20_42$run <- "solution_42"
sr20_43 <- feature_representation(r20, sr20[, "solution_43"])
sr20_43$run <- "solution_43"
sr20_44 <- feature_representation(r20, sr20[, "solution_44"])
sr20_44$run <- "solution_44"
sr20_45 <- feature_representation(r20, sr20[, "solution_45"])
sr20_45$run <- "solution_45"
sr20_46 <- feature_representation(r20, sr20[, "solution_46"])
sr20_46$run <- "solution_46"
sr20_47 <- feature_representation(r20, sr20[, "solution_47"])
sr20_47$run <- "solution_47"
sr20_48 <- feature_representation(r20, sr20[, "solution_48"])
sr20_48$run <- "solution_48"
sr20_49 <- feature_representation(r20, sr20[, "solution_49"])
sr20_49$run <- "solution_49"
sr20_50 <- feature_representation(r20, sr20[, "solution_50"])
sr20_50$run <- "solution_50"
sr20_51 <- feature_representation(r20, sr20[, "solution_51"])
sr20_51$run <- "solution_51"
sr20_52 <- feature_representation(r20, sr20[, "solution_52"])
sr20_52$run <- "solution_52"
sr20_53 <- feature_representation(r20, sr20[, "solution_53"])
sr20_53$run <- "solution_53"
sr20_54 <- feature_representation(r20, sr20[, "solution_54"])
sr20_54$run <- "solution_54"
sr20_55 <- feature_representation(r20, sr20[, "solution_55"])
sr20_55$run <- "solution_55"
sr20_56 <- feature_representation(r20, sr20[, "solution_56"])
sr20_56$run <- "solution_56"
sr20_57 <- feature_representation(r20, sr20[, "solution_57"])
sr20_57$run <- "solution_57"
sr20_58 <- feature_representation(r20, sr20[, "solution_58"])
sr20_58$run <- "solution_58"
sr20_59 <- feature_representation(r20, sr20[, "solution_59"])
sr20_59$run <- "solution_59"
sr20_60 <- feature_representation(r20, sr20[, "solution_60"])
sr20_60$run <- "solution_60"
sr20_61 <- feature_representation(r20, sr20[, "solution_61"])
sr20_61$run <- "solution_61"
sr20_62 <- feature_representation(r20, sr20[, "solution_62"])
sr20_62$run <- "solution_62"
sr20_63 <- feature_representation(r20, sr20[, "solution_63"])
sr20_63$run <- "solution_63"
sr20_64 <- feature_representation(r20, sr20[, "solution_64"])
sr20_64$run <- "solution_64"
sr20_65 <- feature_representation(r20, sr20[, "solution_65"])
sr20_65$run <- "solution_65"
sr20_66 <- feature_representation(r20, sr20[, "solution_66"])
sr20_66$run <- "solution_66"
sr20_67 <- feature_representation(r20, sr20[, "solution_67"])
sr20_67$run <- "solution_67"
sr20_68 <- feature_representation(r20, sr20[, "solution_68"])
sr20_68$run <- "solution_68"
sr20_69 <- feature_representation(r20, sr20[, "solution_69"])
sr20_69$run <- "solution_69"
sr20_70 <- feature_representation(r20, sr20[, "solution_70"])
sr20_70$run <- "solution_70"
sr20_71 <- feature_representation(r20, sr20[, "solution_71"])
sr20_71$run <- "solution_71"
sr20_72 <- feature_representation(r20, sr20[, "solution_72"])
sr20_72$run <- "solution_72"
sr20_73 <- feature_representation(r20, sr20[, "solution_73"])
sr20_73$run <- "solution_73"
sr20_74 <- feature_representation(r20, sr20[, "solution_74"])
sr20_74$run <- "solution_74"
sr20_75 <- feature_representation(r20, sr20[, "solution_75"])
sr20_75$run <- "solution_75"
sr20_76 <- feature_representation(r20, sr20[, "solution_76"])
sr20_76$run <- "solution_76"
sr20_77 <- feature_representation(r20, sr20[, "solution_77"])
sr20_77$run <- "solution_77"
sr20_78 <- feature_representation(r20, sr20[, "solution_78"])
sr20_78$run <- "solution_78"
sr20_79 <- feature_representation(r20, sr20[, "solution_79"])
sr20_79$run <- "solution_79"
sr20_80 <- feature_representation(r20, sr20[, "solution_80"])
sr20_80$run <- "solution_80"
sr20_81 <- feature_representation(r20, sr20[, "solution_81"])
sr20_81$run <- "solution_81"
sr20_82 <- feature_representation(r20, sr20[, "solution_82"])
sr20_82$run <- "solution_82"
sr20_83 <- feature_representation(r20, sr20[, "solution_83"])
sr20_83$run <- "solution_83"
sr20_84 <- feature_representation(r20, sr20[, "solution_84"])
sr20_84$run <- "solution_84"
sr20_85 <- feature_representation(r20, sr20[, "solution_85"])
sr20_85$run <- "solution_85"
sr20_86 <- feature_representation(r20, sr20[, "solution_86"])
sr20_86$run <- "solution_86"
sr20_87 <- feature_representation(r20, sr20[, "solution_87"])
sr20_87$run <- "solution_87"
sr20_88 <- feature_representation(r20, sr20[, "solution_88"])
sr20_88$run <- "solution_88"
sr20_89 <- feature_representation(r20, sr20[, "solution_89"])
sr20_89$run <- "solution_89"
sr20_90 <- feature_representation(r20, sr20[, "solution_90"])
sr20_90$run <- "solution_90"
sr20_91 <- feature_representation(r20, sr20[, "solution_91"])
sr20_91$run <- "solution_91"
sr20_92 <- feature_representation(r20, sr20[, "solution_92"])
sr20_92$run <- "solution_92"
sr20_93 <- feature_representation(r20, sr20[, "solution_93"])
sr20_93$run <- "solution_93"
sr20_94 <- feature_representation(r20, sr20[, "solution_94"])
sr20_94$run <- "solution_94"
sr20_95 <- feature_representation(r20, sr20[, "solution_95"])
sr20_95$run <- "solution_95"
sr20_96 <- feature_representation(r20, sr20[, "solution_96"])
sr20_96$run <- "solution_96"
sr20_97 <- feature_representation(r20, sr20[, "solution_97"])
sr20_97$run <- "solution_97"
sr20_98 <- feature_representation(r20, sr20[, "solution_98"])
sr20_98$run <- "solution_98"
sr20_99 <- feature_representation(r20, sr20[, "solution_99"])
sr20_99$run <- "solution_99"
sr20_100 <- feature_representation(r20, sr20[, "solution_100"])
sr20_100$run <- "solution_100"
sr20_r<-bind_rows(sr20_1,sr20_2,sr20_3,sr20_4,sr20_5,sr20_6,sr20_7,sr20_8,sr20_9,sr20_10,sr20_11,sr20_12,sr20_13,sr20_14,sr20_15,sr20_16,sr20_17,sr20_18,sr20_19,sr20_20,sr20_21,sr20_22,sr20_23,sr20_24,sr20_25,sr20_26,sr20_27,sr20_28,sr20_29,sr20_30,sr20_31,sr20_32,sr20_33,sr20_34,sr20_35,sr20_36,sr20_37,sr20_38,sr20_39,sr20_40,sr20_41,sr20_42,sr20_43,sr20_44,sr20_45,sr20_46,sr20_47,sr20_48,sr20_49,sr20_50,sr20_51,sr20_52,sr20_53,sr20_54,sr20_55,sr20_56,sr20_57,sr20_58,sr20_59,sr20_60,sr20_61,sr20_62,sr20_63,sr20_64,sr20_65,sr20_66,sr20_67,sr20_68,sr20_69,sr20_70,sr20_71,sr20_72,sr20_73,sr20_74,sr20_75,sr20_76,sr20_77,sr20_78,sr20_79,sr20_80,sr20_81,sr20_82,sr20_83,sr20_84,sr20_85,sr20_86,sr20_87,sr20_88,sr20_89,sr20_90,sr20_91,sr20_92,sr20_93,sr20_94,sr20_95,sr20_96,sr20_97,sr20_98,sr20_99,sr20_100)
sr20_r$objective <- "Random"
sr20_r$species<-"20"


feature_a <- feature_abundances(r20, na.rm = FALSE)

sr20r<-left_join(sr20_r,feature_a)


#25
pu_dat25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r25 <- problem(pu_dat25, spec_dat25, cost_column = "cost", rij = puvsp_dat25) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat25$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr25 <- solve(r25)

sr25<-as.data.frame(sr25) %>% as_tibble()

sr25$objective<-"Random"
sr25$solution<-"25"

# Features Dataset
sr25_1 <- feature_representation(r25, sr25[, "solution_1"])
sr25_1$run <- "solution_1"
sr25_2 <- feature_representation(r25, sr25[, "solution_2"])
sr25_2$run <- "solution_2"
sr25_3 <- feature_representation(r25, sr25[, "solution_3"])
sr25_3$run <- "solution_3"
sr25_4 <- feature_representation(r25, sr25[, "solution_4"])
sr25_4$run <- "solution_4"
sr25_5 <- feature_representation(r25, sr25[, "solution_5"])
sr25_5$run <- "solution_5"
sr25_6 <- feature_representation(r25, sr25[, "solution_6"])
sr25_6$run <- "solution_6"
sr25_7 <- feature_representation(r25, sr25[, "solution_7"])
sr25_7$run <- "solution_7"
sr25_8 <- feature_representation(r25, sr25[, "solution_8"])
sr25_8$run <- "solution_8"
sr25_9 <- feature_representation(r25, sr25[, "solution_9"])
sr25_9$run <- "solution_9"
sr25_10 <- feature_representation(r25, sr25[, "solution_10"])
sr25_10$run <- "solution_10"
sr25_11 <- feature_representation(r25, sr25[, "solution_11"])
sr25_11$run <- "solution_11"
sr25_12 <- feature_representation(r25, sr25[, "solution_12"])
sr25_12$run <- "solution_12"
sr25_13 <- feature_representation(r25, sr25[, "solution_13"])
sr25_13$run <- "solution_13"
sr25_14 <- feature_representation(r25, sr25[, "solution_14"])
sr25_14$run <- "solution_14"
sr25_15 <- feature_representation(r25, sr25[, "solution_15"])
sr25_15$run <- "solution_15"
sr25_16 <- feature_representation(r25, sr25[, "solution_16"])
sr25_16$run <- "solution_16"
sr25_17 <- feature_representation(r25, sr25[, "solution_17"])
sr25_17$run <- "solution_17"
sr25_18 <- feature_representation(r25, sr25[, "solution_18"])
sr25_18$run <- "solution_18"
sr25_19 <- feature_representation(r25, sr25[, "solution_19"])
sr25_19$run <- "solution_19"
sr25_20 <- feature_representation(r25, sr25[, "solution_20"])
sr25_20$run <- "solution_20"
sr25_21 <- feature_representation(r25, sr25[, "solution_21"])
sr25_21$run <- "solution_21"
sr25_22 <- feature_representation(r25, sr25[, "solution_22"])
sr25_22$run <- "solution_22"
sr25_23 <- feature_representation(r25, sr25[, "solution_23"])
sr25_23$run <- "solution_23"
sr25_24 <- feature_representation(r25, sr25[, "solution_24"])
sr25_24$run <- "solution_24"
sr25_25 <- feature_representation(r25, sr25[, "solution_25"])
sr25_25$run <- "solution_25"
sr25_26 <- feature_representation(r25, sr25[, "solution_26"])
sr25_26$run <- "solution_26"
sr25_27 <- feature_representation(r25, sr25[, "solution_27"])
sr25_27$run <- "solution_27"
sr25_28 <- feature_representation(r25, sr25[, "solution_28"])
sr25_28$run <- "solution_28"
sr25_29 <- feature_representation(r25, sr25[, "solution_29"])
sr25_29$run <- "solution_29"
sr25_30 <- feature_representation(r25, sr25[, "solution_30"])
sr25_30$run <- "solution_30"
sr25_31 <- feature_representation(r25, sr25[, "solution_31"])
sr25_31$run <- "solution_31"
sr25_32 <- feature_representation(r25, sr25[, "solution_32"])
sr25_32$run <- "solution_32"
sr25_33 <- feature_representation(r25, sr25[, "solution_33"])
sr25_33$run <- "solution_33"
sr25_34 <- feature_representation(r25, sr25[, "solution_34"])
sr25_34$run <- "solution_34"
sr25_35 <- feature_representation(r25, sr25[, "solution_35"])
sr25_35$run <- "solution_35"
sr25_36 <- feature_representation(r25, sr25[, "solution_36"])
sr25_36$run <- "solution_36"
sr25_37 <- feature_representation(r25, sr25[, "solution_37"])
sr25_37$run <- "solution_37"
sr25_38 <- feature_representation(r25, sr25[, "solution_38"])
sr25_38$run <- "solution_38"
sr25_39 <- feature_representation(r25, sr25[, "solution_39"])
sr25_39$run <- "solution_39"
sr25_40 <- feature_representation(r25, sr25[, "solution_40"])
sr25_40$run <- "solution_40"
sr25_41 <- feature_representation(r25, sr25[, "solution_41"])
sr25_41$run <- "solution_41"
sr25_42 <- feature_representation(r25, sr25[, "solution_42"])
sr25_42$run <- "solution_42"
sr25_43 <- feature_representation(r25, sr25[, "solution_43"])
sr25_43$run <- "solution_43"
sr25_44 <- feature_representation(r25, sr25[, "solution_44"])
sr25_44$run <- "solution_44"
sr25_45 <- feature_representation(r25, sr25[, "solution_45"])
sr25_45$run <- "solution_45"
sr25_46 <- feature_representation(r25, sr25[, "solution_46"])
sr25_46$run <- "solution_46"
sr25_47 <- feature_representation(r25, sr25[, "solution_47"])
sr25_47$run <- "solution_47"
sr25_48 <- feature_representation(r25, sr25[, "solution_48"])
sr25_48$run <- "solution_48"
sr25_49 <- feature_representation(r25, sr25[, "solution_49"])
sr25_49$run <- "solution_49"
sr25_50 <- feature_representation(r25, sr25[, "solution_50"])
sr25_50$run <- "solution_50"
sr25_51 <- feature_representation(r25, sr25[, "solution_51"])
sr25_51$run <- "solution_51"
sr25_52 <- feature_representation(r25, sr25[, "solution_52"])
sr25_52$run <- "solution_52"
sr25_53 <- feature_representation(r25, sr25[, "solution_53"])
sr25_53$run <- "solution_53"
sr25_54 <- feature_representation(r25, sr25[, "solution_54"])
sr25_54$run <- "solution_54"
sr25_55 <- feature_representation(r25, sr25[, "solution_55"])
sr25_55$run <- "solution_55"
sr25_56 <- feature_representation(r25, sr25[, "solution_56"])
sr25_56$run <- "solution_56"
sr25_57 <- feature_representation(r25, sr25[, "solution_57"])
sr25_57$run <- "solution_57"
sr25_58 <- feature_representation(r25, sr25[, "solution_58"])
sr25_58$run <- "solution_58"
sr25_59 <- feature_representation(r25, sr25[, "solution_59"])
sr25_59$run <- "solution_59"
sr25_60 <- feature_representation(r25, sr25[, "solution_60"])
sr25_60$run <- "solution_60"
sr25_61 <- feature_representation(r25, sr25[, "solution_61"])
sr25_61$run <- "solution_61"
sr25_62 <- feature_representation(r25, sr25[, "solution_62"])
sr25_62$run <- "solution_62"
sr25_63 <- feature_representation(r25, sr25[, "solution_63"])
sr25_63$run <- "solution_63"
sr25_64 <- feature_representation(r25, sr25[, "solution_64"])
sr25_64$run <- "solution_64"
sr25_65 <- feature_representation(r25, sr25[, "solution_65"])
sr25_65$run <- "solution_65"
sr25_66 <- feature_representation(r25, sr25[, "solution_66"])
sr25_66$run <- "solution_66"
sr25_67 <- feature_representation(r25, sr25[, "solution_67"])
sr25_67$run <- "solution_67"
sr25_68 <- feature_representation(r25, sr25[, "solution_68"])
sr25_68$run <- "solution_68"
sr25_69 <- feature_representation(r25, sr25[, "solution_69"])
sr25_69$run <- "solution_69"
sr25_70 <- feature_representation(r25, sr25[, "solution_70"])
sr25_70$run <- "solution_70"
sr25_71 <- feature_representation(r25, sr25[, "solution_71"])
sr25_71$run <- "solution_71"
sr25_72 <- feature_representation(r25, sr25[, "solution_72"])
sr25_72$run <- "solution_72"
sr25_73 <- feature_representation(r25, sr25[, "solution_73"])
sr25_73$run <- "solution_73"
sr25_74 <- feature_representation(r25, sr25[, "solution_74"])
sr25_74$run <- "solution_74"
sr25_75 <- feature_representation(r25, sr25[, "solution_75"])
sr25_75$run <- "solution_75"
sr25_76 <- feature_representation(r25, sr25[, "solution_76"])
sr25_76$run <- "solution_76"
sr25_77 <- feature_representation(r25, sr25[, "solution_77"])
sr25_77$run <- "solution_77"
sr25_78 <- feature_representation(r25, sr25[, "solution_78"])
sr25_78$run <- "solution_78"
sr25_79 <- feature_representation(r25, sr25[, "solution_79"])
sr25_79$run <- "solution_79"
sr25_80 <- feature_representation(r25, sr25[, "solution_80"])
sr25_80$run <- "solution_80"
sr25_81 <- feature_representation(r25, sr25[, "solution_81"])
sr25_81$run <- "solution_81"
sr25_82 <- feature_representation(r25, sr25[, "solution_82"])
sr25_82$run <- "solution_82"
sr25_83 <- feature_representation(r25, sr25[, "solution_83"])
sr25_83$run <- "solution_83"
sr25_84 <- feature_representation(r25, sr25[, "solution_84"])
sr25_84$run <- "solution_84"
sr25_85 <- feature_representation(r25, sr25[, "solution_85"])
sr25_85$run <- "solution_85"
sr25_86 <- feature_representation(r25, sr25[, "solution_86"])
sr25_86$run <- "solution_86"
sr25_87 <- feature_representation(r25, sr25[, "solution_87"])
sr25_87$run <- "solution_87"
sr25_88 <- feature_representation(r25, sr25[, "solution_88"])
sr25_88$run <- "solution_88"
sr25_89 <- feature_representation(r25, sr25[, "solution_89"])
sr25_89$run <- "solution_89"
sr25_90 <- feature_representation(r25, sr25[, "solution_90"])
sr25_90$run <- "solution_90"
sr25_91 <- feature_representation(r25, sr25[, "solution_91"])
sr25_91$run <- "solution_91"
sr25_92 <- feature_representation(r25, sr25[, "solution_92"])
sr25_92$run <- "solution_92"
sr25_93 <- feature_representation(r25, sr25[, "solution_93"])
sr25_93$run <- "solution_93"
sr25_94 <- feature_representation(r25, sr25[, "solution_94"])
sr25_94$run <- "solution_94"
sr25_95 <- feature_representation(r25, sr25[, "solution_95"])
sr25_95$run <- "solution_95"
sr25_96 <- feature_representation(r25, sr25[, "solution_96"])
sr25_96$run <- "solution_96"
sr25_97 <- feature_representation(r25, sr25[, "solution_97"])
sr25_97$run <- "solution_97"
sr25_98 <- feature_representation(r25, sr25[, "solution_98"])
sr25_98$run <- "solution_98"
sr25_99 <- feature_representation(r25, sr25[, "solution_99"])
sr25_99$run <- "solution_99"
sr25_100 <- feature_representation(r25, sr25[, "solution_100"])
sr25_100$run <- "solution_100"
sr25_r<-bind_rows(sr25_1,sr25_2,sr25_3,sr25_4,sr25_5,sr25_6,sr25_7,sr25_8,sr25_9,sr25_10,sr25_11,sr25_12,sr25_13,sr25_14,sr25_15,sr25_16,sr25_17,sr25_18,sr25_19,sr25_20,sr25_21,sr25_22,sr25_23,sr25_24,sr25_25,sr25_26,sr25_27,sr25_28,sr25_29,sr25_30,sr25_31,sr25_32,sr25_33,sr25_34,sr25_35,sr25_36,sr25_37,sr25_38,sr25_39,sr25_40,sr25_41,sr25_42,sr25_43,sr25_44,sr25_45,sr25_46,sr25_47,sr25_48,sr25_49,sr25_50,sr25_51,sr25_52,sr25_53,sr25_54,sr25_55,sr25_56,sr25_57,sr25_58,sr25_59,sr25_60,sr25_61,sr25_62,sr25_63,sr25_64,sr25_65,sr25_66,sr25_67,sr25_68,sr25_69,sr25_70,sr25_71,sr25_72,sr25_73,sr25_74,sr25_75,sr25_76,sr25_77,sr25_78,sr25_79,sr25_80,sr25_81,sr25_82,sr25_83,sr25_84,sr25_85,sr25_86,sr25_87,sr25_88,sr25_89,sr25_90,sr25_91,sr25_92,sr25_93,sr25_94,sr25_95,sr25_96,sr25_97,sr25_98,sr25_99,sr25_100)
sr25_r$objective <- "Random"
sr25_r$species<-"25"


feature_a <- feature_abundances(r25, na.rm = FALSE)

sr25r<-left_join(sr25_r,feature_a)

#30
pu_dat30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r30 <- problem(pu_dat30, spec_dat30, cost_column = "cost", rij = puvsp_dat30) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat30$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr30 <- solve(r30)

sr30<-as.data.frame(sr30) %>% as_tibble()

sr30$objective<-"Random"

sr30$solution<-"30"

# Features Dataset
sr30_1 <- feature_representation(r30, sr30[, "solution_1"])
sr30_1$run <- "solution_1"
sr30_2 <- feature_representation(r30, sr30[, "solution_2"])
sr30_2$run <- "solution_2"
sr30_3 <- feature_representation(r30, sr30[, "solution_3"])
sr30_3$run <- "solution_3"
sr30_4 <- feature_representation(r30, sr30[, "solution_4"])
sr30_4$run <- "solution_4"
sr30_5 <- feature_representation(r30, sr30[, "solution_5"])
sr30_5$run <- "solution_5"
sr30_6 <- feature_representation(r30, sr30[, "solution_6"])
sr30_6$run <- "solution_6"
sr30_7 <- feature_representation(r30, sr30[, "solution_7"])
sr30_7$run <- "solution_7"
sr30_8 <- feature_representation(r30, sr30[, "solution_8"])
sr30_8$run <- "solution_8"
sr30_9 <- feature_representation(r30, sr30[, "solution_9"])
sr30_9$run <- "solution_9"
sr30_10 <- feature_representation(r30, sr30[, "solution_10"])
sr30_10$run <- "solution_10"
sr30_11 <- feature_representation(r30, sr30[, "solution_11"])
sr30_11$run <- "solution_11"
sr30_12 <- feature_representation(r30, sr30[, "solution_12"])
sr30_12$run <- "solution_12"
sr30_13 <- feature_representation(r30, sr30[, "solution_13"])
sr30_13$run <- "solution_13"
sr30_14 <- feature_representation(r30, sr30[, "solution_14"])
sr30_14$run <- "solution_14"
sr30_15 <- feature_representation(r30, sr30[, "solution_15"])
sr30_15$run <- "solution_15"
sr30_16 <- feature_representation(r30, sr30[, "solution_16"])
sr30_16$run <- "solution_16"
sr30_17 <- feature_representation(r30, sr30[, "solution_17"])
sr30_17$run <- "solution_17"
sr30_18 <- feature_representation(r30, sr30[, "solution_18"])
sr30_18$run <- "solution_18"
sr30_19 <- feature_representation(r30, sr30[, "solution_19"])
sr30_19$run <- "solution_19"
sr30_20 <- feature_representation(r30, sr30[, "solution_20"])
sr30_20$run <- "solution_20"
sr30_21 <- feature_representation(r30, sr30[, "solution_21"])
sr30_21$run <- "solution_21"
sr30_22 <- feature_representation(r30, sr30[, "solution_22"])
sr30_22$run <- "solution_22"
sr30_23 <- feature_representation(r30, sr30[, "solution_23"])
sr30_23$run <- "solution_23"
sr30_24 <- feature_representation(r30, sr30[, "solution_24"])
sr30_24$run <- "solution_24"
sr30_25 <- feature_representation(r30, sr30[, "solution_25"])
sr30_25$run <- "solution_25"
sr30_26 <- feature_representation(r30, sr30[, "solution_26"])
sr30_26$run <- "solution_26"
sr30_27 <- feature_representation(r30, sr30[, "solution_27"])
sr30_27$run <- "solution_27"
sr30_28 <- feature_representation(r30, sr30[, "solution_28"])
sr30_28$run <- "solution_28"
sr30_29 <- feature_representation(r30, sr30[, "solution_29"])
sr30_29$run <- "solution_29"
sr30_30 <- feature_representation(r30, sr30[, "solution_30"])
sr30_30$run <- "solution_30"
sr30_31 <- feature_representation(r30, sr30[, "solution_31"])
sr30_31$run <- "solution_31"
sr30_32 <- feature_representation(r30, sr30[, "solution_32"])
sr30_32$run <- "solution_32"
sr30_33 <- feature_representation(r30, sr30[, "solution_33"])
sr30_33$run <- "solution_33"
sr30_34 <- feature_representation(r30, sr30[, "solution_34"])
sr30_34$run <- "solution_34"
sr30_35 <- feature_representation(r30, sr30[, "solution_35"])
sr30_35$run <- "solution_35"
sr30_36 <- feature_representation(r30, sr30[, "solution_36"])
sr30_36$run <- "solution_36"
sr30_37 <- feature_representation(r30, sr30[, "solution_37"])
sr30_37$run <- "solution_37"
sr30_38 <- feature_representation(r30, sr30[, "solution_38"])
sr30_38$run <- "solution_38"
sr30_39 <- feature_representation(r30, sr30[, "solution_39"])
sr30_39$run <- "solution_39"
sr30_40 <- feature_representation(r30, sr30[, "solution_40"])
sr30_40$run <- "solution_40"
sr30_41 <- feature_representation(r30, sr30[, "solution_41"])
sr30_41$run <- "solution_41"
sr30_42 <- feature_representation(r30, sr30[, "solution_42"])
sr30_42$run <- "solution_42"
sr30_43 <- feature_representation(r30, sr30[, "solution_43"])
sr30_43$run <- "solution_43"
sr30_44 <- feature_representation(r30, sr30[, "solution_44"])
sr30_44$run <- "solution_44"
sr30_45 <- feature_representation(r30, sr30[, "solution_45"])
sr30_45$run <- "solution_45"
sr30_46 <- feature_representation(r30, sr30[, "solution_46"])
sr30_46$run <- "solution_46"
sr30_47 <- feature_representation(r30, sr30[, "solution_47"])
sr30_47$run <- "solution_47"
sr30_48 <- feature_representation(r30, sr30[, "solution_48"])
sr30_48$run <- "solution_48"
sr30_49 <- feature_representation(r30, sr30[, "solution_49"])
sr30_49$run <- "solution_49"
sr30_50 <- feature_representation(r30, sr30[, "solution_50"])
sr30_50$run <- "solution_50"
sr30_51 <- feature_representation(r30, sr30[, "solution_51"])
sr30_51$run <- "solution_51"
sr30_52 <- feature_representation(r30, sr30[, "solution_52"])
sr30_52$run <- "solution_52"
sr30_53 <- feature_representation(r30, sr30[, "solution_53"])
sr30_53$run <- "solution_53"
sr30_54 <- feature_representation(r30, sr30[, "solution_54"])
sr30_54$run <- "solution_54"
sr30_55 <- feature_representation(r30, sr30[, "solution_55"])
sr30_55$run <- "solution_55"
sr30_56 <- feature_representation(r30, sr30[, "solution_56"])
sr30_56$run <- "solution_56"
sr30_57 <- feature_representation(r30, sr30[, "solution_57"])
sr30_57$run <- "solution_57"
sr30_58 <- feature_representation(r30, sr30[, "solution_58"])
sr30_58$run <- "solution_58"
sr30_59 <- feature_representation(r30, sr30[, "solution_59"])
sr30_59$run <- "solution_59"
sr30_60 <- feature_representation(r30, sr30[, "solution_60"])
sr30_60$run <- "solution_60"
sr30_61 <- feature_representation(r30, sr30[, "solution_61"])
sr30_61$run <- "solution_61"
sr30_62 <- feature_representation(r30, sr30[, "solution_62"])
sr30_62$run <- "solution_62"
sr30_63 <- feature_representation(r30, sr30[, "solution_63"])
sr30_63$run <- "solution_63"
sr30_64 <- feature_representation(r30, sr30[, "solution_64"])
sr30_64$run <- "solution_64"
sr30_65 <- feature_representation(r30, sr30[, "solution_65"])
sr30_65$run <- "solution_65"
sr30_66 <- feature_representation(r30, sr30[, "solution_66"])
sr30_66$run <- "solution_66"
sr30_67 <- feature_representation(r30, sr30[, "solution_67"])
sr30_67$run <- "solution_67"
sr30_68 <- feature_representation(r30, sr30[, "solution_68"])
sr30_68$run <- "solution_68"
sr30_69 <- feature_representation(r30, sr30[, "solution_69"])
sr30_69$run <- "solution_69"
sr30_70 <- feature_representation(r30, sr30[, "solution_70"])
sr30_70$run <- "solution_70"
sr30_71 <- feature_representation(r30, sr30[, "solution_71"])
sr30_71$run <- "solution_71"
sr30_72 <- feature_representation(r30, sr30[, "solution_72"])
sr30_72$run <- "solution_72"
sr30_73 <- feature_representation(r30, sr30[, "solution_73"])
sr30_73$run <- "solution_73"
sr30_74 <- feature_representation(r30, sr30[, "solution_74"])
sr30_74$run <- "solution_74"
sr30_75 <- feature_representation(r30, sr30[, "solution_75"])
sr30_75$run <- "solution_75"
sr30_76 <- feature_representation(r30, sr30[, "solution_76"])
sr30_76$run <- "solution_76"
sr30_77 <- feature_representation(r30, sr30[, "solution_77"])
sr30_77$run <- "solution_77"
sr30_78 <- feature_representation(r30, sr30[, "solution_78"])
sr30_78$run <- "solution_78"
sr30_79 <- feature_representation(r30, sr30[, "solution_79"])
sr30_79$run <- "solution_79"
sr30_80 <- feature_representation(r30, sr30[, "solution_80"])
sr30_80$run <- "solution_80"
sr30_81 <- feature_representation(r30, sr30[, "solution_81"])
sr30_81$run <- "solution_81"
sr30_82 <- feature_representation(r30, sr30[, "solution_82"])
sr30_82$run <- "solution_82"
sr30_83 <- feature_representation(r30, sr30[, "solution_83"])
sr30_83$run <- "solution_83"
sr30_84 <- feature_representation(r30, sr30[, "solution_84"])
sr30_84$run <- "solution_84"
sr30_85 <- feature_representation(r30, sr30[, "solution_85"])
sr30_85$run <- "solution_85"
sr30_86 <- feature_representation(r30, sr30[, "solution_86"])
sr30_86$run <- "solution_86"
sr30_87 <- feature_representation(r30, sr30[, "solution_87"])
sr30_87$run <- "solution_87"
sr30_88 <- feature_representation(r30, sr30[, "solution_88"])
sr30_88$run <- "solution_88"
sr30_89 <- feature_representation(r30, sr30[, "solution_89"])
sr30_89$run <- "solution_89"
sr30_90 <- feature_representation(r30, sr30[, "solution_90"])
sr30_90$run <- "solution_90"
sr30_91 <- feature_representation(r30, sr30[, "solution_91"])
sr30_91$run <- "solution_91"
sr30_92 <- feature_representation(r30, sr30[, "solution_92"])
sr30_92$run <- "solution_92"
sr30_93 <- feature_representation(r30, sr30[, "solution_93"])
sr30_93$run <- "solution_93"
sr30_94 <- feature_representation(r30, sr30[, "solution_94"])
sr30_94$run <- "solution_94"
sr30_95 <- feature_representation(r30, sr30[, "solution_95"])
sr30_95$run <- "solution_95"
sr30_96 <- feature_representation(r30, sr30[, "solution_96"])
sr30_96$run <- "solution_96"
sr30_97 <- feature_representation(r30, sr30[, "solution_97"])
sr30_97$run <- "solution_97"
sr30_98 <- feature_representation(r30, sr30[, "solution_98"])
sr30_98$run <- "solution_98"
sr30_99 <- feature_representation(r30, sr30[, "solution_99"])
sr30_99$run <- "solution_99"
sr30_100 <- feature_representation(r30, sr30[, "solution_100"])
sr30_100$run <- "solution_100"
sr30_r<-bind_rows(sr30_1,sr30_2,sr30_3,sr30_4,sr30_5,sr30_6,sr30_7,sr30_8,sr30_9,sr30_10,sr30_11,sr30_12,sr30_13,sr30_14,sr30_15,sr30_16,sr30_17,sr30_18,sr30_19,sr30_20,sr30_21,sr30_22,sr30_23,sr30_24,sr30_25,sr30_26,sr30_27,sr30_28,sr30_29,sr30_30,sr30_31,sr30_32,sr30_33,sr30_34,sr30_35,sr30_36,sr30_37,sr30_38,sr30_39,sr30_40,sr30_41,sr30_42,sr30_43,sr30_44,sr30_45,sr30_46,sr30_47,sr30_48,sr30_49,sr30_50,sr30_51,sr30_52,sr30_53,sr30_54,sr30_55,sr30_56,sr30_57,sr30_58,sr30_59,sr30_60,sr30_61,sr30_62,sr30_63,sr30_64,sr30_65,sr30_66,sr30_67,sr30_68,sr30_69,sr30_70,sr30_71,sr30_72,sr30_73,sr30_74,sr30_75,sr30_76,sr30_77,sr30_78,sr30_79,sr30_80,sr30_81,sr30_82,sr30_83,sr30_84,sr30_85,sr30_86,sr30_87,sr30_88,sr30_89,sr30_90,sr30_91,sr30_92,sr30_93,sr30_94,sr30_95,sr30_96,sr30_97,sr30_98,sr30_99,sr30_100)
sr30_r$objective <- "Random"
sr30_r$species<-"30"


feature_a <- feature_abundances(r30, na.rm = FALSE)

sr30r<-left_join(sr30_r,feature_a)

#35
pu_dat35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r35 <- problem(pu_dat35, spec_dat35, cost_column = "cost", rij = puvsp_dat35) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat35$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr35 <- solve(r35) 

sr35<-as.data.frame(sr35) %>% as_tibble()

sr35$objective<-"Random"
sr35$solution<-"35"

# Features Dataset
sr35_1 <- feature_representation(r35, sr35[, "solution_1"])
sr35_1$run <- "solution_1"
sr35_2 <- feature_representation(r35, sr35[, "solution_2"])
sr35_2$run <- "solution_2"
sr35_3 <- feature_representation(r35, sr35[, "solution_3"])
sr35_3$run <- "solution_3"
sr35_4 <- feature_representation(r35, sr35[, "solution_4"])
sr35_4$run <- "solution_4"
sr35_5 <- feature_representation(r35, sr35[, "solution_5"])
sr35_5$run <- "solution_5"
sr35_6 <- feature_representation(r35, sr35[, "solution_6"])
sr35_6$run <- "solution_6"
sr35_7 <- feature_representation(r35, sr35[, "solution_7"])
sr35_7$run <- "solution_7"
sr35_8 <- feature_representation(r35, sr35[, "solution_8"])
sr35_8$run <- "solution_8"
sr35_9 <- feature_representation(r35, sr35[, "solution_9"])
sr35_9$run <- "solution_9"
sr35_10 <- feature_representation(r35, sr35[, "solution_10"])
sr35_10$run <- "solution_10"
sr35_11 <- feature_representation(r35, sr35[, "solution_11"])
sr35_11$run <- "solution_11"
sr35_12 <- feature_representation(r35, sr35[, "solution_12"])
sr35_12$run <- "solution_12"
sr35_13 <- feature_representation(r35, sr35[, "solution_13"])
sr35_13$run <- "solution_13"
sr35_14 <- feature_representation(r35, sr35[, "solution_14"])
sr35_14$run <- "solution_14"
sr35_15 <- feature_representation(r35, sr35[, "solution_15"])
sr35_15$run <- "solution_15"
sr35_16 <- feature_representation(r35, sr35[, "solution_16"])
sr35_16$run <- "solution_16"
sr35_17 <- feature_representation(r35, sr35[, "solution_17"])
sr35_17$run <- "solution_17"
sr35_18 <- feature_representation(r35, sr35[, "solution_18"])
sr35_18$run <- "solution_18"
sr35_19 <- feature_representation(r35, sr35[, "solution_19"])
sr35_19$run <- "solution_19"
sr35_20 <- feature_representation(r35, sr35[, "solution_20"])
sr35_20$run <- "solution_20"
sr35_21 <- feature_representation(r35, sr35[, "solution_21"])
sr35_21$run <- "solution_21"
sr35_22 <- feature_representation(r35, sr35[, "solution_22"])
sr35_22$run <- "solution_22"
sr35_23 <- feature_representation(r35, sr35[, "solution_23"])
sr35_23$run <- "solution_23"
sr35_24 <- feature_representation(r35, sr35[, "solution_24"])
sr35_24$run <- "solution_24"
sr35_25 <- feature_representation(r35, sr35[, "solution_25"])
sr35_25$run <- "solution_25"
sr35_26 <- feature_representation(r35, sr35[, "solution_26"])
sr35_26$run <- "solution_26"
sr35_27 <- feature_representation(r35, sr35[, "solution_27"])
sr35_27$run <- "solution_27"
sr35_28 <- feature_representation(r35, sr35[, "solution_28"])
sr35_28$run <- "solution_28"
sr35_29 <- feature_representation(r35, sr35[, "solution_29"])
sr35_29$run <- "solution_29"
sr35_30 <- feature_representation(r35, sr35[, "solution_30"])
sr35_30$run <- "solution_30"
sr35_31 <- feature_representation(r35, sr35[, "solution_31"])
sr35_31$run <- "solution_31"
sr35_32 <- feature_representation(r35, sr35[, "solution_32"])
sr35_32$run <- "solution_32"
sr35_33 <- feature_representation(r35, sr35[, "solution_33"])
sr35_33$run <- "solution_33"
sr35_34 <- feature_representation(r35, sr35[, "solution_34"])
sr35_34$run <- "solution_34"
sr35_35 <- feature_representation(r35, sr35[, "solution_35"])
sr35_35$run <- "solution_35"
sr35_36 <- feature_representation(r35, sr35[, "solution_36"])
sr35_36$run <- "solution_36"
sr35_37 <- feature_representation(r35, sr35[, "solution_37"])
sr35_37$run <- "solution_37"
sr35_38 <- feature_representation(r35, sr35[, "solution_38"])
sr35_38$run <- "solution_38"
sr35_39 <- feature_representation(r35, sr35[, "solution_39"])
sr35_39$run <- "solution_39"
sr35_40 <- feature_representation(r35, sr35[, "solution_40"])
sr35_40$run <- "solution_40"
sr35_41 <- feature_representation(r35, sr35[, "solution_41"])
sr35_41$run <- "solution_41"
sr35_42 <- feature_representation(r35, sr35[, "solution_42"])
sr35_42$run <- "solution_42"
sr35_43 <- feature_representation(r35, sr35[, "solution_43"])
sr35_43$run <- "solution_43"
sr35_44 <- feature_representation(r35, sr35[, "solution_44"])
sr35_44$run <- "solution_44"
sr35_45 <- feature_representation(r35, sr35[, "solution_45"])
sr35_45$run <- "solution_45"
sr35_46 <- feature_representation(r35, sr35[, "solution_46"])
sr35_46$run <- "solution_46"
sr35_47 <- feature_representation(r35, sr35[, "solution_47"])
sr35_47$run <- "solution_47"
sr35_48 <- feature_representation(r35, sr35[, "solution_48"])
sr35_48$run <- "solution_48"
sr35_49 <- feature_representation(r35, sr35[, "solution_49"])
sr35_49$run <- "solution_49"
sr35_50 <- feature_representation(r35, sr35[, "solution_50"])
sr35_50$run <- "solution_50"
sr35_51 <- feature_representation(r35, sr35[, "solution_51"])
sr35_51$run <- "solution_51"
sr35_52 <- feature_representation(r35, sr35[, "solution_52"])
sr35_52$run <- "solution_52"
sr35_53 <- feature_representation(r35, sr35[, "solution_53"])
sr35_53$run <- "solution_53"
sr35_54 <- feature_representation(r35, sr35[, "solution_54"])
sr35_54$run <- "solution_54"
sr35_55 <- feature_representation(r35, sr35[, "solution_55"])
sr35_55$run <- "solution_55"
sr35_56 <- feature_representation(r35, sr35[, "solution_56"])
sr35_56$run <- "solution_56"
sr35_57 <- feature_representation(r35, sr35[, "solution_57"])
sr35_57$run <- "solution_57"
sr35_58 <- feature_representation(r35, sr35[, "solution_58"])
sr35_58$run <- "solution_58"
sr35_59 <- feature_representation(r35, sr35[, "solution_59"])
sr35_59$run <- "solution_59"
sr35_60 <- feature_representation(r35, sr35[, "solution_60"])
sr35_60$run <- "solution_60"
sr35_61 <- feature_representation(r35, sr35[, "solution_61"])
sr35_61$run <- "solution_61"
sr35_62 <- feature_representation(r35, sr35[, "solution_62"])
sr35_62$run <- "solution_62"
sr35_63 <- feature_representation(r35, sr35[, "solution_63"])
sr35_63$run <- "solution_63"
sr35_64 <- feature_representation(r35, sr35[, "solution_64"])
sr35_64$run <- "solution_64"
sr35_65 <- feature_representation(r35, sr35[, "solution_65"])
sr35_65$run <- "solution_65"
sr35_66 <- feature_representation(r35, sr35[, "solution_66"])
sr35_66$run <- "solution_66"
sr35_67 <- feature_representation(r35, sr35[, "solution_67"])
sr35_67$run <- "solution_67"
sr35_68 <- feature_representation(r35, sr35[, "solution_68"])
sr35_68$run <- "solution_68"
sr35_69 <- feature_representation(r35, sr35[, "solution_69"])
sr35_69$run <- "solution_69"
sr35_70 <- feature_representation(r35, sr35[, "solution_70"])
sr35_70$run <- "solution_70"
sr35_71 <- feature_representation(r35, sr35[, "solution_71"])
sr35_71$run <- "solution_71"
sr35_72 <- feature_representation(r35, sr35[, "solution_72"])
sr35_72$run <- "solution_72"
sr35_73 <- feature_representation(r35, sr35[, "solution_73"])
sr35_73$run <- "solution_73"
sr35_74 <- feature_representation(r35, sr35[, "solution_74"])
sr35_74$run <- "solution_74"
sr35_75 <- feature_representation(r35, sr35[, "solution_75"])
sr35_75$run <- "solution_75"
sr35_76 <- feature_representation(r35, sr35[, "solution_76"])
sr35_76$run <- "solution_76"
sr35_77 <- feature_representation(r35, sr35[, "solution_77"])
sr35_77$run <- "solution_77"
sr35_78 <- feature_representation(r35, sr35[, "solution_78"])
sr35_78$run <- "solution_78"
sr35_79 <- feature_representation(r35, sr35[, "solution_79"])
sr35_79$run <- "solution_79"
sr35_80 <- feature_representation(r35, sr35[, "solution_80"])
sr35_80$run <- "solution_80"
sr35_81 <- feature_representation(r35, sr35[, "solution_81"])
sr35_81$run <- "solution_81"
sr35_82 <- feature_representation(r35, sr35[, "solution_82"])
sr35_82$run <- "solution_82"
sr35_83 <- feature_representation(r35, sr35[, "solution_83"])
sr35_83$run <- "solution_83"
sr35_84 <- feature_representation(r35, sr35[, "solution_84"])
sr35_84$run <- "solution_84"
sr35_85 <- feature_representation(r35, sr35[, "solution_85"])
sr35_85$run <- "solution_85"
sr35_86 <- feature_representation(r35, sr35[, "solution_86"])
sr35_86$run <- "solution_86"
sr35_87 <- feature_representation(r35, sr35[, "solution_87"])
sr35_87$run <- "solution_87"
sr35_88 <- feature_representation(r35, sr35[, "solution_88"])
sr35_88$run <- "solution_88"
sr35_89 <- feature_representation(r35, sr35[, "solution_89"])
sr35_89$run <- "solution_89"
sr35_90 <- feature_representation(r35, sr35[, "solution_90"])
sr35_90$run <- "solution_90"
sr35_91 <- feature_representation(r35, sr35[, "solution_91"])
sr35_91$run <- "solution_91"
sr35_92 <- feature_representation(r35, sr35[, "solution_92"])
sr35_92$run <- "solution_92"
sr35_93 <- feature_representation(r35, sr35[, "solution_93"])
sr35_93$run <- "solution_93"
sr35_94 <- feature_representation(r35, sr35[, "solution_94"])
sr35_94$run <- "solution_94"
sr35_95 <- feature_representation(r35, sr35[, "solution_95"])
sr35_95$run <- "solution_95"
sr35_96 <- feature_representation(r35, sr35[, "solution_96"])
sr35_96$run <- "solution_96"
sr35_97 <- feature_representation(r35, sr35[, "solution_97"])
sr35_97$run <- "solution_97"
sr35_98 <- feature_representation(r35, sr35[, "solution_98"])
sr35_98$run <- "solution_98"
sr35_99 <- feature_representation(r35, sr35[, "solution_99"])
sr35_99$run <- "solution_99"
sr35_100 <- feature_representation(r35, sr35[, "solution_100"])
sr35_100$run <- "solution_100"
sr35_r<-bind_rows(sr35_1,sr35_2,sr35_3,sr35_4,sr35_5,sr35_6,sr35_7,sr35_8,sr35_9,sr35_10,sr35_11,sr35_12,sr35_13,sr35_14,sr35_15,sr35_16,sr35_17,sr35_18,sr35_19,sr35_20,sr35_21,sr35_22,sr35_23,sr35_24,sr35_25,sr35_26,sr35_27,sr35_28,sr35_29,sr35_30,sr35_31,sr35_32,sr35_33,sr35_34,sr35_35,sr35_36,sr35_37,sr35_38,sr35_39,sr35_40,sr35_41,sr35_42,sr35_43,sr35_44,sr35_45,sr35_46,sr35_47,sr35_48,sr35_49,sr35_50,sr35_51,sr35_52,sr35_53,sr35_54,sr35_55,sr35_56,sr35_57,sr35_58,sr35_59,sr35_60,sr35_61,sr35_62,sr35_63,sr35_64,sr35_65,sr35_66,sr35_67,sr35_68,sr35_69,sr35_70,sr35_71,sr35_72,sr35_73,sr35_74,sr35_75,sr35_76,sr35_77,sr35_78,sr35_79,sr35_80,sr35_81,sr35_82,sr35_83,sr35_84,sr35_85,sr35_86,sr35_87,sr35_88,sr35_89,sr35_90,sr35_91,sr35_92,sr35_93,sr35_94,sr35_95,sr35_96,sr35_97,sr35_98,sr35_99,sr35_100)
sr35_r$objective <- "Random"
sr35_r$species<-"35"


feature_a <- feature_abundances(r35, na.rm = FALSE)

sr35r<-left_join(sr35_r,feature_a)

#40
pu_dat40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r40 <- problem(pu_dat40, spec_dat40, cost_column = "cost", rij = puvsp_dat40) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat40$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr40 <- solve(r40)

sr40<-as.data.frame(sr40) %>% as_tibble()

sr40$objective<-"Random"
sr40$solution<-"40"

# Features Dataset
sr40_1 <- feature_representation(r40, sr40[, "solution_1"])
sr40_1$run <- "solution_1"
sr40_2 <- feature_representation(r40, sr40[, "solution_2"])
sr40_2$run <- "solution_2"
sr40_3 <- feature_representation(r40, sr40[, "solution_3"])
sr40_3$run <- "solution_3"
sr40_4 <- feature_representation(r40, sr40[, "solution_4"])
sr40_4$run <- "solution_4"
sr40_5 <- feature_representation(r40, sr40[, "solution_5"])
sr40_5$run <- "solution_5"
sr40_6 <- feature_representation(r40, sr40[, "solution_6"])
sr40_6$run <- "solution_6"
sr40_7 <- feature_representation(r40, sr40[, "solution_7"])
sr40_7$run <- "solution_7"
sr40_8 <- feature_representation(r40, sr40[, "solution_8"])
sr40_8$run <- "solution_8"
sr40_9 <- feature_representation(r40, sr40[, "solution_9"])
sr40_9$run <- "solution_9"
sr40_10 <- feature_representation(r40, sr40[, "solution_10"])
sr40_10$run <- "solution_10"
sr40_11 <- feature_representation(r40, sr40[, "solution_11"])
sr40_11$run <- "solution_11"
sr40_12 <- feature_representation(r40, sr40[, "solution_12"])
sr40_12$run <- "solution_12"
sr40_13 <- feature_representation(r40, sr40[, "solution_13"])
sr40_13$run <- "solution_13"
sr40_14 <- feature_representation(r40, sr40[, "solution_14"])
sr40_14$run <- "solution_14"
sr40_15 <- feature_representation(r40, sr40[, "solution_15"])
sr40_15$run <- "solution_15"
sr40_16 <- feature_representation(r40, sr40[, "solution_16"])
sr40_16$run <- "solution_16"
sr40_17 <- feature_representation(r40, sr40[, "solution_17"])
sr40_17$run <- "solution_17"
sr40_18 <- feature_representation(r40, sr40[, "solution_18"])
sr40_18$run <- "solution_18"
sr40_19 <- feature_representation(r40, sr40[, "solution_19"])
sr40_19$run <- "solution_19"
sr40_20 <- feature_representation(r40, sr40[, "solution_20"])
sr40_20$run <- "solution_20"
sr40_21 <- feature_representation(r40, sr40[, "solution_21"])
sr40_21$run <- "solution_21"
sr40_22 <- feature_representation(r40, sr40[, "solution_22"])
sr40_22$run <- "solution_22"
sr40_23 <- feature_representation(r40, sr40[, "solution_23"])
sr40_23$run <- "solution_23"
sr40_24 <- feature_representation(r40, sr40[, "solution_24"])
sr40_24$run <- "solution_24"
sr40_25 <- feature_representation(r40, sr40[, "solution_25"])
sr40_25$run <- "solution_25"
sr40_26 <- feature_representation(r40, sr40[, "solution_26"])
sr40_26$run <- "solution_26"
sr40_27 <- feature_representation(r40, sr40[, "solution_27"])
sr40_27$run <- "solution_27"
sr40_28 <- feature_representation(r40, sr40[, "solution_28"])
sr40_28$run <- "solution_28"
sr40_29 <- feature_representation(r40, sr40[, "solution_29"])
sr40_29$run <- "solution_29"
sr40_30 <- feature_representation(r40, sr40[, "solution_30"])
sr40_30$run <- "solution_30"
sr40_31 <- feature_representation(r40, sr40[, "solution_31"])
sr40_31$run <- "solution_31"
sr40_32 <- feature_representation(r40, sr40[, "solution_32"])
sr40_32$run <- "solution_32"
sr40_33 <- feature_representation(r40, sr40[, "solution_33"])
sr40_33$run <- "solution_33"
sr40_34 <- feature_representation(r40, sr40[, "solution_34"])
sr40_34$run <- "solution_34"
sr40_35 <- feature_representation(r40, sr40[, "solution_35"])
sr40_35$run <- "solution_35"
sr40_36 <- feature_representation(r40, sr40[, "solution_36"])
sr40_36$run <- "solution_36"
sr40_37 <- feature_representation(r40, sr40[, "solution_37"])
sr40_37$run <- "solution_37"
sr40_38 <- feature_representation(r40, sr40[, "solution_38"])
sr40_38$run <- "solution_38"
sr40_39 <- feature_representation(r40, sr40[, "solution_39"])
sr40_39$run <- "solution_39"
sr40_40 <- feature_representation(r40, sr40[, "solution_40"])
sr40_40$run <- "solution_40"
sr40_41 <- feature_representation(r40, sr40[, "solution_41"])
sr40_41$run <- "solution_41"
sr40_42 <- feature_representation(r40, sr40[, "solution_42"])
sr40_42$run <- "solution_42"
sr40_43 <- feature_representation(r40, sr40[, "solution_43"])
sr40_43$run <- "solution_43"
sr40_44 <- feature_representation(r40, sr40[, "solution_44"])
sr40_44$run <- "solution_44"
sr40_45 <- feature_representation(r40, sr40[, "solution_45"])
sr40_45$run <- "solution_45"
sr40_46 <- feature_representation(r40, sr40[, "solution_46"])
sr40_46$run <- "solution_46"
sr40_47 <- feature_representation(r40, sr40[, "solution_47"])
sr40_47$run <- "solution_47"
sr40_48 <- feature_representation(r40, sr40[, "solution_48"])
sr40_48$run <- "solution_48"
sr40_49 <- feature_representation(r40, sr40[, "solution_49"])
sr40_49$run <- "solution_49"
sr40_50 <- feature_representation(r40, sr40[, "solution_50"])
sr40_50$run <- "solution_50"
sr40_51 <- feature_representation(r40, sr40[, "solution_51"])
sr40_51$run <- "solution_51"
sr40_52 <- feature_representation(r40, sr40[, "solution_52"])
sr40_52$run <- "solution_52"
sr40_53 <- feature_representation(r40, sr40[, "solution_53"])
sr40_53$run <- "solution_53"
sr40_54 <- feature_representation(r40, sr40[, "solution_54"])
sr40_54$run <- "solution_54"
sr40_55 <- feature_representation(r40, sr40[, "solution_55"])
sr40_55$run <- "solution_55"
sr40_56 <- feature_representation(r40, sr40[, "solution_56"])
sr40_56$run <- "solution_56"
sr40_57 <- feature_representation(r40, sr40[, "solution_57"])
sr40_57$run <- "solution_57"
sr40_58 <- feature_representation(r40, sr40[, "solution_58"])
sr40_58$run <- "solution_58"
sr40_59 <- feature_representation(r40, sr40[, "solution_59"])
sr40_59$run <- "solution_59"
sr40_60 <- feature_representation(r40, sr40[, "solution_60"])
sr40_60$run <- "solution_60"
sr40_61 <- feature_representation(r40, sr40[, "solution_61"])
sr40_61$run <- "solution_61"
sr40_62 <- feature_representation(r40, sr40[, "solution_62"])
sr40_62$run <- "solution_62"
sr40_63 <- feature_representation(r40, sr40[, "solution_63"])
sr40_63$run <- "solution_63"
sr40_64 <- feature_representation(r40, sr40[, "solution_64"])
sr40_64$run <- "solution_64"
sr40_65 <- feature_representation(r40, sr40[, "solution_65"])
sr40_65$run <- "solution_65"
sr40_66 <- feature_representation(r40, sr40[, "solution_66"])
sr40_66$run <- "solution_66"
sr40_67 <- feature_representation(r40, sr40[, "solution_67"])
sr40_67$run <- "solution_67"
sr40_68 <- feature_representation(r40, sr40[, "solution_68"])
sr40_68$run <- "solution_68"
sr40_69 <- feature_representation(r40, sr40[, "solution_69"])
sr40_69$run <- "solution_69"
sr40_70 <- feature_representation(r40, sr40[, "solution_70"])
sr40_70$run <- "solution_70"
sr40_71 <- feature_representation(r40, sr40[, "solution_71"])
sr40_71$run <- "solution_71"
sr40_72 <- feature_representation(r40, sr40[, "solution_72"])
sr40_72$run <- "solution_72"
sr40_73 <- feature_representation(r40, sr40[, "solution_73"])
sr40_73$run <- "solution_73"
sr40_74 <- feature_representation(r40, sr40[, "solution_74"])
sr40_74$run <- "solution_74"
sr40_75 <- feature_representation(r40, sr40[, "solution_75"])
sr40_75$run <- "solution_75"
sr40_76 <- feature_representation(r40, sr40[, "solution_76"])
sr40_76$run <- "solution_76"
sr40_77 <- feature_representation(r40, sr40[, "solution_77"])
sr40_77$run <- "solution_77"
sr40_78 <- feature_representation(r40, sr40[, "solution_78"])
sr40_78$run <- "solution_78"
sr40_79 <- feature_representation(r40, sr40[, "solution_79"])
sr40_79$run <- "solution_79"
sr40_80 <- feature_representation(r40, sr40[, "solution_80"])
sr40_80$run <- "solution_80"
sr40_81 <- feature_representation(r40, sr40[, "solution_81"])
sr40_81$run <- "solution_81"
sr40_82 <- feature_representation(r40, sr40[, "solution_82"])
sr40_82$run <- "solution_82"
sr40_83 <- feature_representation(r40, sr40[, "solution_83"])
sr40_83$run <- "solution_83"
sr40_84 <- feature_representation(r40, sr40[, "solution_84"])
sr40_84$run <- "solution_84"
sr40_85 <- feature_representation(r40, sr40[, "solution_85"])
sr40_85$run <- "solution_85"
sr40_86 <- feature_representation(r40, sr40[, "solution_86"])
sr40_86$run <- "solution_86"
sr40_87 <- feature_representation(r40, sr40[, "solution_87"])
sr40_87$run <- "solution_87"
sr40_88 <- feature_representation(r40, sr40[, "solution_88"])
sr40_88$run <- "solution_88"
sr40_89 <- feature_representation(r40, sr40[, "solution_89"])
sr40_89$run <- "solution_89"
sr40_90 <- feature_representation(r40, sr40[, "solution_90"])
sr40_90$run <- "solution_90"
sr40_91 <- feature_representation(r40, sr40[, "solution_91"])
sr40_91$run <- "solution_91"
sr40_92 <- feature_representation(r40, sr40[, "solution_92"])
sr40_92$run <- "solution_92"
sr40_93 <- feature_representation(r40, sr40[, "solution_93"])
sr40_93$run <- "solution_93"
sr40_94 <- feature_representation(r40, sr40[, "solution_94"])
sr40_94$run <- "solution_94"
sr40_95 <- feature_representation(r40, sr40[, "solution_95"])
sr40_95$run <- "solution_95"
sr40_96 <- feature_representation(r40, sr40[, "solution_96"])
sr40_96$run <- "solution_96"
sr40_97 <- feature_representation(r40, sr40[, "solution_97"])
sr40_97$run <- "solution_97"
sr40_98 <- feature_representation(r40, sr40[, "solution_98"])
sr40_98$run <- "solution_98"
sr40_99 <- feature_representation(r40, sr40[, "solution_99"])
sr40_99$run <- "solution_99"
sr40_100 <- feature_representation(r40, sr40[, "solution_100"])
sr40_100$run <- "solution_100"
sr40_r<-bind_rows(sr40_1,sr40_2,sr40_3,sr40_4,sr40_5,sr40_6,sr40_7,sr40_8,sr40_9,sr40_10,sr40_11,sr40_12,sr40_13,sr40_14,sr40_15,sr40_16,sr40_17,sr40_18,sr40_19,sr40_20,sr40_21,sr40_22,sr40_23,sr40_24,sr40_25,sr40_26,sr40_27,sr40_28,sr40_29,sr40_30,sr40_31,sr40_32,sr40_33,sr40_34,sr40_35,sr40_36,sr40_37,sr40_38,sr40_39,sr40_40,sr40_41,sr40_42,sr40_43,sr40_44,sr40_45,sr40_46,sr40_47,sr40_48,sr40_49,sr40_50,sr40_51,sr40_52,sr40_53,sr40_54,sr40_55,sr40_56,sr40_57,sr40_58,sr40_59,sr40_60,sr40_61,sr40_62,sr40_63,sr40_64,sr40_65,sr40_66,sr40_67,sr40_68,sr40_69,sr40_70,sr40_71,sr40_72,sr40_73,sr40_74,sr40_75,sr40_76,sr40_77,sr40_78,sr40_79,sr40_80,sr40_81,sr40_82,sr40_83,sr40_84,sr40_85,sr40_86,sr40_87,sr40_88,sr40_89,sr40_90,sr40_91,sr40_92,sr40_93,sr40_94,sr40_95,sr40_96,sr40_97,sr40_98,sr40_99,sr40_100)
sr40_r$objective <- "Random"
sr40_r$species<-"40"


feature_a <- feature_abundances(r40, na.rm = FALSE)

sr40r<-left_join(sr40_r,feature_a)


#45
pu_dat45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r45 <- problem(pu_dat45, spec_dat45, cost_column = "cost", rij = puvsp_dat45) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat45$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr45 <- solve(r45)

sr45<-as.data.frame(sr45) %>% as_tibble()

sr45$objective<-"Random"
sr45$solution<-"45"

# Features Dataset
sr45_1 <- feature_representation(r45, sr45[, "solution_1"])
sr45_1$run <- "solution_1"
sr45_2 <- feature_representation(r45, sr45[, "solution_2"])
sr45_2$run <- "solution_2"
sr45_3 <- feature_representation(r45, sr45[, "solution_3"])
sr45_3$run <- "solution_3"
sr45_4 <- feature_representation(r45, sr45[, "solution_4"])
sr45_4$run <- "solution_4"
sr45_5 <- feature_representation(r45, sr45[, "solution_5"])
sr45_5$run <- "solution_5"
sr45_6 <- feature_representation(r45, sr45[, "solution_6"])
sr45_6$run <- "solution_6"
sr45_7 <- feature_representation(r45, sr45[, "solution_7"])
sr45_7$run <- "solution_7"
sr45_8 <- feature_representation(r45, sr45[, "solution_8"])
sr45_8$run <- "solution_8"
sr45_9 <- feature_representation(r45, sr45[, "solution_9"])
sr45_9$run <- "solution_9"
sr45_10 <- feature_representation(r45, sr45[, "solution_10"])
sr45_10$run <- "solution_10"
sr45_11 <- feature_representation(r45, sr45[, "solution_11"])
sr45_11$run <- "solution_11"
sr45_12 <- feature_representation(r45, sr45[, "solution_12"])
sr45_12$run <- "solution_12"
sr45_13 <- feature_representation(r45, sr45[, "solution_13"])
sr45_13$run <- "solution_13"
sr45_14 <- feature_representation(r45, sr45[, "solution_14"])
sr45_14$run <- "solution_14"
sr45_15 <- feature_representation(r45, sr45[, "solution_15"])
sr45_15$run <- "solution_15"
sr45_16 <- feature_representation(r45, sr45[, "solution_16"])
sr45_16$run <- "solution_16"
sr45_17 <- feature_representation(r45, sr45[, "solution_17"])
sr45_17$run <- "solution_17"
sr45_18 <- feature_representation(r45, sr45[, "solution_18"])
sr45_18$run <- "solution_18"
sr45_19 <- feature_representation(r45, sr45[, "solution_19"])
sr45_19$run <- "solution_19"
sr45_20 <- feature_representation(r45, sr45[, "solution_20"])
sr45_20$run <- "solution_20"
sr45_21 <- feature_representation(r45, sr45[, "solution_21"])
sr45_21$run <- "solution_21"
sr45_22 <- feature_representation(r45, sr45[, "solution_22"])
sr45_22$run <- "solution_22"
sr45_23 <- feature_representation(r45, sr45[, "solution_23"])
sr45_23$run <- "solution_23"
sr45_24 <- feature_representation(r45, sr45[, "solution_24"])
sr45_24$run <- "solution_24"
sr45_25 <- feature_representation(r45, sr45[, "solution_25"])
sr45_25$run <- "solution_25"
sr45_26 <- feature_representation(r45, sr45[, "solution_26"])
sr45_26$run <- "solution_26"
sr45_27 <- feature_representation(r45, sr45[, "solution_27"])
sr45_27$run <- "solution_27"
sr45_28 <- feature_representation(r45, sr45[, "solution_28"])
sr45_28$run <- "solution_28"
sr45_29 <- feature_representation(r45, sr45[, "solution_29"])
sr45_29$run <- "solution_29"
sr45_30 <- feature_representation(r45, sr45[, "solution_30"])
sr45_30$run <- "solution_30"
sr45_31 <- feature_representation(r45, sr45[, "solution_31"])
sr45_31$run <- "solution_31"
sr45_32 <- feature_representation(r45, sr45[, "solution_32"])
sr45_32$run <- "solution_32"
sr45_33 <- feature_representation(r45, sr45[, "solution_33"])
sr45_33$run <- "solution_33"
sr45_34 <- feature_representation(r45, sr45[, "solution_34"])
sr45_34$run <- "solution_34"
sr45_35 <- feature_representation(r45, sr45[, "solution_35"])
sr45_35$run <- "solution_35"
sr45_36 <- feature_representation(r45, sr45[, "solution_36"])
sr45_36$run <- "solution_36"
sr45_37 <- feature_representation(r45, sr45[, "solution_37"])
sr45_37$run <- "solution_37"
sr45_38 <- feature_representation(r45, sr45[, "solution_38"])
sr45_38$run <- "solution_38"
sr45_39 <- feature_representation(r45, sr45[, "solution_39"])
sr45_39$run <- "solution_39"
sr45_40 <- feature_representation(r45, sr45[, "solution_40"])
sr45_40$run <- "solution_40"
sr45_41 <- feature_representation(r45, sr45[, "solution_41"])
sr45_41$run <- "solution_41"
sr45_42 <- feature_representation(r45, sr45[, "solution_42"])
sr45_42$run <- "solution_42"
sr45_43 <- feature_representation(r45, sr45[, "solution_43"])
sr45_43$run <- "solution_43"
sr45_44 <- feature_representation(r45, sr45[, "solution_44"])
sr45_44$run <- "solution_44"
sr45_45 <- feature_representation(r45, sr45[, "solution_45"])
sr45_45$run <- "solution_45"
sr45_46 <- feature_representation(r45, sr45[, "solution_46"])
sr45_46$run <- "solution_46"
sr45_47 <- feature_representation(r45, sr45[, "solution_47"])
sr45_47$run <- "solution_47"
sr45_48 <- feature_representation(r45, sr45[, "solution_48"])
sr45_48$run <- "solution_48"
sr45_49 <- feature_representation(r45, sr45[, "solution_49"])
sr45_49$run <- "solution_49"
sr45_50 <- feature_representation(r45, sr45[, "solution_50"])
sr45_50$run <- "solution_50"
sr45_51 <- feature_representation(r45, sr45[, "solution_51"])
sr45_51$run <- "solution_51"
sr45_52 <- feature_representation(r45, sr45[, "solution_52"])
sr45_52$run <- "solution_52"
sr45_53 <- feature_representation(r45, sr45[, "solution_53"])
sr45_53$run <- "solution_53"
sr45_54 <- feature_representation(r45, sr45[, "solution_54"])
sr45_54$run <- "solution_54"
sr45_55 <- feature_representation(r45, sr45[, "solution_55"])
sr45_55$run <- "solution_55"
sr45_56 <- feature_representation(r45, sr45[, "solution_56"])
sr45_56$run <- "solution_56"
sr45_57 <- feature_representation(r45, sr45[, "solution_57"])
sr45_57$run <- "solution_57"
sr45_58 <- feature_representation(r45, sr45[, "solution_58"])
sr45_58$run <- "solution_58"
sr45_59 <- feature_representation(r45, sr45[, "solution_59"])
sr45_59$run <- "solution_59"
sr45_60 <- feature_representation(r45, sr45[, "solution_60"])
sr45_60$run <- "solution_60"
sr45_61 <- feature_representation(r45, sr45[, "solution_61"])
sr45_61$run <- "solution_61"
sr45_62 <- feature_representation(r45, sr45[, "solution_62"])
sr45_62$run <- "solution_62"
sr45_63 <- feature_representation(r45, sr45[, "solution_63"])
sr45_63$run <- "solution_63"
sr45_64 <- feature_representation(r45, sr45[, "solution_64"])
sr45_64$run <- "solution_64"
sr45_65 <- feature_representation(r45, sr45[, "solution_65"])
sr45_65$run <- "solution_65"
sr45_66 <- feature_representation(r45, sr45[, "solution_66"])
sr45_66$run <- "solution_66"
sr45_67 <- feature_representation(r45, sr45[, "solution_67"])
sr45_67$run <- "solution_67"
sr45_68 <- feature_representation(r45, sr45[, "solution_68"])
sr45_68$run <- "solution_68"
sr45_69 <- feature_representation(r45, sr45[, "solution_69"])
sr45_69$run <- "solution_69"
sr45_70 <- feature_representation(r45, sr45[, "solution_70"])
sr45_70$run <- "solution_70"
sr45_71 <- feature_representation(r45, sr45[, "solution_71"])
sr45_71$run <- "solution_71"
sr45_72 <- feature_representation(r45, sr45[, "solution_72"])
sr45_72$run <- "solution_72"
sr45_73 <- feature_representation(r45, sr45[, "solution_73"])
sr45_73$run <- "solution_73"
sr45_74 <- feature_representation(r45, sr45[, "solution_74"])
sr45_74$run <- "solution_74"
sr45_75 <- feature_representation(r45, sr45[, "solution_75"])
sr45_75$run <- "solution_75"
sr45_76 <- feature_representation(r45, sr45[, "solution_76"])
sr45_76$run <- "solution_76"
sr45_77 <- feature_representation(r45, sr45[, "solution_77"])
sr45_77$run <- "solution_77"
sr45_78 <- feature_representation(r45, sr45[, "solution_78"])
sr45_78$run <- "solution_78"
sr45_79 <- feature_representation(r45, sr45[, "solution_79"])
sr45_79$run <- "solution_79"
sr45_80 <- feature_representation(r45, sr45[, "solution_80"])
sr45_80$run <- "solution_80"
sr45_81 <- feature_representation(r45, sr45[, "solution_81"])
sr45_81$run <- "solution_81"
sr45_82 <- feature_representation(r45, sr45[, "solution_82"])
sr45_82$run <- "solution_82"
sr45_83 <- feature_representation(r45, sr45[, "solution_83"])
sr45_83$run <- "solution_83"
sr45_84 <- feature_representation(r45, sr45[, "solution_84"])
sr45_84$run <- "solution_84"
sr45_85 <- feature_representation(r45, sr45[, "solution_85"])
sr45_85$run <- "solution_85"
sr45_86 <- feature_representation(r45, sr45[, "solution_86"])
sr45_86$run <- "solution_86"
sr45_87 <- feature_representation(r45, sr45[, "solution_87"])
sr45_87$run <- "solution_87"
sr45_88 <- feature_representation(r45, sr45[, "solution_88"])
sr45_88$run <- "solution_88"
sr45_89 <- feature_representation(r45, sr45[, "solution_89"])
sr45_89$run <- "solution_89"
sr45_90 <- feature_representation(r45, sr45[, "solution_90"])
sr45_90$run <- "solution_90"
sr45_91 <- feature_representation(r45, sr45[, "solution_91"])
sr45_91$run <- "solution_91"
sr45_92 <- feature_representation(r45, sr45[, "solution_92"])
sr45_92$run <- "solution_92"
sr45_93 <- feature_representation(r45, sr45[, "solution_93"])
sr45_93$run <- "solution_93"
sr45_94 <- feature_representation(r45, sr45[, "solution_94"])
sr45_94$run <- "solution_94"
sr45_95 <- feature_representation(r45, sr45[, "solution_95"])
sr45_95$run <- "solution_95"
sr45_96 <- feature_representation(r45, sr45[, "solution_96"])
sr45_96$run <- "solution_96"
sr45_97 <- feature_representation(r45, sr45[, "solution_97"])
sr45_97$run <- "solution_97"
sr45_98 <- feature_representation(r45, sr45[, "solution_98"])
sr45_98$run <- "solution_98"
sr45_99 <- feature_representation(r45, sr45[, "solution_99"])
sr45_99$run <- "solution_99"
sr45_100 <- feature_representation(r45, sr45[, "solution_100"])
sr45_100$run <- "solution_100"
sr45_r<-bind_rows(sr45_1,sr45_2,sr45_3,sr45_4,sr45_5,sr45_6,sr45_7,sr45_8,sr45_9,sr45_10,sr45_11,sr45_12,sr45_13,sr45_14,sr45_15,sr45_16,sr45_17,sr45_18,sr45_19,sr45_20,sr45_21,sr45_22,sr45_23,sr45_24,sr45_25,sr45_26,sr45_27,sr45_28,sr45_29,sr45_30,sr45_31,sr45_32,sr45_33,sr45_34,sr45_35,sr45_36,sr45_37,sr45_38,sr45_39,sr45_40,sr45_41,sr45_42,sr45_43,sr45_44,sr45_45,sr45_46,sr45_47,sr45_48,sr45_49,sr45_50,sr45_51,sr45_52,sr45_53,sr45_54,sr45_55,sr45_56,sr45_57,sr45_58,sr45_59,sr45_60,sr45_61,sr45_62,sr45_63,sr45_64,sr45_65,sr45_66,sr45_67,sr45_68,sr45_69,sr45_70,sr45_71,sr45_72,sr45_73,sr45_74,sr45_75,sr45_76,sr45_77,sr45_78,sr45_79,sr45_80,sr45_81,sr45_82,sr45_83,sr45_84,sr45_85,sr45_86,sr45_87,sr45_88,sr45_89,sr45_90,sr45_91,sr45_92,sr45_93,sr45_94,sr45_95,sr45_96,sr45_97,sr45_98,sr45_99,sr45_100)
sr45_r$objective <- "Random"
sr45_r$species<-"45"


feature_a <- feature_abundances(r45, na.rm = FALSE)

sr45r<-left_join(sr45_r,feature_a)


#50
pu_dat50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r50 <- problem(pu_dat50, spec_dat50, cost_column = "cost", rij = puvsp_dat50) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat50$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr50 <- solve(r50)

sr50<-as.data.frame(sr50) %>% as_tibble()

sr50$objective<-"Random"
sr50$solution<-"50"

# Features Dataset
sr50_1 <- feature_representation(r50, sr50[, "solution_1"])
sr50_1$run <- "solution_1"
sr50_2 <- feature_representation(r50, sr50[, "solution_2"])
sr50_2$run <- "solution_2"
sr50_3 <- feature_representation(r50, sr50[, "solution_3"])
sr50_3$run <- "solution_3"
sr50_4 <- feature_representation(r50, sr50[, "solution_4"])
sr50_4$run <- "solution_4"
sr50_5 <- feature_representation(r50, sr50[, "solution_5"])
sr50_5$run <- "solution_5"
sr50_6 <- feature_representation(r50, sr50[, "solution_6"])
sr50_6$run <- "solution_6"
sr50_7 <- feature_representation(r50, sr50[, "solution_7"])
sr50_7$run <- "solution_7"
sr50_8 <- feature_representation(r50, sr50[, "solution_8"])
sr50_8$run <- "solution_8"
sr50_9 <- feature_representation(r50, sr50[, "solution_9"])
sr50_9$run <- "solution_9"
sr50_10 <- feature_representation(r50, sr50[, "solution_10"])
sr50_10$run <- "solution_10"
sr50_11 <- feature_representation(r50, sr50[, "solution_11"])
sr50_11$run <- "solution_11"
sr50_12 <- feature_representation(r50, sr50[, "solution_12"])
sr50_12$run <- "solution_12"
sr50_13 <- feature_representation(r50, sr50[, "solution_13"])
sr50_13$run <- "solution_13"
sr50_14 <- feature_representation(r50, sr50[, "solution_14"])
sr50_14$run <- "solution_14"
sr50_15 <- feature_representation(r50, sr50[, "solution_15"])
sr50_15$run <- "solution_15"
sr50_16 <- feature_representation(r50, sr50[, "solution_16"])
sr50_16$run <- "solution_16"
sr50_17 <- feature_representation(r50, sr50[, "solution_17"])
sr50_17$run <- "solution_17"
sr50_18 <- feature_representation(r50, sr50[, "solution_18"])
sr50_18$run <- "solution_18"
sr50_19 <- feature_representation(r50, sr50[, "solution_19"])
sr50_19$run <- "solution_19"
sr50_20 <- feature_representation(r50, sr50[, "solution_20"])
sr50_20$run <- "solution_20"
sr50_21 <- feature_representation(r50, sr50[, "solution_21"])
sr50_21$run <- "solution_21"
sr50_22 <- feature_representation(r50, sr50[, "solution_22"])
sr50_22$run <- "solution_22"
sr50_23 <- feature_representation(r50, sr50[, "solution_23"])
sr50_23$run <- "solution_23"
sr50_24 <- feature_representation(r50, sr50[, "solution_24"])
sr50_24$run <- "solution_24"
sr50_25 <- feature_representation(r50, sr50[, "solution_25"])
sr50_25$run <- "solution_25"
sr50_26 <- feature_representation(r50, sr50[, "solution_26"])
sr50_26$run <- "solution_26"
sr50_27 <- feature_representation(r50, sr50[, "solution_27"])
sr50_27$run <- "solution_27"
sr50_28 <- feature_representation(r50, sr50[, "solution_28"])
sr50_28$run <- "solution_28"
sr50_29 <- feature_representation(r50, sr50[, "solution_29"])
sr50_29$run <- "solution_29"
sr50_30 <- feature_representation(r50, sr50[, "solution_30"])
sr50_30$run <- "solution_30"
sr50_31 <- feature_representation(r50, sr50[, "solution_31"])
sr50_31$run <- "solution_31"
sr50_32 <- feature_representation(r50, sr50[, "solution_32"])
sr50_32$run <- "solution_32"
sr50_33 <- feature_representation(r50, sr50[, "solution_33"])
sr50_33$run <- "solution_33"
sr50_34 <- feature_representation(r50, sr50[, "solution_34"])
sr50_34$run <- "solution_34"
sr50_35 <- feature_representation(r50, sr50[, "solution_35"])
sr50_35$run <- "solution_35"
sr50_36 <- feature_representation(r50, sr50[, "solution_36"])
sr50_36$run <- "solution_36"
sr50_37 <- feature_representation(r50, sr50[, "solution_37"])
sr50_37$run <- "solution_37"
sr50_38 <- feature_representation(r50, sr50[, "solution_38"])
sr50_38$run <- "solution_38"
sr50_39 <- feature_representation(r50, sr50[, "solution_39"])
sr50_39$run <- "solution_39"
sr50_40 <- feature_representation(r50, sr50[, "solution_40"])
sr50_40$run <- "solution_40"
sr50_41 <- feature_representation(r50, sr50[, "solution_41"])
sr50_41$run <- "solution_41"
sr50_42 <- feature_representation(r50, sr50[, "solution_42"])
sr50_42$run <- "solution_42"
sr50_43 <- feature_representation(r50, sr50[, "solution_43"])
sr50_43$run <- "solution_43"
sr50_44 <- feature_representation(r50, sr50[, "solution_44"])
sr50_44$run <- "solution_44"
sr50_45 <- feature_representation(r50, sr50[, "solution_45"])
sr50_45$run <- "solution_45"
sr50_46 <- feature_representation(r50, sr50[, "solution_46"])
sr50_46$run <- "solution_46"
sr50_47 <- feature_representation(r50, sr50[, "solution_47"])
sr50_47$run <- "solution_47"
sr50_48 <- feature_representation(r50, sr50[, "solution_48"])
sr50_48$run <- "solution_48"
sr50_49 <- feature_representation(r50, sr50[, "solution_49"])
sr50_49$run <- "solution_49"
sr50_50 <- feature_representation(r50, sr50[, "solution_50"])
sr50_50$run <- "solution_50"
sr50_51 <- feature_representation(r50, sr50[, "solution_51"])
sr50_51$run <- "solution_51"
sr50_52 <- feature_representation(r50, sr50[, "solution_52"])
sr50_52$run <- "solution_52"
sr50_53 <- feature_representation(r50, sr50[, "solution_53"])
sr50_53$run <- "solution_53"
sr50_54 <- feature_representation(r50, sr50[, "solution_54"])
sr50_54$run <- "solution_54"
sr50_55 <- feature_representation(r50, sr50[, "solution_55"])
sr50_55$run <- "solution_55"
sr50_56 <- feature_representation(r50, sr50[, "solution_56"])
sr50_56$run <- "solution_56"
sr50_57 <- feature_representation(r50, sr50[, "solution_57"])
sr50_57$run <- "solution_57"
sr50_58 <- feature_representation(r50, sr50[, "solution_58"])
sr50_58$run <- "solution_58"
sr50_59 <- feature_representation(r50, sr50[, "solution_59"])
sr50_59$run <- "solution_59"
sr50_60 <- feature_representation(r50, sr50[, "solution_60"])
sr50_60$run <- "solution_60"
sr50_61 <- feature_representation(r50, sr50[, "solution_61"])
sr50_61$run <- "solution_61"
sr50_62 <- feature_representation(r50, sr50[, "solution_62"])
sr50_62$run <- "solution_62"
sr50_63 <- feature_representation(r50, sr50[, "solution_63"])
sr50_63$run <- "solution_63"
sr50_64 <- feature_representation(r50, sr50[, "solution_64"])
sr50_64$run <- "solution_64"
sr50_65 <- feature_representation(r50, sr50[, "solution_65"])
sr50_65$run <- "solution_65"
sr50_66 <- feature_representation(r50, sr50[, "solution_66"])
sr50_66$run <- "solution_66"
sr50_67 <- feature_representation(r50, sr50[, "solution_67"])
sr50_67$run <- "solution_67"
sr50_68 <- feature_representation(r50, sr50[, "solution_68"])
sr50_68$run <- "solution_68"
sr50_69 <- feature_representation(r50, sr50[, "solution_69"])
sr50_69$run <- "solution_69"
sr50_70 <- feature_representation(r50, sr50[, "solution_70"])
sr50_70$run <- "solution_70"
sr50_71 <- feature_representation(r50, sr50[, "solution_71"])
sr50_71$run <- "solution_71"
sr50_72 <- feature_representation(r50, sr50[, "solution_72"])
sr50_72$run <- "solution_72"
sr50_73 <- feature_representation(r50, sr50[, "solution_73"])
sr50_73$run <- "solution_73"
sr50_74 <- feature_representation(r50, sr50[, "solution_74"])
sr50_74$run <- "solution_74"
sr50_75 <- feature_representation(r50, sr50[, "solution_75"])
sr50_75$run <- "solution_75"
sr50_76 <- feature_representation(r50, sr50[, "solution_76"])
sr50_76$run <- "solution_76"
sr50_77 <- feature_representation(r50, sr50[, "solution_77"])
sr50_77$run <- "solution_77"
sr50_78 <- feature_representation(r50, sr50[, "solution_78"])
sr50_78$run <- "solution_78"
sr50_79 <- feature_representation(r50, sr50[, "solution_79"])
sr50_79$run <- "solution_79"
sr50_80 <- feature_representation(r50, sr50[, "solution_80"])
sr50_80$run <- "solution_80"
sr50_81 <- feature_representation(r50, sr50[, "solution_81"])
sr50_81$run <- "solution_81"
sr50_82 <- feature_representation(r50, sr50[, "solution_82"])
sr50_82$run <- "solution_82"
sr50_83 <- feature_representation(r50, sr50[, "solution_83"])
sr50_83$run <- "solution_83"
sr50_84 <- feature_representation(r50, sr50[, "solution_84"])
sr50_84$run <- "solution_84"
sr50_85 <- feature_representation(r50, sr50[, "solution_85"])
sr50_85$run <- "solution_85"
sr50_86 <- feature_representation(r50, sr50[, "solution_86"])
sr50_86$run <- "solution_86"
sr50_87 <- feature_representation(r50, sr50[, "solution_87"])
sr50_87$run <- "solution_87"
sr50_88 <- feature_representation(r50, sr50[, "solution_88"])
sr50_88$run <- "solution_88"
sr50_89 <- feature_representation(r50, sr50[, "solution_89"])
sr50_89$run <- "solution_89"
sr50_90 <- feature_representation(r50, sr50[, "solution_90"])
sr50_90$run <- "solution_90"
sr50_91 <- feature_representation(r50, sr50[, "solution_91"])
sr50_91$run <- "solution_91"
sr50_92 <- feature_representation(r50, sr50[, "solution_92"])
sr50_92$run <- "solution_92"
sr50_93 <- feature_representation(r50, sr50[, "solution_93"])
sr50_93$run <- "solution_93"
sr50_94 <- feature_representation(r50, sr50[, "solution_94"])
sr50_94$run <- "solution_94"
sr50_95 <- feature_representation(r50, sr50[, "solution_95"])
sr50_95$run <- "solution_95"
sr50_96 <- feature_representation(r50, sr50[, "solution_96"])
sr50_96$run <- "solution_96"
sr50_97 <- feature_representation(r50, sr50[, "solution_97"])
sr50_97$run <- "solution_97"
sr50_98 <- feature_representation(r50, sr50[, "solution_98"])
sr50_98$run <- "solution_98"
sr50_99 <- feature_representation(r50, sr50[, "solution_99"])
sr50_99$run <- "solution_99"
sr50_100 <- feature_representation(r50, sr50[, "solution_100"])
sr50_100$run <- "solution_100"
sr50_r<-bind_rows(sr50_1,sr50_2,sr50_3,sr50_4,sr50_5,sr50_6,sr50_7,sr50_8,sr50_9,sr50_10,sr50_11,sr50_12,sr50_13,sr50_14,sr50_15,sr50_16,sr50_17,sr50_18,sr50_19,sr50_20,sr50_21,sr50_22,sr50_23,sr50_24,sr50_25,sr50_26,sr50_27,sr50_28,sr50_29,sr50_30,sr50_31,sr50_32,sr50_33,sr50_34,sr50_35,sr50_36,sr50_37,sr50_38,sr50_39,sr50_40,sr50_41,sr50_42,sr50_43,sr50_44,sr50_45,sr50_46,sr50_47,sr50_48,sr50_49,sr50_50,sr50_51,sr50_52,sr50_53,sr50_54,sr50_55,sr50_56,sr50_57,sr50_58,sr50_59,sr50_60,sr50_61,sr50_62,sr50_63,sr50_64,sr50_65,sr50_66,sr50_67,sr50_68,sr50_69,sr50_70,sr50_71,sr50_72,sr50_73,sr50_74,sr50_75,sr50_76,sr50_77,sr50_78,sr50_79,sr50_80,sr50_81,sr50_82,sr50_83,sr50_84,sr50_85,sr50_86,sr50_87,sr50_88,sr50_89,sr50_90,sr50_91,sr50_92,sr50_93,sr50_94,sr50_95,sr50_96,sr50_97,sr50_98,sr50_99,sr50_100)
sr50_r$objective <- "Random"
sr50_r$species<-"50"


feature_a <- feature_abundances(r50, na.rm = FALSE)

sr50r<-left_join(sr50_r,feature_a)


#55
pu_dat55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r55 <- problem(pu_dat55, spec_dat55, cost_column = "cost", rij = puvsp_dat55) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat55$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr55 <- solve(r55)

sr55<-as.data.frame(sr55) %>% as_tibble()

sr55$objective<-"Random"
sr55$solution<-"55"


# Features Dataset
sr55_1 <- feature_representation(r55, sr55[, "solution_1"])
sr55_1$run <- "solution_1"
sr55_2 <- feature_representation(r55, sr55[, "solution_2"])
sr55_2$run <- "solution_2"
sr55_3 <- feature_representation(r55, sr55[, "solution_3"])
sr55_3$run <- "solution_3"
sr55_4 <- feature_representation(r55, sr55[, "solution_4"])
sr55_4$run <- "solution_4"
sr55_5 <- feature_representation(r55, sr55[, "solution_5"])
sr55_5$run <- "solution_5"
sr55_6 <- feature_representation(r55, sr55[, "solution_6"])
sr55_6$run <- "solution_6"
sr55_7 <- feature_representation(r55, sr55[, "solution_7"])
sr55_7$run <- "solution_7"
sr55_8 <- feature_representation(r55, sr55[, "solution_8"])
sr55_8$run <- "solution_8"
sr55_9 <- feature_representation(r55, sr55[, "solution_9"])
sr55_9$run <- "solution_9"
sr55_10 <- feature_representation(r55, sr55[, "solution_10"])
sr55_10$run <- "solution_10"
sr55_11 <- feature_representation(r55, sr55[, "solution_11"])
sr55_11$run <- "solution_11"
sr55_12 <- feature_representation(r55, sr55[, "solution_12"])
sr55_12$run <- "solution_12"
sr55_13 <- feature_representation(r55, sr55[, "solution_13"])
sr55_13$run <- "solution_13"
sr55_14 <- feature_representation(r55, sr55[, "solution_14"])
sr55_14$run <- "solution_14"
sr55_15 <- feature_representation(r55, sr55[, "solution_15"])
sr55_15$run <- "solution_15"
sr55_16 <- feature_representation(r55, sr55[, "solution_16"])
sr55_16$run <- "solution_16"
sr55_17 <- feature_representation(r55, sr55[, "solution_17"])
sr55_17$run <- "solution_17"
sr55_18 <- feature_representation(r55, sr55[, "solution_18"])
sr55_18$run <- "solution_18"
sr55_19 <- feature_representation(r55, sr55[, "solution_19"])
sr55_19$run <- "solution_19"
sr55_20 <- feature_representation(r55, sr55[, "solution_20"])
sr55_20$run <- "solution_20"
sr55_21 <- feature_representation(r55, sr55[, "solution_21"])
sr55_21$run <- "solution_21"
sr55_22 <- feature_representation(r55, sr55[, "solution_22"])
sr55_22$run <- "solution_22"
sr55_23 <- feature_representation(r55, sr55[, "solution_23"])
sr55_23$run <- "solution_23"
sr55_24 <- feature_representation(r55, sr55[, "solution_24"])
sr55_24$run <- "solution_24"
sr55_25 <- feature_representation(r55, sr55[, "solution_25"])
sr55_25$run <- "solution_25"
sr55_26 <- feature_representation(r55, sr55[, "solution_26"])
sr55_26$run <- "solution_26"
sr55_27 <- feature_representation(r55, sr55[, "solution_27"])
sr55_27$run <- "solution_27"
sr55_28 <- feature_representation(r55, sr55[, "solution_28"])
sr55_28$run <- "solution_28"
sr55_29 <- feature_representation(r55, sr55[, "solution_29"])
sr55_29$run <- "solution_29"
sr55_30 <- feature_representation(r55, sr55[, "solution_30"])
sr55_30$run <- "solution_30"
sr55_31 <- feature_representation(r55, sr55[, "solution_31"])
sr55_31$run <- "solution_31"
sr55_32 <- feature_representation(r55, sr55[, "solution_32"])
sr55_32$run <- "solution_32"
sr55_33 <- feature_representation(r55, sr55[, "solution_33"])
sr55_33$run <- "solution_33"
sr55_34 <- feature_representation(r55, sr55[, "solution_34"])
sr55_34$run <- "solution_34"
sr55_35 <- feature_representation(r55, sr55[, "solution_35"])
sr55_35$run <- "solution_35"
sr55_36 <- feature_representation(r55, sr55[, "solution_36"])
sr55_36$run <- "solution_36"
sr55_37 <- feature_representation(r55, sr55[, "solution_37"])
sr55_37$run <- "solution_37"
sr55_38 <- feature_representation(r55, sr55[, "solution_38"])
sr55_38$run <- "solution_38"
sr55_39 <- feature_representation(r55, sr55[, "solution_39"])
sr55_39$run <- "solution_39"
sr55_40 <- feature_representation(r55, sr55[, "solution_40"])
sr55_40$run <- "solution_40"
sr55_41 <- feature_representation(r55, sr55[, "solution_41"])
sr55_41$run <- "solution_41"
sr55_42 <- feature_representation(r55, sr55[, "solution_42"])
sr55_42$run <- "solution_42"
sr55_43 <- feature_representation(r55, sr55[, "solution_43"])
sr55_43$run <- "solution_43"
sr55_44 <- feature_representation(r55, sr55[, "solution_44"])
sr55_44$run <- "solution_44"
sr55_45 <- feature_representation(r55, sr55[, "solution_45"])
sr55_45$run <- "solution_45"
sr55_46 <- feature_representation(r55, sr55[, "solution_46"])
sr55_46$run <- "solution_46"
sr55_47 <- feature_representation(r55, sr55[, "solution_47"])
sr55_47$run <- "solution_47"
sr55_48 <- feature_representation(r55, sr55[, "solution_48"])
sr55_48$run <- "solution_48"
sr55_49 <- feature_representation(r55, sr55[, "solution_49"])
sr55_49$run <- "solution_49"
sr55_50 <- feature_representation(r55, sr55[, "solution_50"])
sr55_50$run <- "solution_50"
sr55_51 <- feature_representation(r55, sr55[, "solution_51"])
sr55_51$run <- "solution_51"
sr55_52 <- feature_representation(r55, sr55[, "solution_52"])
sr55_52$run <- "solution_52"
sr55_53 <- feature_representation(r55, sr55[, "solution_53"])
sr55_53$run <- "solution_53"
sr55_54 <- feature_representation(r55, sr55[, "solution_54"])
sr55_54$run <- "solution_54"
sr55_55 <- feature_representation(r55, sr55[, "solution_55"])
sr55_55$run <- "solution_55"
sr55_56 <- feature_representation(r55, sr55[, "solution_56"])
sr55_56$run <- "solution_56"
sr55_57 <- feature_representation(r55, sr55[, "solution_57"])
sr55_57$run <- "solution_57"
sr55_58 <- feature_representation(r55, sr55[, "solution_58"])
sr55_58$run <- "solution_58"
sr55_59 <- feature_representation(r55, sr55[, "solution_59"])
sr55_59$run <- "solution_59"
sr55_60 <- feature_representation(r55, sr55[, "solution_60"])
sr55_60$run <- "solution_60"
sr55_61 <- feature_representation(r55, sr55[, "solution_61"])
sr55_61$run <- "solution_61"
sr55_62 <- feature_representation(r55, sr55[, "solution_62"])
sr55_62$run <- "solution_62"
sr55_63 <- feature_representation(r55, sr55[, "solution_63"])
sr55_63$run <- "solution_63"
sr55_64 <- feature_representation(r55, sr55[, "solution_64"])
sr55_64$run <- "solution_64"
sr55_65 <- feature_representation(r55, sr55[, "solution_65"])
sr55_65$run <- "solution_65"
sr55_66 <- feature_representation(r55, sr55[, "solution_66"])
sr55_66$run <- "solution_66"
sr55_67 <- feature_representation(r55, sr55[, "solution_67"])
sr55_67$run <- "solution_67"
sr55_68 <- feature_representation(r55, sr55[, "solution_68"])
sr55_68$run <- "solution_68"
sr55_69 <- feature_representation(r55, sr55[, "solution_69"])
sr55_69$run <- "solution_69"
sr55_70 <- feature_representation(r55, sr55[, "solution_70"])
sr55_70$run <- "solution_70"
sr55_71 <- feature_representation(r55, sr55[, "solution_71"])
sr55_71$run <- "solution_71"
sr55_72 <- feature_representation(r55, sr55[, "solution_72"])
sr55_72$run <- "solution_72"
sr55_73 <- feature_representation(r55, sr55[, "solution_73"])
sr55_73$run <- "solution_73"
sr55_74 <- feature_representation(r55, sr55[, "solution_74"])
sr55_74$run <- "solution_74"
sr55_75 <- feature_representation(r55, sr55[, "solution_75"])
sr55_75$run <- "solution_75"
sr55_76 <- feature_representation(r55, sr55[, "solution_76"])
sr55_76$run <- "solution_76"
sr55_77 <- feature_representation(r55, sr55[, "solution_77"])
sr55_77$run <- "solution_77"
sr55_78 <- feature_representation(r55, sr55[, "solution_78"])
sr55_78$run <- "solution_78"
sr55_79 <- feature_representation(r55, sr55[, "solution_79"])
sr55_79$run <- "solution_79"
sr55_80 <- feature_representation(r55, sr55[, "solution_80"])
sr55_80$run <- "solution_80"
sr55_81 <- feature_representation(r55, sr55[, "solution_81"])
sr55_81$run <- "solution_81"
sr55_82 <- feature_representation(r55, sr55[, "solution_82"])
sr55_82$run <- "solution_82"
sr55_83 <- feature_representation(r55, sr55[, "solution_83"])
sr55_83$run <- "solution_83"
sr55_84 <- feature_representation(r55, sr55[, "solution_84"])
sr55_84$run <- "solution_84"
sr55_85 <- feature_representation(r55, sr55[, "solution_85"])
sr55_85$run <- "solution_85"
sr55_86 <- feature_representation(r55, sr55[, "solution_86"])
sr55_86$run <- "solution_86"
sr55_87 <- feature_representation(r55, sr55[, "solution_87"])
sr55_87$run <- "solution_87"
sr55_88 <- feature_representation(r55, sr55[, "solution_88"])
sr55_88$run <- "solution_88"
sr55_89 <- feature_representation(r55, sr55[, "solution_89"])
sr55_89$run <- "solution_89"
sr55_90 <- feature_representation(r55, sr55[, "solution_90"])
sr55_90$run <- "solution_90"
sr55_91 <- feature_representation(r55, sr55[, "solution_91"])
sr55_91$run <- "solution_91"
sr55_92 <- feature_representation(r55, sr55[, "solution_92"])
sr55_92$run <- "solution_92"
sr55_93 <- feature_representation(r55, sr55[, "solution_93"])
sr55_93$run <- "solution_93"
sr55_94 <- feature_representation(r55, sr55[, "solution_94"])
sr55_94$run <- "solution_94"
sr55_95 <- feature_representation(r55, sr55[, "solution_95"])
sr55_95$run <- "solution_95"
sr55_96 <- feature_representation(r55, sr55[, "solution_96"])
sr55_96$run <- "solution_96"
sr55_97 <- feature_representation(r55, sr55[, "solution_97"])
sr55_97$run <- "solution_97"
sr55_98 <- feature_representation(r55, sr55[, "solution_98"])
sr55_98$run <- "solution_98"
sr55_99 <- feature_representation(r55, sr55[, "solution_99"])
sr55_99$run <- "solution_99"
sr55_100 <- feature_representation(r55, sr55[, "solution_100"])
sr55_100$run <- "solution_100"
sr55_r<-bind_rows(sr55_1,sr55_2,sr55_3,sr55_4,sr55_5,sr55_6,sr55_7,sr55_8,sr55_9,sr55_10,sr55_11,sr55_12,sr55_13,sr55_14,sr55_15,sr55_16,sr55_17,sr55_18,sr55_19,sr55_20,sr55_21,sr55_22,sr55_23,sr55_24,sr55_25,sr55_26,sr55_27,sr55_28,sr55_29,sr55_30,sr55_31,sr55_32,sr55_33,sr55_34,sr55_35,sr55_36,sr55_37,sr55_38,sr55_39,sr55_40,sr55_41,sr55_42,sr55_43,sr55_44,sr55_45,sr55_46,sr55_47,sr55_48,sr55_49,sr55_50,sr55_51,sr55_52,sr55_53,sr55_54,sr55_55,sr55_56,sr55_57,sr55_58,sr55_59,sr55_60,sr55_61,sr55_62,sr55_63,sr55_64,sr55_65,sr55_66,sr55_67,sr55_68,sr55_69,sr55_70,sr55_71,sr55_72,sr55_73,sr55_74,sr55_75,sr55_76,sr55_77,sr55_78,sr55_79,sr55_80,sr55_81,sr55_82,sr55_83,sr55_84,sr55_85,sr55_86,sr55_87,sr55_88,sr55_89,sr55_90,sr55_91,sr55_92,sr55_93,sr55_94,sr55_95,sr55_96,sr55_97,sr55_98,sr55_99,sr55_100)
sr55_r$objective <- "Random"
sr55_r$species<-"55"


feature_a <- feature_abundances(r55, na.rm = FALSE)

sr55r<-left_join(sr55_r,feature_a)


#60
pu_dat60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r60 <- problem(pu_dat60, spec_dat60, cost_column = "cost", rij = puvsp_dat60) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat60$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr60 <- solve(r60)

sr60<-as.data.frame(sr60) %>% as_tibble()

sr60$objective<-"Random"
sr60$solution<-"60"

# Features Dataset
sr60_1 <- feature_representation(r60, sr60[, "solution_1"])
sr60_1$run <- "solution_1"
sr60_2 <- feature_representation(r60, sr60[, "solution_2"])
sr60_2$run <- "solution_2"
sr60_3 <- feature_representation(r60, sr60[, "solution_3"])
sr60_3$run <- "solution_3"
sr60_4 <- feature_representation(r60, sr60[, "solution_4"])
sr60_4$run <- "solution_4"
sr60_5 <- feature_representation(r60, sr60[, "solution_5"])
sr60_5$run <- "solution_5"
sr60_6 <- feature_representation(r60, sr60[, "solution_6"])
sr60_6$run <- "solution_6"
sr60_7 <- feature_representation(r60, sr60[, "solution_7"])
sr60_7$run <- "solution_7"
sr60_8 <- feature_representation(r60, sr60[, "solution_8"])
sr60_8$run <- "solution_8"
sr60_9 <- feature_representation(r60, sr60[, "solution_9"])
sr60_9$run <- "solution_9"
sr60_10 <- feature_representation(r60, sr60[, "solution_10"])
sr60_10$run <- "solution_10"
sr60_11 <- feature_representation(r60, sr60[, "solution_11"])
sr60_11$run <- "solution_11"
sr60_12 <- feature_representation(r60, sr60[, "solution_12"])
sr60_12$run <- "solution_12"
sr60_13 <- feature_representation(r60, sr60[, "solution_13"])
sr60_13$run <- "solution_13"
sr60_14 <- feature_representation(r60, sr60[, "solution_14"])
sr60_14$run <- "solution_14"
sr60_15 <- feature_representation(r60, sr60[, "solution_15"])
sr60_15$run <- "solution_15"
sr60_16 <- feature_representation(r60, sr60[, "solution_16"])
sr60_16$run <- "solution_16"
sr60_17 <- feature_representation(r60, sr60[, "solution_17"])
sr60_17$run <- "solution_17"
sr60_18 <- feature_representation(r60, sr60[, "solution_18"])
sr60_18$run <- "solution_18"
sr60_19 <- feature_representation(r60, sr60[, "solution_19"])
sr60_19$run <- "solution_19"
sr60_20 <- feature_representation(r60, sr60[, "solution_20"])
sr60_20$run <- "solution_20"
sr60_21 <- feature_representation(r60, sr60[, "solution_21"])
sr60_21$run <- "solution_21"
sr60_22 <- feature_representation(r60, sr60[, "solution_22"])
sr60_22$run <- "solution_22"
sr60_23 <- feature_representation(r60, sr60[, "solution_23"])
sr60_23$run <- "solution_23"
sr60_24 <- feature_representation(r60, sr60[, "solution_24"])
sr60_24$run <- "solution_24"
sr60_25 <- feature_representation(r60, sr60[, "solution_25"])
sr60_25$run <- "solution_25"
sr60_26 <- feature_representation(r60, sr60[, "solution_26"])
sr60_26$run <- "solution_26"
sr60_27 <- feature_representation(r60, sr60[, "solution_27"])
sr60_27$run <- "solution_27"
sr60_28 <- feature_representation(r60, sr60[, "solution_28"])
sr60_28$run <- "solution_28"
sr60_29 <- feature_representation(r60, sr60[, "solution_29"])
sr60_29$run <- "solution_29"
sr60_30 <- feature_representation(r60, sr60[, "solution_30"])
sr60_30$run <- "solution_30"
sr60_31 <- feature_representation(r60, sr60[, "solution_31"])
sr60_31$run <- "solution_31"
sr60_32 <- feature_representation(r60, sr60[, "solution_32"])
sr60_32$run <- "solution_32"
sr60_33 <- feature_representation(r60, sr60[, "solution_33"])
sr60_33$run <- "solution_33"
sr60_34 <- feature_representation(r60, sr60[, "solution_34"])
sr60_34$run <- "solution_34"
sr60_35 <- feature_representation(r60, sr60[, "solution_35"])
sr60_35$run <- "solution_35"
sr60_36 <- feature_representation(r60, sr60[, "solution_36"])
sr60_36$run <- "solution_36"
sr60_37 <- feature_representation(r60, sr60[, "solution_37"])
sr60_37$run <- "solution_37"
sr60_38 <- feature_representation(r60, sr60[, "solution_38"])
sr60_38$run <- "solution_38"
sr60_39 <- feature_representation(r60, sr60[, "solution_39"])
sr60_39$run <- "solution_39"
sr60_40 <- feature_representation(r60, sr60[, "solution_40"])
sr60_40$run <- "solution_40"
sr60_41 <- feature_representation(r60, sr60[, "solution_41"])
sr60_41$run <- "solution_41"
sr60_42 <- feature_representation(r60, sr60[, "solution_42"])
sr60_42$run <- "solution_42"
sr60_43 <- feature_representation(r60, sr60[, "solution_43"])
sr60_43$run <- "solution_43"
sr60_44 <- feature_representation(r60, sr60[, "solution_44"])
sr60_44$run <- "solution_44"
sr60_45 <- feature_representation(r60, sr60[, "solution_45"])
sr60_45$run <- "solution_45"
sr60_46 <- feature_representation(r60, sr60[, "solution_46"])
sr60_46$run <- "solution_46"
sr60_47 <- feature_representation(r60, sr60[, "solution_47"])
sr60_47$run <- "solution_47"
sr60_48 <- feature_representation(r60, sr60[, "solution_48"])
sr60_48$run <- "solution_48"
sr60_49 <- feature_representation(r60, sr60[, "solution_49"])
sr60_49$run <- "solution_49"
sr60_50 <- feature_representation(r60, sr60[, "solution_50"])
sr60_50$run <- "solution_50"
sr60_51 <- feature_representation(r60, sr60[, "solution_51"])
sr60_51$run <- "solution_51"
sr60_52 <- feature_representation(r60, sr60[, "solution_52"])
sr60_52$run <- "solution_52"
sr60_53 <- feature_representation(r60, sr60[, "solution_53"])
sr60_53$run <- "solution_53"
sr60_54 <- feature_representation(r60, sr60[, "solution_54"])
sr60_54$run <- "solution_54"
sr60_55 <- feature_representation(r60, sr60[, "solution_55"])
sr60_55$run <- "solution_55"
sr60_56 <- feature_representation(r60, sr60[, "solution_56"])
sr60_56$run <- "solution_56"
sr60_57 <- feature_representation(r60, sr60[, "solution_57"])
sr60_57$run <- "solution_57"
sr60_58 <- feature_representation(r60, sr60[, "solution_58"])
sr60_58$run <- "solution_58"
sr60_59 <- feature_representation(r60, sr60[, "solution_59"])
sr60_59$run <- "solution_59"
sr60_60 <- feature_representation(r60, sr60[, "solution_60"])
sr60_60$run <- "solution_60"
sr60_61 <- feature_representation(r60, sr60[, "solution_61"])
sr60_61$run <- "solution_61"
sr60_62 <- feature_representation(r60, sr60[, "solution_62"])
sr60_62$run <- "solution_62"
sr60_63 <- feature_representation(r60, sr60[, "solution_63"])
sr60_63$run <- "solution_63"
sr60_64 <- feature_representation(r60, sr60[, "solution_64"])
sr60_64$run <- "solution_64"
sr60_65 <- feature_representation(r60, sr60[, "solution_65"])
sr60_65$run <- "solution_65"
sr60_66 <- feature_representation(r60, sr60[, "solution_66"])
sr60_66$run <- "solution_66"
sr60_67 <- feature_representation(r60, sr60[, "solution_67"])
sr60_67$run <- "solution_67"
sr60_68 <- feature_representation(r60, sr60[, "solution_68"])
sr60_68$run <- "solution_68"
sr60_69 <- feature_representation(r60, sr60[, "solution_69"])
sr60_69$run <- "solution_69"
sr60_70 <- feature_representation(r60, sr60[, "solution_70"])
sr60_70$run <- "solution_70"
sr60_71 <- feature_representation(r60, sr60[, "solution_71"])
sr60_71$run <- "solution_71"
sr60_72 <- feature_representation(r60, sr60[, "solution_72"])
sr60_72$run <- "solution_72"
sr60_73 <- feature_representation(r60, sr60[, "solution_73"])
sr60_73$run <- "solution_73"
sr60_74 <- feature_representation(r60, sr60[, "solution_74"])
sr60_74$run <- "solution_74"
sr60_75 <- feature_representation(r60, sr60[, "solution_75"])
sr60_75$run <- "solution_75"
sr60_76 <- feature_representation(r60, sr60[, "solution_76"])
sr60_76$run <- "solution_76"
sr60_77 <- feature_representation(r60, sr60[, "solution_77"])
sr60_77$run <- "solution_77"
sr60_78 <- feature_representation(r60, sr60[, "solution_78"])
sr60_78$run <- "solution_78"
sr60_79 <- feature_representation(r60, sr60[, "solution_79"])
sr60_79$run <- "solution_79"
sr60_80 <- feature_representation(r60, sr60[, "solution_80"])
sr60_80$run <- "solution_80"
sr60_81 <- feature_representation(r60, sr60[, "solution_81"])
sr60_81$run <- "solution_81"
sr60_82 <- feature_representation(r60, sr60[, "solution_82"])
sr60_82$run <- "solution_82"
sr60_83 <- feature_representation(r60, sr60[, "solution_83"])
sr60_83$run <- "solution_83"
sr60_84 <- feature_representation(r60, sr60[, "solution_84"])
sr60_84$run <- "solution_84"
sr60_85 <- feature_representation(r60, sr60[, "solution_85"])
sr60_85$run <- "solution_85"
sr60_86 <- feature_representation(r60, sr60[, "solution_86"])
sr60_86$run <- "solution_86"
sr60_87 <- feature_representation(r60, sr60[, "solution_87"])
sr60_87$run <- "solution_87"
sr60_88 <- feature_representation(r60, sr60[, "solution_88"])
sr60_88$run <- "solution_88"
sr60_89 <- feature_representation(r60, sr60[, "solution_89"])
sr60_89$run <- "solution_89"
sr60_90 <- feature_representation(r60, sr60[, "solution_90"])
sr60_90$run <- "solution_90"
sr60_91 <- feature_representation(r60, sr60[, "solution_91"])
sr60_91$run <- "solution_91"
sr60_92 <- feature_representation(r60, sr60[, "solution_92"])
sr60_92$run <- "solution_92"
sr60_93 <- feature_representation(r60, sr60[, "solution_93"])
sr60_93$run <- "solution_93"
sr60_94 <- feature_representation(r60, sr60[, "solution_94"])
sr60_94$run <- "solution_94"
sr60_95 <- feature_representation(r60, sr60[, "solution_95"])
sr60_95$run <- "solution_95"
sr60_96 <- feature_representation(r60, sr60[, "solution_96"])
sr60_96$run <- "solution_96"
sr60_97 <- feature_representation(r60, sr60[, "solution_97"])
sr60_97$run <- "solution_97"
sr60_98 <- feature_representation(r60, sr60[, "solution_98"])
sr60_98$run <- "solution_98"
sr60_99 <- feature_representation(r60, sr60[, "solution_99"])
sr60_99$run <- "solution_99"
sr60_100 <- feature_representation(r60, sr60[, "solution_100"])
sr60_100$run <- "solution_100"
sr60_r<-bind_rows(sr60_1,sr60_2,sr60_3,sr60_4,sr60_5,sr60_6,sr60_7,sr60_8,sr60_9,sr60_10,sr60_11,sr60_12,sr60_13,sr60_14,sr60_15,sr60_16,sr60_17,sr60_18,sr60_19,sr60_20,sr60_21,sr60_22,sr60_23,sr60_24,sr60_25,sr60_26,sr60_27,sr60_28,sr60_29,sr60_30,sr60_31,sr60_32,sr60_33,sr60_34,sr60_35,sr60_36,sr60_37,sr60_38,sr60_39,sr60_40,sr60_41,sr60_42,sr60_43,sr60_44,sr60_45,sr60_46,sr60_47,sr60_48,sr60_49,sr60_50,sr60_51,sr60_52,sr60_53,sr60_54,sr60_55,sr60_56,sr60_57,sr60_58,sr60_59,sr60_60,sr60_61,sr60_62,sr60_63,sr60_64,sr60_65,sr60_66,sr60_67,sr60_68,sr60_69,sr60_70,sr60_71,sr60_72,sr60_73,sr60_74,sr60_75,sr60_76,sr60_77,sr60_78,sr60_79,sr60_80,sr60_81,sr60_82,sr60_83,sr60_84,sr60_85,sr60_86,sr60_87,sr60_88,sr60_89,sr60_90,sr60_91,sr60_92,sr60_93,sr60_94,sr60_95,sr60_96,sr60_97,sr60_98,sr60_99,sr60_100)
sr60_r$objective <- "Random"
sr60_r$species<-"60"


feature_a <- feature_abundances(r60, na.rm = FALSE)

sr60r<-left_join(sr60_r,feature_a)


#65
pu_dat65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r65 <- problem(pu_dat65, spec_dat65, cost_column = "cost", rij = puvsp_dat65) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat65$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr65 <- solve(r65)

sr65<-as.data.frame(sr65) %>% as_tibble()

sr65$objective<-"Random"
sr65$solution<-"65"

# Features Dataset
sr65_1 <- feature_representation(r65, sr65[, "solution_1"])
sr65_1$run <- "solution_1"
sr65_2 <- feature_representation(r65, sr65[, "solution_2"])
sr65_2$run <- "solution_2"
sr65_3 <- feature_representation(r65, sr65[, "solution_3"])
sr65_3$run <- "solution_3"
sr65_4 <- feature_representation(r65, sr65[, "solution_4"])
sr65_4$run <- "solution_4"
sr65_5 <- feature_representation(r65, sr65[, "solution_5"])
sr65_5$run <- "solution_5"
sr65_6 <- feature_representation(r65, sr65[, "solution_6"])
sr65_6$run <- "solution_6"
sr65_7 <- feature_representation(r65, sr65[, "solution_7"])
sr65_7$run <- "solution_7"
sr65_8 <- feature_representation(r65, sr65[, "solution_8"])
sr65_8$run <- "solution_8"
sr65_9 <- feature_representation(r65, sr65[, "solution_9"])
sr65_9$run <- "solution_9"
sr65_10 <- feature_representation(r65, sr65[, "solution_10"])
sr65_10$run <- "solution_10"
sr65_11 <- feature_representation(r65, sr65[, "solution_11"])
sr65_11$run <- "solution_11"
sr65_12 <- feature_representation(r65, sr65[, "solution_12"])
sr65_12$run <- "solution_12"
sr65_13 <- feature_representation(r65, sr65[, "solution_13"])
sr65_13$run <- "solution_13"
sr65_14 <- feature_representation(r65, sr65[, "solution_14"])
sr65_14$run <- "solution_14"
sr65_15 <- feature_representation(r65, sr65[, "solution_15"])
sr65_15$run <- "solution_15"
sr65_16 <- feature_representation(r65, sr65[, "solution_16"])
sr65_16$run <- "solution_16"
sr65_17 <- feature_representation(r65, sr65[, "solution_17"])
sr65_17$run <- "solution_17"
sr65_18 <- feature_representation(r65, sr65[, "solution_18"])
sr65_18$run <- "solution_18"
sr65_19 <- feature_representation(r65, sr65[, "solution_19"])
sr65_19$run <- "solution_19"
sr65_20 <- feature_representation(r65, sr65[, "solution_20"])
sr65_20$run <- "solution_20"
sr65_21 <- feature_representation(r65, sr65[, "solution_21"])
sr65_21$run <- "solution_21"
sr65_22 <- feature_representation(r65, sr65[, "solution_22"])
sr65_22$run <- "solution_22"
sr65_23 <- feature_representation(r65, sr65[, "solution_23"])
sr65_23$run <- "solution_23"
sr65_24 <- feature_representation(r65, sr65[, "solution_24"])
sr65_24$run <- "solution_24"
sr65_25 <- feature_representation(r65, sr65[, "solution_25"])
sr65_25$run <- "solution_25"
sr65_26 <- feature_representation(r65, sr65[, "solution_26"])
sr65_26$run <- "solution_26"
sr65_27 <- feature_representation(r65, sr65[, "solution_27"])
sr65_27$run <- "solution_27"
sr65_28 <- feature_representation(r65, sr65[, "solution_28"])
sr65_28$run <- "solution_28"
sr65_29 <- feature_representation(r65, sr65[, "solution_29"])
sr65_29$run <- "solution_29"
sr65_30 <- feature_representation(r65, sr65[, "solution_30"])
sr65_30$run <- "solution_30"
sr65_31 <- feature_representation(r65, sr65[, "solution_31"])
sr65_31$run <- "solution_31"
sr65_32 <- feature_representation(r65, sr65[, "solution_32"])
sr65_32$run <- "solution_32"
sr65_33 <- feature_representation(r65, sr65[, "solution_33"])
sr65_33$run <- "solution_33"
sr65_34 <- feature_representation(r65, sr65[, "solution_34"])
sr65_34$run <- "solution_34"
sr65_35 <- feature_representation(r65, sr65[, "solution_35"])
sr65_35$run <- "solution_35"
sr65_36 <- feature_representation(r65, sr65[, "solution_36"])
sr65_36$run <- "solution_36"
sr65_37 <- feature_representation(r65, sr65[, "solution_37"])
sr65_37$run <- "solution_37"
sr65_38 <- feature_representation(r65, sr65[, "solution_38"])
sr65_38$run <- "solution_38"
sr65_39 <- feature_representation(r65, sr65[, "solution_39"])
sr65_39$run <- "solution_39"
sr65_40 <- feature_representation(r65, sr65[, "solution_40"])
sr65_40$run <- "solution_40"
sr65_41 <- feature_representation(r65, sr65[, "solution_41"])
sr65_41$run <- "solution_41"
sr65_42 <- feature_representation(r65, sr65[, "solution_42"])
sr65_42$run <- "solution_42"
sr65_43 <- feature_representation(r65, sr65[, "solution_43"])
sr65_43$run <- "solution_43"
sr65_44 <- feature_representation(r65, sr65[, "solution_44"])
sr65_44$run <- "solution_44"
sr65_45 <- feature_representation(r65, sr65[, "solution_45"])
sr65_45$run <- "solution_45"
sr65_46 <- feature_representation(r65, sr65[, "solution_46"])
sr65_46$run <- "solution_46"
sr65_47 <- feature_representation(r65, sr65[, "solution_47"])
sr65_47$run <- "solution_47"
sr65_48 <- feature_representation(r65, sr65[, "solution_48"])
sr65_48$run <- "solution_48"
sr65_49 <- feature_representation(r65, sr65[, "solution_49"])
sr65_49$run <- "solution_49"
sr65_50 <- feature_representation(r65, sr65[, "solution_50"])
sr65_50$run <- "solution_50"
sr65_51 <- feature_representation(r65, sr65[, "solution_51"])
sr65_51$run <- "solution_51"
sr65_52 <- feature_representation(r65, sr65[, "solution_52"])
sr65_52$run <- "solution_52"
sr65_53 <- feature_representation(r65, sr65[, "solution_53"])
sr65_53$run <- "solution_53"
sr65_54 <- feature_representation(r65, sr65[, "solution_54"])
sr65_54$run <- "solution_54"
sr65_55 <- feature_representation(r65, sr65[, "solution_55"])
sr65_55$run <- "solution_55"
sr65_56 <- feature_representation(r65, sr65[, "solution_56"])
sr65_56$run <- "solution_56"
sr65_57 <- feature_representation(r65, sr65[, "solution_57"])
sr65_57$run <- "solution_57"
sr65_58 <- feature_representation(r65, sr65[, "solution_58"])
sr65_58$run <- "solution_58"
sr65_59 <- feature_representation(r65, sr65[, "solution_59"])
sr65_59$run <- "solution_59"
sr65_60 <- feature_representation(r65, sr65[, "solution_60"])
sr65_60$run <- "solution_60"
sr65_61 <- feature_representation(r65, sr65[, "solution_61"])
sr65_61$run <- "solution_61"
sr65_62 <- feature_representation(r65, sr65[, "solution_62"])
sr65_62$run <- "solution_62"
sr65_63 <- feature_representation(r65, sr65[, "solution_63"])
sr65_63$run <- "solution_63"
sr65_64 <- feature_representation(r65, sr65[, "solution_64"])
sr65_64$run <- "solution_64"
sr65_65 <- feature_representation(r65, sr65[, "solution_65"])
sr65_65$run <- "solution_65"
sr65_66 <- feature_representation(r65, sr65[, "solution_66"])
sr65_66$run <- "solution_66"
sr65_67 <- feature_representation(r65, sr65[, "solution_67"])
sr65_67$run <- "solution_67"
sr65_68 <- feature_representation(r65, sr65[, "solution_68"])
sr65_68$run <- "solution_68"
sr65_69 <- feature_representation(r65, sr65[, "solution_69"])
sr65_69$run <- "solution_69"
sr65_70 <- feature_representation(r65, sr65[, "solution_70"])
sr65_70$run <- "solution_70"
sr65_71 <- feature_representation(r65, sr65[, "solution_71"])
sr65_71$run <- "solution_71"
sr65_72 <- feature_representation(r65, sr65[, "solution_72"])
sr65_72$run <- "solution_72"
sr65_73 <- feature_representation(r65, sr65[, "solution_73"])
sr65_73$run <- "solution_73"
sr65_74 <- feature_representation(r65, sr65[, "solution_74"])
sr65_74$run <- "solution_74"
sr65_75 <- feature_representation(r65, sr65[, "solution_75"])
sr65_75$run <- "solution_75"
sr65_76 <- feature_representation(r65, sr65[, "solution_76"])
sr65_76$run <- "solution_76"
sr65_77 <- feature_representation(r65, sr65[, "solution_77"])
sr65_77$run <- "solution_77"
sr65_78 <- feature_representation(r65, sr65[, "solution_78"])
sr65_78$run <- "solution_78"
sr65_79 <- feature_representation(r65, sr65[, "solution_79"])
sr65_79$run <- "solution_79"
sr65_80 <- feature_representation(r65, sr65[, "solution_80"])
sr65_80$run <- "solution_80"
sr65_81 <- feature_representation(r65, sr65[, "solution_81"])
sr65_81$run <- "solution_81"
sr65_82 <- feature_representation(r65, sr65[, "solution_82"])
sr65_82$run <- "solution_82"
sr65_83 <- feature_representation(r65, sr65[, "solution_83"])
sr65_83$run <- "solution_83"
sr65_84 <- feature_representation(r65, sr65[, "solution_84"])
sr65_84$run <- "solution_84"
sr65_85 <- feature_representation(r65, sr65[, "solution_85"])
sr65_85$run <- "solution_85"
sr65_86 <- feature_representation(r65, sr65[, "solution_86"])
sr65_86$run <- "solution_86"
sr65_87 <- feature_representation(r65, sr65[, "solution_87"])
sr65_87$run <- "solution_87"
sr65_88 <- feature_representation(r65, sr65[, "solution_88"])
sr65_88$run <- "solution_88"
sr65_89 <- feature_representation(r65, sr65[, "solution_89"])
sr65_89$run <- "solution_89"
sr65_90 <- feature_representation(r65, sr65[, "solution_90"])
sr65_90$run <- "solution_90"
sr65_91 <- feature_representation(r65, sr65[, "solution_91"])
sr65_91$run <- "solution_91"
sr65_92 <- feature_representation(r65, sr65[, "solution_92"])
sr65_92$run <- "solution_92"
sr65_93 <- feature_representation(r65, sr65[, "solution_93"])
sr65_93$run <- "solution_93"
sr65_94 <- feature_representation(r65, sr65[, "solution_94"])
sr65_94$run <- "solution_94"
sr65_95 <- feature_representation(r65, sr65[, "solution_95"])
sr65_95$run <- "solution_95"
sr65_96 <- feature_representation(r65, sr65[, "solution_96"])
sr65_96$run <- "solution_96"
sr65_97 <- feature_representation(r65, sr65[, "solution_97"])
sr65_97$run <- "solution_97"
sr65_98 <- feature_representation(r65, sr65[, "solution_98"])
sr65_98$run <- "solution_98"
sr65_99 <- feature_representation(r65, sr65[, "solution_99"])
sr65_99$run <- "solution_99"
sr65_100 <- feature_representation(r65, sr65[, "solution_100"])
sr65_100$run <- "solution_100"
sr65_r<-bind_rows(sr65_1,sr65_2,sr65_3,sr65_4,sr65_5,sr65_6,sr65_7,sr65_8,sr65_9,sr65_10,sr65_11,sr65_12,sr65_13,sr65_14,sr65_15,sr65_16,sr65_17,sr65_18,sr65_19,sr65_20,sr65_21,sr65_22,sr65_23,sr65_24,sr65_25,sr65_26,sr65_27,sr65_28,sr65_29,sr65_30,sr65_31,sr65_32,sr65_33,sr65_34,sr65_35,sr65_36,sr65_37,sr65_38,sr65_39,sr65_40,sr65_41,sr65_42,sr65_43,sr65_44,sr65_45,sr65_46,sr65_47,sr65_48,sr65_49,sr65_50,sr65_51,sr65_52,sr65_53,sr65_54,sr65_55,sr65_56,sr65_57,sr65_58,sr65_59,sr65_60,sr65_61,sr65_62,sr65_63,sr65_64,sr65_65,sr65_66,sr65_67,sr65_68,sr65_69,sr65_70,sr65_71,sr65_72,sr65_73,sr65_74,sr65_75,sr65_76,sr65_77,sr65_78,sr65_79,sr65_80,sr65_81,sr65_82,sr65_83,sr65_84,sr65_85,sr65_86,sr65_87,sr65_88,sr65_89,sr65_90,sr65_91,sr65_92,sr65_93,sr65_94,sr65_95,sr65_96,sr65_97,sr65_98,sr65_99,sr65_100)
sr65_r$objective <- "Random"
sr65_r$species<-"65"


feature_a <- feature_abundances(r65, na.rm = FALSE)

sr65r<-left_join(sr65_r,feature_a)


#70
pu_dat70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r70 <- problem(pu_dat70, spec_dat70, cost_column = "cost", rij = puvsp_dat70) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat70$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr70 <- solve(r70)

sr70<-as.data.frame(sr70) %>% as_tibble()

sr70$objective<-"Random"
sr70$solution<-"70"

# Features Dataset
sr70_1 <- feature_representation(r70, sr70[, "solution_1"])
sr70_1$run <- "solution_1"
sr70_2 <- feature_representation(r70, sr70[, "solution_2"])
sr70_2$run <- "solution_2"
sr70_3 <- feature_representation(r70, sr70[, "solution_3"])
sr70_3$run <- "solution_3"
sr70_4 <- feature_representation(r70, sr70[, "solution_4"])
sr70_4$run <- "solution_4"
sr70_5 <- feature_representation(r70, sr70[, "solution_5"])
sr70_5$run <- "solution_5"
sr70_6 <- feature_representation(r70, sr70[, "solution_6"])
sr70_6$run <- "solution_6"
sr70_7 <- feature_representation(r70, sr70[, "solution_7"])
sr70_7$run <- "solution_7"
sr70_8 <- feature_representation(r70, sr70[, "solution_8"])
sr70_8$run <- "solution_8"
sr70_9 <- feature_representation(r70, sr70[, "solution_9"])
sr70_9$run <- "solution_9"
sr70_10 <- feature_representation(r70, sr70[, "solution_10"])
sr70_10$run <- "solution_10"
sr70_11 <- feature_representation(r70, sr70[, "solution_11"])
sr70_11$run <- "solution_11"
sr70_12 <- feature_representation(r70, sr70[, "solution_12"])
sr70_12$run <- "solution_12"
sr70_13 <- feature_representation(r70, sr70[, "solution_13"])
sr70_13$run <- "solution_13"
sr70_14 <- feature_representation(r70, sr70[, "solution_14"])
sr70_14$run <- "solution_14"
sr70_15 <- feature_representation(r70, sr70[, "solution_15"])
sr70_15$run <- "solution_15"
sr70_16 <- feature_representation(r70, sr70[, "solution_16"])
sr70_16$run <- "solution_16"
sr70_17 <- feature_representation(r70, sr70[, "solution_17"])
sr70_17$run <- "solution_17"
sr70_18 <- feature_representation(r70, sr70[, "solution_18"])
sr70_18$run <- "solution_18"
sr70_19 <- feature_representation(r70, sr70[, "solution_19"])
sr70_19$run <- "solution_19"
sr70_20 <- feature_representation(r70, sr70[, "solution_20"])
sr70_20$run <- "solution_20"
sr70_21 <- feature_representation(r70, sr70[, "solution_21"])
sr70_21$run <- "solution_21"
sr70_22 <- feature_representation(r70, sr70[, "solution_22"])
sr70_22$run <- "solution_22"
sr70_23 <- feature_representation(r70, sr70[, "solution_23"])
sr70_23$run <- "solution_23"
sr70_24 <- feature_representation(r70, sr70[, "solution_24"])
sr70_24$run <- "solution_24"
sr70_25 <- feature_representation(r70, sr70[, "solution_25"])
sr70_25$run <- "solution_25"
sr70_26 <- feature_representation(r70, sr70[, "solution_26"])
sr70_26$run <- "solution_26"
sr70_27 <- feature_representation(r70, sr70[, "solution_27"])
sr70_27$run <- "solution_27"
sr70_28 <- feature_representation(r70, sr70[, "solution_28"])
sr70_28$run <- "solution_28"
sr70_29 <- feature_representation(r70, sr70[, "solution_29"])
sr70_29$run <- "solution_29"
sr70_30 <- feature_representation(r70, sr70[, "solution_30"])
sr70_30$run <- "solution_30"
sr70_31 <- feature_representation(r70, sr70[, "solution_31"])
sr70_31$run <- "solution_31"
sr70_32 <- feature_representation(r70, sr70[, "solution_32"])
sr70_32$run <- "solution_32"
sr70_33 <- feature_representation(r70, sr70[, "solution_33"])
sr70_33$run <- "solution_33"
sr70_34 <- feature_representation(r70, sr70[, "solution_34"])
sr70_34$run <- "solution_34"
sr70_35 <- feature_representation(r70, sr70[, "solution_35"])
sr70_35$run <- "solution_35"
sr70_36 <- feature_representation(r70, sr70[, "solution_36"])
sr70_36$run <- "solution_36"
sr70_37 <- feature_representation(r70, sr70[, "solution_37"])
sr70_37$run <- "solution_37"
sr70_38 <- feature_representation(r70, sr70[, "solution_38"])
sr70_38$run <- "solution_38"
sr70_39 <- feature_representation(r70, sr70[, "solution_39"])
sr70_39$run <- "solution_39"
sr70_40 <- feature_representation(r70, sr70[, "solution_40"])
sr70_40$run <- "solution_40"
sr70_41 <- feature_representation(r70, sr70[, "solution_41"])
sr70_41$run <- "solution_41"
sr70_42 <- feature_representation(r70, sr70[, "solution_42"])
sr70_42$run <- "solution_42"
sr70_43 <- feature_representation(r70, sr70[, "solution_43"])
sr70_43$run <- "solution_43"
sr70_44 <- feature_representation(r70, sr70[, "solution_44"])
sr70_44$run <- "solution_44"
sr70_45 <- feature_representation(r70, sr70[, "solution_45"])
sr70_45$run <- "solution_45"
sr70_46 <- feature_representation(r70, sr70[, "solution_46"])
sr70_46$run <- "solution_46"
sr70_47 <- feature_representation(r70, sr70[, "solution_47"])
sr70_47$run <- "solution_47"
sr70_48 <- feature_representation(r70, sr70[, "solution_48"])
sr70_48$run <- "solution_48"
sr70_49 <- feature_representation(r70, sr70[, "solution_49"])
sr70_49$run <- "solution_49"
sr70_50 <- feature_representation(r70, sr70[, "solution_50"])
sr70_50$run <- "solution_50"
sr70_51 <- feature_representation(r70, sr70[, "solution_51"])
sr70_51$run <- "solution_51"
sr70_52 <- feature_representation(r70, sr70[, "solution_52"])
sr70_52$run <- "solution_52"
sr70_53 <- feature_representation(r70, sr70[, "solution_53"])
sr70_53$run <- "solution_53"
sr70_54 <- feature_representation(r70, sr70[, "solution_54"])
sr70_54$run <- "solution_54"
sr70_55 <- feature_representation(r70, sr70[, "solution_55"])
sr70_55$run <- "solution_55"
sr70_56 <- feature_representation(r70, sr70[, "solution_56"])
sr70_56$run <- "solution_56"
sr70_57 <- feature_representation(r70, sr70[, "solution_57"])
sr70_57$run <- "solution_57"
sr70_58 <- feature_representation(r70, sr70[, "solution_58"])
sr70_58$run <- "solution_58"
sr70_59 <- feature_representation(r70, sr70[, "solution_59"])
sr70_59$run <- "solution_59"
sr70_60 <- feature_representation(r70, sr70[, "solution_60"])
sr70_60$run <- "solution_60"
sr70_61 <- feature_representation(r70, sr70[, "solution_61"])
sr70_61$run <- "solution_61"
sr70_62 <- feature_representation(r70, sr70[, "solution_62"])
sr70_62$run <- "solution_62"
sr70_63 <- feature_representation(r70, sr70[, "solution_63"])
sr70_63$run <- "solution_63"
sr70_64 <- feature_representation(r70, sr70[, "solution_64"])
sr70_64$run <- "solution_64"
sr70_65 <- feature_representation(r70, sr70[, "solution_65"])
sr70_65$run <- "solution_65"
sr70_66 <- feature_representation(r70, sr70[, "solution_66"])
sr70_66$run <- "solution_66"
sr70_67 <- feature_representation(r70, sr70[, "solution_67"])
sr70_67$run <- "solution_67"
sr70_68 <- feature_representation(r70, sr70[, "solution_68"])
sr70_68$run <- "solution_68"
sr70_69 <- feature_representation(r70, sr70[, "solution_69"])
sr70_69$run <- "solution_69"
sr70_70 <- feature_representation(r70, sr70[, "solution_70"])
sr70_70$run <- "solution_70"
sr70_71 <- feature_representation(r70, sr70[, "solution_71"])
sr70_71$run <- "solution_71"
sr70_72 <- feature_representation(r70, sr70[, "solution_72"])
sr70_72$run <- "solution_72"
sr70_73 <- feature_representation(r70, sr70[, "solution_73"])
sr70_73$run <- "solution_73"
sr70_74 <- feature_representation(r70, sr70[, "solution_74"])
sr70_74$run <- "solution_74"
sr70_75 <- feature_representation(r70, sr70[, "solution_75"])
sr70_75$run <- "solution_75"
sr70_76 <- feature_representation(r70, sr70[, "solution_76"])
sr70_76$run <- "solution_76"
sr70_77 <- feature_representation(r70, sr70[, "solution_77"])
sr70_77$run <- "solution_77"
sr70_78 <- feature_representation(r70, sr70[, "solution_78"])
sr70_78$run <- "solution_78"
sr70_79 <- feature_representation(r70, sr70[, "solution_79"])
sr70_79$run <- "solution_79"
sr70_80 <- feature_representation(r70, sr70[, "solution_80"])
sr70_80$run <- "solution_80"
sr70_81 <- feature_representation(r70, sr70[, "solution_81"])
sr70_81$run <- "solution_81"
sr70_82 <- feature_representation(r70, sr70[, "solution_82"])
sr70_82$run <- "solution_82"
sr70_83 <- feature_representation(r70, sr70[, "solution_83"])
sr70_83$run <- "solution_83"
sr70_84 <- feature_representation(r70, sr70[, "solution_84"])
sr70_84$run <- "solution_84"
sr70_85 <- feature_representation(r70, sr70[, "solution_85"])
sr70_85$run <- "solution_85"
sr70_86 <- feature_representation(r70, sr70[, "solution_86"])
sr70_86$run <- "solution_86"
sr70_87 <- feature_representation(r70, sr70[, "solution_87"])
sr70_87$run <- "solution_87"
sr70_88 <- feature_representation(r70, sr70[, "solution_88"])
sr70_88$run <- "solution_88"
sr70_89 <- feature_representation(r70, sr70[, "solution_89"])
sr70_89$run <- "solution_89"
sr70_90 <- feature_representation(r70, sr70[, "solution_90"])
sr70_90$run <- "solution_90"
sr70_91 <- feature_representation(r70, sr70[, "solution_91"])
sr70_91$run <- "solution_91"
sr70_92 <- feature_representation(r70, sr70[, "solution_92"])
sr70_92$run <- "solution_92"
sr70_93 <- feature_representation(r70, sr70[, "solution_93"])
sr70_93$run <- "solution_93"
sr70_94 <- feature_representation(r70, sr70[, "solution_94"])
sr70_94$run <- "solution_94"
sr70_95 <- feature_representation(r70, sr70[, "solution_95"])
sr70_95$run <- "solution_95"
sr70_96 <- feature_representation(r70, sr70[, "solution_96"])
sr70_96$run <- "solution_96"
sr70_97 <- feature_representation(r70, sr70[, "solution_97"])
sr70_97$run <- "solution_97"
sr70_98 <- feature_representation(r70, sr70[, "solution_98"])
sr70_98$run <- "solution_98"
sr70_99 <- feature_representation(r70, sr70[, "solution_99"])
sr70_99$run <- "solution_99"
sr70_100 <- feature_representation(r70, sr70[, "solution_100"])
sr70_100$run <- "solution_100"
sr70_r<-bind_rows(sr70_1,sr70_2,sr70_3,sr70_4,sr70_5,sr70_6,sr70_7,sr70_8,sr70_9,sr70_10,sr70_11,sr70_12,sr70_13,sr70_14,sr70_15,sr70_16,sr70_17,sr70_18,sr70_19,sr70_20,sr70_21,sr70_22,sr70_23,sr70_24,sr70_25,sr70_26,sr70_27,sr70_28,sr70_29,sr70_30,sr70_31,sr70_32,sr70_33,sr70_34,sr70_35,sr70_36,sr70_37,sr70_38,sr70_39,sr70_40,sr70_41,sr70_42,sr70_43,sr70_44,sr70_45,sr70_46,sr70_47,sr70_48,sr70_49,sr70_50,sr70_51,sr70_52,sr70_53,sr70_54,sr70_55,sr70_56,sr70_57,sr70_58,sr70_59,sr70_60,sr70_61,sr70_62,sr70_63,sr70_64,sr70_65,sr70_66,sr70_67,sr70_68,sr70_69,sr70_70,sr70_71,sr70_72,sr70_73,sr70_74,sr70_75,sr70_76,sr70_77,sr70_78,sr70_79,sr70_80,sr70_81,sr70_82,sr70_83,sr70_84,sr70_85,sr70_86,sr70_87,sr70_88,sr70_89,sr70_90,sr70_91,sr70_92,sr70_93,sr70_94,sr70_95,sr70_96,sr70_97,sr70_98,sr70_99,sr70_100)
sr70_r$objective <- "Random"
sr70_r$species<-"70"


feature_a <- feature_abundances(r70, na.rm = FALSE)

sr70r<-left_join(sr70_r,feature_a)


#75
pu_dat75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r75 <- problem(pu_dat75, spec_dat75, cost_column = "cost", rij = puvsp_dat75) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat75$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr75 <- solve(r75)

sr75<-as.data.frame(sr75) %>% as_tibble()

sr75$objective<-"Random"
sr75$solution<-"75"


# Features Dataset
sr75_1 <- feature_representation(r75, sr75[, "solution_1"])
sr75_1$run <- "solution_1"
sr75_2 <- feature_representation(r75, sr75[, "solution_2"])
sr75_2$run <- "solution_2"
sr75_3 <- feature_representation(r75, sr75[, "solution_3"])
sr75_3$run <- "solution_3"
sr75_4 <- feature_representation(r75, sr75[, "solution_4"])
sr75_4$run <- "solution_4"
sr75_5 <- feature_representation(r75, sr75[, "solution_5"])
sr75_5$run <- "solution_5"
sr75_6 <- feature_representation(r75, sr75[, "solution_6"])
sr75_6$run <- "solution_6"
sr75_7 <- feature_representation(r75, sr75[, "solution_7"])
sr75_7$run <- "solution_7"
sr75_8 <- feature_representation(r75, sr75[, "solution_8"])
sr75_8$run <- "solution_8"
sr75_9 <- feature_representation(r75, sr75[, "solution_9"])
sr75_9$run <- "solution_9"
sr75_10 <- feature_representation(r75, sr75[, "solution_10"])
sr75_10$run <- "solution_10"
sr75_11 <- feature_representation(r75, sr75[, "solution_11"])
sr75_11$run <- "solution_11"
sr75_12 <- feature_representation(r75, sr75[, "solution_12"])
sr75_12$run <- "solution_12"
sr75_13 <- feature_representation(r75, sr75[, "solution_13"])
sr75_13$run <- "solution_13"
sr75_14 <- feature_representation(r75, sr75[, "solution_14"])
sr75_14$run <- "solution_14"
sr75_15 <- feature_representation(r75, sr75[, "solution_15"])
sr75_15$run <- "solution_15"
sr75_16 <- feature_representation(r75, sr75[, "solution_16"])
sr75_16$run <- "solution_16"
sr75_17 <- feature_representation(r75, sr75[, "solution_17"])
sr75_17$run <- "solution_17"
sr75_18 <- feature_representation(r75, sr75[, "solution_18"])
sr75_18$run <- "solution_18"
sr75_19 <- feature_representation(r75, sr75[, "solution_19"])
sr75_19$run <- "solution_19"
sr75_20 <- feature_representation(r75, sr75[, "solution_20"])
sr75_20$run <- "solution_20"
sr75_21 <- feature_representation(r75, sr75[, "solution_21"])
sr75_21$run <- "solution_21"
sr75_22 <- feature_representation(r75, sr75[, "solution_22"])
sr75_22$run <- "solution_22"
sr75_23 <- feature_representation(r75, sr75[, "solution_23"])
sr75_23$run <- "solution_23"
sr75_24 <- feature_representation(r75, sr75[, "solution_24"])
sr75_24$run <- "solution_24"
sr75_25 <- feature_representation(r75, sr75[, "solution_25"])
sr75_25$run <- "solution_25"
sr75_26 <- feature_representation(r75, sr75[, "solution_26"])
sr75_26$run <- "solution_26"
sr75_27 <- feature_representation(r75, sr75[, "solution_27"])
sr75_27$run <- "solution_27"
sr75_28 <- feature_representation(r75, sr75[, "solution_28"])
sr75_28$run <- "solution_28"
sr75_29 <- feature_representation(r75, sr75[, "solution_29"])
sr75_29$run <- "solution_29"
sr75_30 <- feature_representation(r75, sr75[, "solution_30"])
sr75_30$run <- "solution_30"
sr75_31 <- feature_representation(r75, sr75[, "solution_31"])
sr75_31$run <- "solution_31"
sr75_32 <- feature_representation(r75, sr75[, "solution_32"])
sr75_32$run <- "solution_32"
sr75_33 <- feature_representation(r75, sr75[, "solution_33"])
sr75_33$run <- "solution_33"
sr75_34 <- feature_representation(r75, sr75[, "solution_34"])
sr75_34$run <- "solution_34"
sr75_35 <- feature_representation(r75, sr75[, "solution_35"])
sr75_35$run <- "solution_35"
sr75_36 <- feature_representation(r75, sr75[, "solution_36"])
sr75_36$run <- "solution_36"
sr75_37 <- feature_representation(r75, sr75[, "solution_37"])
sr75_37$run <- "solution_37"
sr75_38 <- feature_representation(r75, sr75[, "solution_38"])
sr75_38$run <- "solution_38"
sr75_39 <- feature_representation(r75, sr75[, "solution_39"])
sr75_39$run <- "solution_39"
sr75_40 <- feature_representation(r75, sr75[, "solution_40"])
sr75_40$run <- "solution_40"
sr75_41 <- feature_representation(r75, sr75[, "solution_41"])
sr75_41$run <- "solution_41"
sr75_42 <- feature_representation(r75, sr75[, "solution_42"])
sr75_42$run <- "solution_42"
sr75_43 <- feature_representation(r75, sr75[, "solution_43"])
sr75_43$run <- "solution_43"
sr75_44 <- feature_representation(r75, sr75[, "solution_44"])
sr75_44$run <- "solution_44"
sr75_45 <- feature_representation(r75, sr75[, "solution_45"])
sr75_45$run <- "solution_45"
sr75_46 <- feature_representation(r75, sr75[, "solution_46"])
sr75_46$run <- "solution_46"
sr75_47 <- feature_representation(r75, sr75[, "solution_47"])
sr75_47$run <- "solution_47"
sr75_48 <- feature_representation(r75, sr75[, "solution_48"])
sr75_48$run <- "solution_48"
sr75_49 <- feature_representation(r75, sr75[, "solution_49"])
sr75_49$run <- "solution_49"
sr75_50 <- feature_representation(r75, sr75[, "solution_50"])
sr75_50$run <- "solution_50"
sr75_51 <- feature_representation(r75, sr75[, "solution_51"])
sr75_51$run <- "solution_51"
sr75_52 <- feature_representation(r75, sr75[, "solution_52"])
sr75_52$run <- "solution_52"
sr75_53 <- feature_representation(r75, sr75[, "solution_53"])
sr75_53$run <- "solution_53"
sr75_54 <- feature_representation(r75, sr75[, "solution_54"])
sr75_54$run <- "solution_54"
sr75_55 <- feature_representation(r75, sr75[, "solution_55"])
sr75_55$run <- "solution_55"
sr75_56 <- feature_representation(r75, sr75[, "solution_56"])
sr75_56$run <- "solution_56"
sr75_57 <- feature_representation(r75, sr75[, "solution_57"])
sr75_57$run <- "solution_57"
sr75_58 <- feature_representation(r75, sr75[, "solution_58"])
sr75_58$run <- "solution_58"
sr75_59 <- feature_representation(r75, sr75[, "solution_59"])
sr75_59$run <- "solution_59"
sr75_60 <- feature_representation(r75, sr75[, "solution_60"])
sr75_60$run <- "solution_60"
sr75_61 <- feature_representation(r75, sr75[, "solution_61"])
sr75_61$run <- "solution_61"
sr75_62 <- feature_representation(r75, sr75[, "solution_62"])
sr75_62$run <- "solution_62"
sr75_63 <- feature_representation(r75, sr75[, "solution_63"])
sr75_63$run <- "solution_63"
sr75_64 <- feature_representation(r75, sr75[, "solution_64"])
sr75_64$run <- "solution_64"
sr75_65 <- feature_representation(r75, sr75[, "solution_65"])
sr75_65$run <- "solution_65"
sr75_66 <- feature_representation(r75, sr75[, "solution_66"])
sr75_66$run <- "solution_66"
sr75_67 <- feature_representation(r75, sr75[, "solution_67"])
sr75_67$run <- "solution_67"
sr75_68 <- feature_representation(r75, sr75[, "solution_68"])
sr75_68$run <- "solution_68"
sr75_69 <- feature_representation(r75, sr75[, "solution_69"])
sr75_69$run <- "solution_69"
sr75_70 <- feature_representation(r75, sr75[, "solution_70"])
sr75_70$run <- "solution_70"
sr75_71 <- feature_representation(r75, sr75[, "solution_71"])
sr75_71$run <- "solution_71"
sr75_72 <- feature_representation(r75, sr75[, "solution_72"])
sr75_72$run <- "solution_72"
sr75_73 <- feature_representation(r75, sr75[, "solution_73"])
sr75_73$run <- "solution_73"
sr75_74 <- feature_representation(r75, sr75[, "solution_74"])
sr75_74$run <- "solution_74"
sr75_75 <- feature_representation(r75, sr75[, "solution_75"])
sr75_75$run <- "solution_75"
sr75_76 <- feature_representation(r75, sr75[, "solution_76"])
sr75_76$run <- "solution_76"
sr75_77 <- feature_representation(r75, sr75[, "solution_77"])
sr75_77$run <- "solution_77"
sr75_78 <- feature_representation(r75, sr75[, "solution_78"])
sr75_78$run <- "solution_78"
sr75_79 <- feature_representation(r75, sr75[, "solution_79"])
sr75_79$run <- "solution_79"
sr75_80 <- feature_representation(r75, sr75[, "solution_80"])
sr75_80$run <- "solution_80"
sr75_81 <- feature_representation(r75, sr75[, "solution_81"])
sr75_81$run <- "solution_81"
sr75_82 <- feature_representation(r75, sr75[, "solution_82"])
sr75_82$run <- "solution_82"
sr75_83 <- feature_representation(r75, sr75[, "solution_83"])
sr75_83$run <- "solution_83"
sr75_84 <- feature_representation(r75, sr75[, "solution_84"])
sr75_84$run <- "solution_84"
sr75_85 <- feature_representation(r75, sr75[, "solution_85"])
sr75_85$run <- "solution_85"
sr75_86 <- feature_representation(r75, sr75[, "solution_86"])
sr75_86$run <- "solution_86"
sr75_87 <- feature_representation(r75, sr75[, "solution_87"])
sr75_87$run <- "solution_87"
sr75_88 <- feature_representation(r75, sr75[, "solution_88"])
sr75_88$run <- "solution_88"
sr75_89 <- feature_representation(r75, sr75[, "solution_89"])
sr75_89$run <- "solution_89"
sr75_90 <- feature_representation(r75, sr75[, "solution_90"])
sr75_90$run <- "solution_90"
sr75_91 <- feature_representation(r75, sr75[, "solution_91"])
sr75_91$run <- "solution_91"
sr75_92 <- feature_representation(r75, sr75[, "solution_92"])
sr75_92$run <- "solution_92"
sr75_93 <- feature_representation(r75, sr75[, "solution_93"])
sr75_93$run <- "solution_93"
sr75_94 <- feature_representation(r75, sr75[, "solution_94"])
sr75_94$run <- "solution_94"
sr75_95 <- feature_representation(r75, sr75[, "solution_95"])
sr75_95$run <- "solution_95"
sr75_96 <- feature_representation(r75, sr75[, "solution_96"])
sr75_96$run <- "solution_96"
sr75_97 <- feature_representation(r75, sr75[, "solution_97"])
sr75_97$run <- "solution_97"
sr75_98 <- feature_representation(r75, sr75[, "solution_98"])
sr75_98$run <- "solution_98"
sr75_99 <- feature_representation(r75, sr75[, "solution_99"])
sr75_99$run <- "solution_99"
sr75_100 <- feature_representation(r75, sr75[, "solution_100"])
sr75_100$run <- "solution_100"
sr75_r<-bind_rows(sr75_1,sr75_2,sr75_3,sr75_4,sr75_5,sr75_6,sr75_7,sr75_8,sr75_9,sr75_10,sr75_11,sr75_12,sr75_13,sr75_14,sr75_15,sr75_16,sr75_17,sr75_18,sr75_19,sr75_20,sr75_21,sr75_22,sr75_23,sr75_24,sr75_25,sr75_26,sr75_27,sr75_28,sr75_29,sr75_30,sr75_31,sr75_32,sr75_33,sr75_34,sr75_35,sr75_36,sr75_37,sr75_38,sr75_39,sr75_40,sr75_41,sr75_42,sr75_43,sr75_44,sr75_45,sr75_46,sr75_47,sr75_48,sr75_49,sr75_50,sr75_51,sr75_52,sr75_53,sr75_54,sr75_55,sr75_56,sr75_57,sr75_58,sr75_59,sr75_60,sr75_61,sr75_62,sr75_63,sr75_64,sr75_65,sr75_66,sr75_67,sr75_68,sr75_69,sr75_70,sr75_71,sr75_72,sr75_73,sr75_74,sr75_75,sr75_76,sr75_77,sr75_78,sr75_79,sr75_80,sr75_81,sr75_82,sr75_83,sr75_84,sr75_85,sr75_86,sr75_87,sr75_88,sr75_89,sr75_90,sr75_91,sr75_92,sr75_93,sr75_94,sr75_95,sr75_96,sr75_97,sr75_98,sr75_99,sr75_100)
sr75_r$objective <- "Random"
sr75_r$species<-"75"


feature_a <- feature_abundances(r75, na.rm = FALSE)

sr75r<-left_join(sr75_r,feature_a)

#80
pu_dat80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r80 <- problem(pu_dat80, spec_dat80, cost_column = "cost", rij = puvsp_dat80) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat80$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr80 <- solve(r80)

sr80<-as.data.frame(sr80) %>% as_tibble()

sr80$objective<-"Random"
sr80$solution<-"80"

# Features Dataset
sr80_1 <- feature_representation(r80, sr80[, "solution_1"])
sr80_1$run <- "solution_1"
sr80_2 <- feature_representation(r80, sr80[, "solution_2"])
sr80_2$run <- "solution_2"
sr80_3 <- feature_representation(r80, sr80[, "solution_3"])
sr80_3$run <- "solution_3"
sr80_4 <- feature_representation(r80, sr80[, "solution_4"])
sr80_4$run <- "solution_4"
sr80_5 <- feature_representation(r80, sr80[, "solution_5"])
sr80_5$run <- "solution_5"
sr80_6 <- feature_representation(r80, sr80[, "solution_6"])
sr80_6$run <- "solution_6"
sr80_7 <- feature_representation(r80, sr80[, "solution_7"])
sr80_7$run <- "solution_7"
sr80_8 <- feature_representation(r80, sr80[, "solution_8"])
sr80_8$run <- "solution_8"
sr80_9 <- feature_representation(r80, sr80[, "solution_9"])
sr80_9$run <- "solution_9"
sr80_10 <- feature_representation(r80, sr80[, "solution_10"])
sr80_10$run <- "solution_10"
sr80_11 <- feature_representation(r80, sr80[, "solution_11"])
sr80_11$run <- "solution_11"
sr80_12 <- feature_representation(r80, sr80[, "solution_12"])
sr80_12$run <- "solution_12"
sr80_13 <- feature_representation(r80, sr80[, "solution_13"])
sr80_13$run <- "solution_13"
sr80_14 <- feature_representation(r80, sr80[, "solution_14"])
sr80_14$run <- "solution_14"
sr80_15 <- feature_representation(r80, sr80[, "solution_15"])
sr80_15$run <- "solution_15"
sr80_16 <- feature_representation(r80, sr80[, "solution_16"])
sr80_16$run <- "solution_16"
sr80_17 <- feature_representation(r80, sr80[, "solution_17"])
sr80_17$run <- "solution_17"
sr80_18 <- feature_representation(r80, sr80[, "solution_18"])
sr80_18$run <- "solution_18"
sr80_19 <- feature_representation(r80, sr80[, "solution_19"])
sr80_19$run <- "solution_19"
sr80_20 <- feature_representation(r80, sr80[, "solution_20"])
sr80_20$run <- "solution_20"
sr80_21 <- feature_representation(r80, sr80[, "solution_21"])
sr80_21$run <- "solution_21"
sr80_22 <- feature_representation(r80, sr80[, "solution_22"])
sr80_22$run <- "solution_22"
sr80_23 <- feature_representation(r80, sr80[, "solution_23"])
sr80_23$run <- "solution_23"
sr80_24 <- feature_representation(r80, sr80[, "solution_24"])
sr80_24$run <- "solution_24"
sr80_25 <- feature_representation(r80, sr80[, "solution_25"])
sr80_25$run <- "solution_25"
sr80_26 <- feature_representation(r80, sr80[, "solution_26"])
sr80_26$run <- "solution_26"
sr80_27 <- feature_representation(r80, sr80[, "solution_27"])
sr80_27$run <- "solution_27"
sr80_28 <- feature_representation(r80, sr80[, "solution_28"])
sr80_28$run <- "solution_28"
sr80_29 <- feature_representation(r80, sr80[, "solution_29"])
sr80_29$run <- "solution_29"
sr80_30 <- feature_representation(r80, sr80[, "solution_30"])
sr80_30$run <- "solution_30"
sr80_31 <- feature_representation(r80, sr80[, "solution_31"])
sr80_31$run <- "solution_31"
sr80_32 <- feature_representation(r80, sr80[, "solution_32"])
sr80_32$run <- "solution_32"
sr80_33 <- feature_representation(r80, sr80[, "solution_33"])
sr80_33$run <- "solution_33"
sr80_34 <- feature_representation(r80, sr80[, "solution_34"])
sr80_34$run <- "solution_34"
sr80_35 <- feature_representation(r80, sr80[, "solution_35"])
sr80_35$run <- "solution_35"
sr80_36 <- feature_representation(r80, sr80[, "solution_36"])
sr80_36$run <- "solution_36"
sr80_37 <- feature_representation(r80, sr80[, "solution_37"])
sr80_37$run <- "solution_37"
sr80_38 <- feature_representation(r80, sr80[, "solution_38"])
sr80_38$run <- "solution_38"
sr80_39 <- feature_representation(r80, sr80[, "solution_39"])
sr80_39$run <- "solution_39"
sr80_40 <- feature_representation(r80, sr80[, "solution_40"])
sr80_40$run <- "solution_40"
sr80_41 <- feature_representation(r80, sr80[, "solution_41"])
sr80_41$run <- "solution_41"
sr80_42 <- feature_representation(r80, sr80[, "solution_42"])
sr80_42$run <- "solution_42"
sr80_43 <- feature_representation(r80, sr80[, "solution_43"])
sr80_43$run <- "solution_43"
sr80_44 <- feature_representation(r80, sr80[, "solution_44"])
sr80_44$run <- "solution_44"
sr80_45 <- feature_representation(r80, sr80[, "solution_45"])
sr80_45$run <- "solution_45"
sr80_46 <- feature_representation(r80, sr80[, "solution_46"])
sr80_46$run <- "solution_46"
sr80_47 <- feature_representation(r80, sr80[, "solution_47"])
sr80_47$run <- "solution_47"
sr80_48 <- feature_representation(r80, sr80[, "solution_48"])
sr80_48$run <- "solution_48"
sr80_49 <- feature_representation(r80, sr80[, "solution_49"])
sr80_49$run <- "solution_49"
sr80_50 <- feature_representation(r80, sr80[, "solution_50"])
sr80_50$run <- "solution_50"
sr80_51 <- feature_representation(r80, sr80[, "solution_51"])
sr80_51$run <- "solution_51"
sr80_52 <- feature_representation(r80, sr80[, "solution_52"])
sr80_52$run <- "solution_52"
sr80_53 <- feature_representation(r80, sr80[, "solution_53"])
sr80_53$run <- "solution_53"
sr80_54 <- feature_representation(r80, sr80[, "solution_54"])
sr80_54$run <- "solution_54"
sr80_55 <- feature_representation(r80, sr80[, "solution_55"])
sr80_55$run <- "solution_55"
sr80_56 <- feature_representation(r80, sr80[, "solution_56"])
sr80_56$run <- "solution_56"
sr80_57 <- feature_representation(r80, sr80[, "solution_57"])
sr80_57$run <- "solution_57"
sr80_58 <- feature_representation(r80, sr80[, "solution_58"])
sr80_58$run <- "solution_58"
sr80_59 <- feature_representation(r80, sr80[, "solution_59"])
sr80_59$run <- "solution_59"
sr80_60 <- feature_representation(r80, sr80[, "solution_60"])
sr80_60$run <- "solution_60"
sr80_61 <- feature_representation(r80, sr80[, "solution_61"])
sr80_61$run <- "solution_61"
sr80_62 <- feature_representation(r80, sr80[, "solution_62"])
sr80_62$run <- "solution_62"
sr80_63 <- feature_representation(r80, sr80[, "solution_63"])
sr80_63$run <- "solution_63"
sr80_64 <- feature_representation(r80, sr80[, "solution_64"])
sr80_64$run <- "solution_64"
sr80_65 <- feature_representation(r80, sr80[, "solution_65"])
sr80_65$run <- "solution_65"
sr80_66 <- feature_representation(r80, sr80[, "solution_66"])
sr80_66$run <- "solution_66"
sr80_67 <- feature_representation(r80, sr80[, "solution_67"])
sr80_67$run <- "solution_67"
sr80_68 <- feature_representation(r80, sr80[, "solution_68"])
sr80_68$run <- "solution_68"
sr80_69 <- feature_representation(r80, sr80[, "solution_69"])
sr80_69$run <- "solution_69"
sr80_70 <- feature_representation(r80, sr80[, "solution_70"])
sr80_70$run <- "solution_70"
sr80_71 <- feature_representation(r80, sr80[, "solution_71"])
sr80_71$run <- "solution_71"
sr80_72 <- feature_representation(r80, sr80[, "solution_72"])
sr80_72$run <- "solution_72"
sr80_73 <- feature_representation(r80, sr80[, "solution_73"])
sr80_73$run <- "solution_73"
sr80_74 <- feature_representation(r80, sr80[, "solution_74"])
sr80_74$run <- "solution_74"
sr80_75 <- feature_representation(r80, sr80[, "solution_75"])
sr80_75$run <- "solution_75"
sr80_76 <- feature_representation(r80, sr80[, "solution_76"])
sr80_76$run <- "solution_76"
sr80_77 <- feature_representation(r80, sr80[, "solution_77"])
sr80_77$run <- "solution_77"
sr80_78 <- feature_representation(r80, sr80[, "solution_78"])
sr80_78$run <- "solution_78"
sr80_79 <- feature_representation(r80, sr80[, "solution_79"])
sr80_79$run <- "solution_79"
sr80_80 <- feature_representation(r80, sr80[, "solution_80"])
sr80_80$run <- "solution_80"
sr80_81 <- feature_representation(r80, sr80[, "solution_81"])
sr80_81$run <- "solution_81"
sr80_82 <- feature_representation(r80, sr80[, "solution_82"])
sr80_82$run <- "solution_82"
sr80_83 <- feature_representation(r80, sr80[, "solution_83"])
sr80_83$run <- "solution_83"
sr80_84 <- feature_representation(r80, sr80[, "solution_84"])
sr80_84$run <- "solution_84"
sr80_85 <- feature_representation(r80, sr80[, "solution_85"])
sr80_85$run <- "solution_85"
sr80_86 <- feature_representation(r80, sr80[, "solution_86"])
sr80_86$run <- "solution_86"
sr80_87 <- feature_representation(r80, sr80[, "solution_87"])
sr80_87$run <- "solution_87"
sr80_88 <- feature_representation(r80, sr80[, "solution_88"])
sr80_88$run <- "solution_88"
sr80_89 <- feature_representation(r80, sr80[, "solution_89"])
sr80_89$run <- "solution_89"
sr80_90 <- feature_representation(r80, sr80[, "solution_90"])
sr80_90$run <- "solution_90"
sr80_91 <- feature_representation(r80, sr80[, "solution_91"])
sr80_91$run <- "solution_91"
sr80_92 <- feature_representation(r80, sr80[, "solution_92"])
sr80_92$run <- "solution_92"
sr80_93 <- feature_representation(r80, sr80[, "solution_93"])
sr80_93$run <- "solution_93"
sr80_94 <- feature_representation(r80, sr80[, "solution_94"])
sr80_94$run <- "solution_94"
sr80_95 <- feature_representation(r80, sr80[, "solution_95"])
sr80_95$run <- "solution_95"
sr80_96 <- feature_representation(r80, sr80[, "solution_96"])
sr80_96$run <- "solution_96"
sr80_97 <- feature_representation(r80, sr80[, "solution_97"])
sr80_97$run <- "solution_97"
sr80_98 <- feature_representation(r80, sr80[, "solution_98"])
sr80_98$run <- "solution_98"
sr80_99 <- feature_representation(r80, sr80[, "solution_99"])
sr80_99$run <- "solution_99"
sr80_100 <- feature_representation(r80, sr80[, "solution_100"])
sr80_100$run <- "solution_100"
sr80_r<-bind_rows(sr80_1,sr80_2,sr80_3,sr80_4,sr80_5,sr80_6,sr80_7,sr80_8,sr80_9,sr80_10,sr80_11,sr80_12,sr80_13,sr80_14,sr80_15,sr80_16,sr80_17,sr80_18,sr80_19,sr80_20,sr80_21,sr80_22,sr80_23,sr80_24,sr80_25,sr80_26,sr80_27,sr80_28,sr80_29,sr80_30,sr80_31,sr80_32,sr80_33,sr80_34,sr80_35,sr80_36,sr80_37,sr80_38,sr80_39,sr80_40,sr80_41,sr80_42,sr80_43,sr80_44,sr80_45,sr80_46,sr80_47,sr80_48,sr80_49,sr80_50,sr80_51,sr80_52,sr80_53,sr80_54,sr80_55,sr80_56,sr80_57,sr80_58,sr80_59,sr80_60,sr80_61,sr80_62,sr80_63,sr80_64,sr80_65,sr80_66,sr80_67,sr80_68,sr80_69,sr80_70,sr80_71,sr80_72,sr80_73,sr80_74,sr80_75,sr80_76,sr80_77,sr80_78,sr80_79,sr80_80,sr80_81,sr80_82,sr80_83,sr80_84,sr80_85,sr80_86,sr80_87,sr80_88,sr80_89,sr80_90,sr80_91,sr80_92,sr80_93,sr80_94,sr80_95,sr80_96,sr80_97,sr80_98,sr80_99,sr80_100)
sr80_r$objective <- "Random"
sr80_r$species<-"80"


feature_a <- feature_abundances(r80, na.rm = FALSE)

sr80r<-left_join(sr80_r,feature_a)


#85
pu_dat85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r85 <- problem(pu_dat85, spec_dat85, cost_column = "cost", rij = puvsp_dat85) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat85$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr85 <- solve(r85)

sr85<-as.data.frame(sr85) %>% as_tibble()

sr85$objective<-"Random"
sr85$solution<-"85"

# Features Dataset
sr85_1 <- feature_representation(r85, sr85[, "solution_1"])
sr85_1$run <- "solution_1"
sr85_2 <- feature_representation(r85, sr85[, "solution_2"])
sr85_2$run <- "solution_2"
sr85_3 <- feature_representation(r85, sr85[, "solution_3"])
sr85_3$run <- "solution_3"
sr85_4 <- feature_representation(r85, sr85[, "solution_4"])
sr85_4$run <- "solution_4"
sr85_5 <- feature_representation(r85, sr85[, "solution_5"])
sr85_5$run <- "solution_5"
sr85_6 <- feature_representation(r85, sr85[, "solution_6"])
sr85_6$run <- "solution_6"
sr85_7 <- feature_representation(r85, sr85[, "solution_7"])
sr85_7$run <- "solution_7"
sr85_8 <- feature_representation(r85, sr85[, "solution_8"])
sr85_8$run <- "solution_8"
sr85_9 <- feature_representation(r85, sr85[, "solution_9"])
sr85_9$run <- "solution_9"
sr85_10 <- feature_representation(r85, sr85[, "solution_10"])
sr85_10$run <- "solution_10"
sr85_11 <- feature_representation(r85, sr85[, "solution_11"])
sr85_11$run <- "solution_11"
sr85_12 <- feature_representation(r85, sr85[, "solution_12"])
sr85_12$run <- "solution_12"
sr85_13 <- feature_representation(r85, sr85[, "solution_13"])
sr85_13$run <- "solution_13"
sr85_14 <- feature_representation(r85, sr85[, "solution_14"])
sr85_14$run <- "solution_14"
sr85_15 <- feature_representation(r85, sr85[, "solution_15"])
sr85_15$run <- "solution_15"
sr85_16 <- feature_representation(r85, sr85[, "solution_16"])
sr85_16$run <- "solution_16"
sr85_17 <- feature_representation(r85, sr85[, "solution_17"])
sr85_17$run <- "solution_17"
sr85_18 <- feature_representation(r85, sr85[, "solution_18"])
sr85_18$run <- "solution_18"
sr85_19 <- feature_representation(r85, sr85[, "solution_19"])
sr85_19$run <- "solution_19"
sr85_20 <- feature_representation(r85, sr85[, "solution_20"])
sr85_20$run <- "solution_20"
sr85_21 <- feature_representation(r85, sr85[, "solution_21"])
sr85_21$run <- "solution_21"
sr85_22 <- feature_representation(r85, sr85[, "solution_22"])
sr85_22$run <- "solution_22"
sr85_23 <- feature_representation(r85, sr85[, "solution_23"])
sr85_23$run <- "solution_23"
sr85_24 <- feature_representation(r85, sr85[, "solution_24"])
sr85_24$run <- "solution_24"
sr85_25 <- feature_representation(r85, sr85[, "solution_25"])
sr85_25$run <- "solution_25"
sr85_26 <- feature_representation(r85, sr85[, "solution_26"])
sr85_26$run <- "solution_26"
sr85_27 <- feature_representation(r85, sr85[, "solution_27"])
sr85_27$run <- "solution_27"
sr85_28 <- feature_representation(r85, sr85[, "solution_28"])
sr85_28$run <- "solution_28"
sr85_29 <- feature_representation(r85, sr85[, "solution_29"])
sr85_29$run <- "solution_29"
sr85_30 <- feature_representation(r85, sr85[, "solution_30"])
sr85_30$run <- "solution_30"
sr85_31 <- feature_representation(r85, sr85[, "solution_31"])
sr85_31$run <- "solution_31"
sr85_32 <- feature_representation(r85, sr85[, "solution_32"])
sr85_32$run <- "solution_32"
sr85_33 <- feature_representation(r85, sr85[, "solution_33"])
sr85_33$run <- "solution_33"
sr85_34 <- feature_representation(r85, sr85[, "solution_34"])
sr85_34$run <- "solution_34"
sr85_35 <- feature_representation(r85, sr85[, "solution_35"])
sr85_35$run <- "solution_35"
sr85_36 <- feature_representation(r85, sr85[, "solution_36"])
sr85_36$run <- "solution_36"
sr85_37 <- feature_representation(r85, sr85[, "solution_37"])
sr85_37$run <- "solution_37"
sr85_38 <- feature_representation(r85, sr85[, "solution_38"])
sr85_38$run <- "solution_38"
sr85_39 <- feature_representation(r85, sr85[, "solution_39"])
sr85_39$run <- "solution_39"
sr85_40 <- feature_representation(r85, sr85[, "solution_40"])
sr85_40$run <- "solution_40"
sr85_41 <- feature_representation(r85, sr85[, "solution_41"])
sr85_41$run <- "solution_41"
sr85_42 <- feature_representation(r85, sr85[, "solution_42"])
sr85_42$run <- "solution_42"
sr85_43 <- feature_representation(r85, sr85[, "solution_43"])
sr85_43$run <- "solution_43"
sr85_44 <- feature_representation(r85, sr85[, "solution_44"])
sr85_44$run <- "solution_44"
sr85_45 <- feature_representation(r85, sr85[, "solution_45"])
sr85_45$run <- "solution_45"
sr85_46 <- feature_representation(r85, sr85[, "solution_46"])
sr85_46$run <- "solution_46"
sr85_47 <- feature_representation(r85, sr85[, "solution_47"])
sr85_47$run <- "solution_47"
sr85_48 <- feature_representation(r85, sr85[, "solution_48"])
sr85_48$run <- "solution_48"
sr85_49 <- feature_representation(r85, sr85[, "solution_49"])
sr85_49$run <- "solution_49"
sr85_50 <- feature_representation(r85, sr85[, "solution_50"])
sr85_50$run <- "solution_50"
sr85_51 <- feature_representation(r85, sr85[, "solution_51"])
sr85_51$run <- "solution_51"
sr85_52 <- feature_representation(r85, sr85[, "solution_52"])
sr85_52$run <- "solution_52"
sr85_53 <- feature_representation(r85, sr85[, "solution_53"])
sr85_53$run <- "solution_53"
sr85_54 <- feature_representation(r85, sr85[, "solution_54"])
sr85_54$run <- "solution_54"
sr85_55 <- feature_representation(r85, sr85[, "solution_55"])
sr85_55$run <- "solution_55"
sr85_56 <- feature_representation(r85, sr85[, "solution_56"])
sr85_56$run <- "solution_56"
sr85_57 <- feature_representation(r85, sr85[, "solution_57"])
sr85_57$run <- "solution_57"
sr85_58 <- feature_representation(r85, sr85[, "solution_58"])
sr85_58$run <- "solution_58"
sr85_59 <- feature_representation(r85, sr85[, "solution_59"])
sr85_59$run <- "solution_59"
sr85_60 <- feature_representation(r85, sr85[, "solution_60"])
sr85_60$run <- "solution_60"
sr85_61 <- feature_representation(r85, sr85[, "solution_61"])
sr85_61$run <- "solution_61"
sr85_62 <- feature_representation(r85, sr85[, "solution_62"])
sr85_62$run <- "solution_62"
sr85_63 <- feature_representation(r85, sr85[, "solution_63"])
sr85_63$run <- "solution_63"
sr85_64 <- feature_representation(r85, sr85[, "solution_64"])
sr85_64$run <- "solution_64"
sr85_65 <- feature_representation(r85, sr85[, "solution_65"])
sr85_65$run <- "solution_65"
sr85_66 <- feature_representation(r85, sr85[, "solution_66"])
sr85_66$run <- "solution_66"
sr85_67 <- feature_representation(r85, sr85[, "solution_67"])
sr85_67$run <- "solution_67"
sr85_68 <- feature_representation(r85, sr85[, "solution_68"])
sr85_68$run <- "solution_68"
sr85_69 <- feature_representation(r85, sr85[, "solution_69"])
sr85_69$run <- "solution_69"
sr85_70 <- feature_representation(r85, sr85[, "solution_70"])
sr85_70$run <- "solution_70"
sr85_71 <- feature_representation(r85, sr85[, "solution_71"])
sr85_71$run <- "solution_71"
sr85_72 <- feature_representation(r85, sr85[, "solution_72"])
sr85_72$run <- "solution_72"
sr85_73 <- feature_representation(r85, sr85[, "solution_73"])
sr85_73$run <- "solution_73"
sr85_74 <- feature_representation(r85, sr85[, "solution_74"])
sr85_74$run <- "solution_74"
sr85_75 <- feature_representation(r85, sr85[, "solution_75"])
sr85_75$run <- "solution_75"
sr85_76 <- feature_representation(r85, sr85[, "solution_76"])
sr85_76$run <- "solution_76"
sr85_77 <- feature_representation(r85, sr85[, "solution_77"])
sr85_77$run <- "solution_77"
sr85_78 <- feature_representation(r85, sr85[, "solution_78"])
sr85_78$run <- "solution_78"
sr85_79 <- feature_representation(r85, sr85[, "solution_79"])
sr85_79$run <- "solution_79"
sr85_80 <- feature_representation(r85, sr85[, "solution_80"])
sr85_80$run <- "solution_80"
sr85_81 <- feature_representation(r85, sr85[, "solution_81"])
sr85_81$run <- "solution_81"
sr85_82 <- feature_representation(r85, sr85[, "solution_82"])
sr85_82$run <- "solution_82"
sr85_83 <- feature_representation(r85, sr85[, "solution_83"])
sr85_83$run <- "solution_83"
sr85_84 <- feature_representation(r85, sr85[, "solution_84"])
sr85_84$run <- "solution_84"
sr85_85 <- feature_representation(r85, sr85[, "solution_85"])
sr85_85$run <- "solution_85"
sr85_86 <- feature_representation(r85, sr85[, "solution_86"])
sr85_86$run <- "solution_86"
sr85_87 <- feature_representation(r85, sr85[, "solution_87"])
sr85_87$run <- "solution_87"
sr85_88 <- feature_representation(r85, sr85[, "solution_88"])
sr85_88$run <- "solution_88"
sr85_89 <- feature_representation(r85, sr85[, "solution_89"])
sr85_89$run <- "solution_89"
sr85_90 <- feature_representation(r85, sr85[, "solution_90"])
sr85_90$run <- "solution_90"
sr85_91 <- feature_representation(r85, sr85[, "solution_91"])
sr85_91$run <- "solution_91"
sr85_92 <- feature_representation(r85, sr85[, "solution_92"])
sr85_92$run <- "solution_92"
sr85_93 <- feature_representation(r85, sr85[, "solution_93"])
sr85_93$run <- "solution_93"
sr85_94 <- feature_representation(r85, sr85[, "solution_94"])
sr85_94$run <- "solution_94"
sr85_95 <- feature_representation(r85, sr85[, "solution_95"])
sr85_95$run <- "solution_95"
sr85_96 <- feature_representation(r85, sr85[, "solution_96"])
sr85_96$run <- "solution_96"
sr85_97 <- feature_representation(r85, sr85[, "solution_97"])
sr85_97$run <- "solution_97"
sr85_98 <- feature_representation(r85, sr85[, "solution_98"])
sr85_98$run <- "solution_98"
sr85_99 <- feature_representation(r85, sr85[, "solution_99"])
sr85_99$run <- "solution_99"
sr85_100 <- feature_representation(r85, sr85[, "solution_100"])
sr85_100$run <- "solution_100"
sr85_r<-bind_rows(sr85_1,sr85_2,sr85_3,sr85_4,sr85_5,sr85_6,sr85_7,sr85_8,sr85_9,sr85_10,sr85_11,sr85_12,sr85_13,sr85_14,sr85_15,sr85_16,sr85_17,sr85_18,sr85_19,sr85_20,sr85_21,sr85_22,sr85_23,sr85_24,sr85_25,sr85_26,sr85_27,sr85_28,sr85_29,sr85_30,sr85_31,sr85_32,sr85_33,sr85_34,sr85_35,sr85_36,sr85_37,sr85_38,sr85_39,sr85_40,sr85_41,sr85_42,sr85_43,sr85_44,sr85_45,sr85_46,sr85_47,sr85_48,sr85_49,sr85_50,sr85_51,sr85_52,sr85_53,sr85_54,sr85_55,sr85_56,sr85_57,sr85_58,sr85_59,sr85_60,sr85_61,sr85_62,sr85_63,sr85_64,sr85_65,sr85_66,sr85_67,sr85_68,sr85_69,sr85_70,sr85_71,sr85_72,sr85_73,sr85_74,sr85_75,sr85_76,sr85_77,sr85_78,sr85_79,sr85_80,sr85_81,sr85_82,sr85_83,sr85_84,sr85_85,sr85_86,sr85_87,sr85_88,sr85_89,sr85_90,sr85_91,sr85_92,sr85_93,sr85_94,sr85_95,sr85_96,sr85_97,sr85_98,sr85_99,sr85_100)
sr85_r$objective <- "Random"
sr85_r$species<-"85"


feature_a <- feature_abundances(r85, na.rm = FALSE)

sr85r<-left_join(sr85_r,feature_a)


#90
pu_dat90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r90 <- problem(pu_dat90, spec_dat90, cost_column = "cost", rij = puvsp_dat90) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat90$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr90 <- solve(r90)

sr90<-as.data.frame(sr90) %>% as_tibble()

sr90$objective<-"Random"
sr90$solution<-"90"

# Features Dataset
sr90_1 <- feature_representation(r90, sr90[, "solution_1"])
sr90_1$run <- "solution_1"
sr90_2 <- feature_representation(r90, sr90[, "solution_2"])
sr90_2$run <- "solution_2"
sr90_3 <- feature_representation(r90, sr90[, "solution_3"])
sr90_3$run <- "solution_3"
sr90_4 <- feature_representation(r90, sr90[, "solution_4"])
sr90_4$run <- "solution_4"
sr90_5 <- feature_representation(r90, sr90[, "solution_5"])
sr90_5$run <- "solution_5"
sr90_6 <- feature_representation(r90, sr90[, "solution_6"])
sr90_6$run <- "solution_6"
sr90_7 <- feature_representation(r90, sr90[, "solution_7"])
sr90_7$run <- "solution_7"
sr90_8 <- feature_representation(r90, sr90[, "solution_8"])
sr90_8$run <- "solution_8"
sr90_9 <- feature_representation(r90, sr90[, "solution_9"])
sr90_9$run <- "solution_9"
sr90_10 <- feature_representation(r90, sr90[, "solution_10"])
sr90_10$run <- "solution_10"
sr90_11 <- feature_representation(r90, sr90[, "solution_11"])
sr90_11$run <- "solution_11"
sr90_12 <- feature_representation(r90, sr90[, "solution_12"])
sr90_12$run <- "solution_12"
sr90_13 <- feature_representation(r90, sr90[, "solution_13"])
sr90_13$run <- "solution_13"
sr90_14 <- feature_representation(r90, sr90[, "solution_14"])
sr90_14$run <- "solution_14"
sr90_15 <- feature_representation(r90, sr90[, "solution_15"])
sr90_15$run <- "solution_15"
sr90_16 <- feature_representation(r90, sr90[, "solution_16"])
sr90_16$run <- "solution_16"
sr90_17 <- feature_representation(r90, sr90[, "solution_17"])
sr90_17$run <- "solution_17"
sr90_18 <- feature_representation(r90, sr90[, "solution_18"])
sr90_18$run <- "solution_18"
sr90_19 <- feature_representation(r90, sr90[, "solution_19"])
sr90_19$run <- "solution_19"
sr90_20 <- feature_representation(r90, sr90[, "solution_20"])
sr90_20$run <- "solution_20"
sr90_21 <- feature_representation(r90, sr90[, "solution_21"])
sr90_21$run <- "solution_21"
sr90_22 <- feature_representation(r90, sr90[, "solution_22"])
sr90_22$run <- "solution_22"
sr90_23 <- feature_representation(r90, sr90[, "solution_23"])
sr90_23$run <- "solution_23"
sr90_24 <- feature_representation(r90, sr90[, "solution_24"])
sr90_24$run <- "solution_24"
sr90_25 <- feature_representation(r90, sr90[, "solution_25"])
sr90_25$run <- "solution_25"
sr90_26 <- feature_representation(r90, sr90[, "solution_26"])
sr90_26$run <- "solution_26"
sr90_27 <- feature_representation(r90, sr90[, "solution_27"])
sr90_27$run <- "solution_27"
sr90_28 <- feature_representation(r90, sr90[, "solution_28"])
sr90_28$run <- "solution_28"
sr90_29 <- feature_representation(r90, sr90[, "solution_29"])
sr90_29$run <- "solution_29"
sr90_30 <- feature_representation(r90, sr90[, "solution_30"])
sr90_30$run <- "solution_30"
sr90_31 <- feature_representation(r90, sr90[, "solution_31"])
sr90_31$run <- "solution_31"
sr90_32 <- feature_representation(r90, sr90[, "solution_32"])
sr90_32$run <- "solution_32"
sr90_33 <- feature_representation(r90, sr90[, "solution_33"])
sr90_33$run <- "solution_33"
sr90_34 <- feature_representation(r90, sr90[, "solution_34"])
sr90_34$run <- "solution_34"
sr90_35 <- feature_representation(r90, sr90[, "solution_35"])
sr90_35$run <- "solution_35"
sr90_36 <- feature_representation(r90, sr90[, "solution_36"])
sr90_36$run <- "solution_36"
sr90_37 <- feature_representation(r90, sr90[, "solution_37"])
sr90_37$run <- "solution_37"
sr90_38 <- feature_representation(r90, sr90[, "solution_38"])
sr90_38$run <- "solution_38"
sr90_39 <- feature_representation(r90, sr90[, "solution_39"])
sr90_39$run <- "solution_39"
sr90_40 <- feature_representation(r90, sr90[, "solution_40"])
sr90_40$run <- "solution_40"
sr90_41 <- feature_representation(r90, sr90[, "solution_41"])
sr90_41$run <- "solution_41"
sr90_42 <- feature_representation(r90, sr90[, "solution_42"])
sr90_42$run <- "solution_42"
sr90_43 <- feature_representation(r90, sr90[, "solution_43"])
sr90_43$run <- "solution_43"
sr90_44 <- feature_representation(r90, sr90[, "solution_44"])
sr90_44$run <- "solution_44"
sr90_45 <- feature_representation(r90, sr90[, "solution_45"])
sr90_45$run <- "solution_45"
sr90_46 <- feature_representation(r90, sr90[, "solution_46"])
sr90_46$run <- "solution_46"
sr90_47 <- feature_representation(r90, sr90[, "solution_47"])
sr90_47$run <- "solution_47"
sr90_48 <- feature_representation(r90, sr90[, "solution_48"])
sr90_48$run <- "solution_48"
sr90_49 <- feature_representation(r90, sr90[, "solution_49"])
sr90_49$run <- "solution_49"
sr90_50 <- feature_representation(r90, sr90[, "solution_50"])
sr90_50$run <- "solution_50"
sr90_51 <- feature_representation(r90, sr90[, "solution_51"])
sr90_51$run <- "solution_51"
sr90_52 <- feature_representation(r90, sr90[, "solution_52"])
sr90_52$run <- "solution_52"
sr90_53 <- feature_representation(r90, sr90[, "solution_53"])
sr90_53$run <- "solution_53"
sr90_54 <- feature_representation(r90, sr90[, "solution_54"])
sr90_54$run <- "solution_54"
sr90_55 <- feature_representation(r90, sr90[, "solution_55"])
sr90_55$run <- "solution_55"
sr90_56 <- feature_representation(r90, sr90[, "solution_56"])
sr90_56$run <- "solution_56"
sr90_57 <- feature_representation(r90, sr90[, "solution_57"])
sr90_57$run <- "solution_57"
sr90_58 <- feature_representation(r90, sr90[, "solution_58"])
sr90_58$run <- "solution_58"
sr90_59 <- feature_representation(r90, sr90[, "solution_59"])
sr90_59$run <- "solution_59"
sr90_60 <- feature_representation(r90, sr90[, "solution_60"])
sr90_60$run <- "solution_60"
sr90_61 <- feature_representation(r90, sr90[, "solution_61"])
sr90_61$run <- "solution_61"
sr90_62 <- feature_representation(r90, sr90[, "solution_62"])
sr90_62$run <- "solution_62"
sr90_63 <- feature_representation(r90, sr90[, "solution_63"])
sr90_63$run <- "solution_63"
sr90_64 <- feature_representation(r90, sr90[, "solution_64"])
sr90_64$run <- "solution_64"
sr90_65 <- feature_representation(r90, sr90[, "solution_65"])
sr90_65$run <- "solution_65"
sr90_66 <- feature_representation(r90, sr90[, "solution_66"])
sr90_66$run <- "solution_66"
sr90_67 <- feature_representation(r90, sr90[, "solution_67"])
sr90_67$run <- "solution_67"
sr90_68 <- feature_representation(r90, sr90[, "solution_68"])
sr90_68$run <- "solution_68"
sr90_69 <- feature_representation(r90, sr90[, "solution_69"])
sr90_69$run <- "solution_69"
sr90_70 <- feature_representation(r90, sr90[, "solution_70"])
sr90_70$run <- "solution_70"
sr90_71 <- feature_representation(r90, sr90[, "solution_71"])
sr90_71$run <- "solution_71"
sr90_72 <- feature_representation(r90, sr90[, "solution_72"])
sr90_72$run <- "solution_72"
sr90_73 <- feature_representation(r90, sr90[, "solution_73"])
sr90_73$run <- "solution_73"
sr90_74 <- feature_representation(r90, sr90[, "solution_74"])
sr90_74$run <- "solution_74"
sr90_75 <- feature_representation(r90, sr90[, "solution_75"])
sr90_75$run <- "solution_75"
sr90_76 <- feature_representation(r90, sr90[, "solution_76"])
sr90_76$run <- "solution_76"
sr90_77 <- feature_representation(r90, sr90[, "solution_77"])
sr90_77$run <- "solution_77"
sr90_78 <- feature_representation(r90, sr90[, "solution_78"])
sr90_78$run <- "solution_78"
sr90_79 <- feature_representation(r90, sr90[, "solution_79"])
sr90_79$run <- "solution_79"
sr90_80 <- feature_representation(r90, sr90[, "solution_80"])
sr90_80$run <- "solution_80"
sr90_81 <- feature_representation(r90, sr90[, "solution_81"])
sr90_81$run <- "solution_81"
sr90_82 <- feature_representation(r90, sr90[, "solution_82"])
sr90_82$run <- "solution_82"
sr90_83 <- feature_representation(r90, sr90[, "solution_83"])
sr90_83$run <- "solution_83"
sr90_84 <- feature_representation(r90, sr90[, "solution_84"])
sr90_84$run <- "solution_84"
sr90_85 <- feature_representation(r90, sr90[, "solution_85"])
sr90_85$run <- "solution_85"
sr90_86 <- feature_representation(r90, sr90[, "solution_86"])
sr90_86$run <- "solution_86"
sr90_87 <- feature_representation(r90, sr90[, "solution_87"])
sr90_87$run <- "solution_87"
sr90_88 <- feature_representation(r90, sr90[, "solution_88"])
sr90_88$run <- "solution_88"
sr90_89 <- feature_representation(r90, sr90[, "solution_89"])
sr90_89$run <- "solution_89"
sr90_90 <- feature_representation(r90, sr90[, "solution_90"])
sr90_90$run <- "solution_90"
sr90_91 <- feature_representation(r90, sr90[, "solution_91"])
sr90_91$run <- "solution_91"
sr90_92 <- feature_representation(r90, sr90[, "solution_92"])
sr90_92$run <- "solution_92"
sr90_93 <- feature_representation(r90, sr90[, "solution_93"])
sr90_93$run <- "solution_93"
sr90_94 <- feature_representation(r90, sr90[, "solution_94"])
sr90_94$run <- "solution_94"
sr90_95 <- feature_representation(r90, sr90[, "solution_95"])
sr90_95$run <- "solution_95"
sr90_96 <- feature_representation(r90, sr90[, "solution_96"])
sr90_96$run <- "solution_96"
sr90_97 <- feature_representation(r90, sr90[, "solution_97"])
sr90_97$run <- "solution_97"
sr90_98 <- feature_representation(r90, sr90[, "solution_98"])
sr90_98$run <- "solution_98"
sr90_99 <- feature_representation(r90, sr90[, "solution_99"])
sr90_99$run <- "solution_99"
sr90_100 <- feature_representation(r90, sr90[, "solution_100"])
sr90_100$run <- "solution_100"
sr90_r<-bind_rows(sr90_1,sr90_2,sr90_3,sr90_4,sr90_5,sr90_6,sr90_7,sr90_8,sr90_9,sr90_10,sr90_11,sr90_12,sr90_13,sr90_14,sr90_15,sr90_16,sr90_17,sr90_18,sr90_19,sr90_20,sr90_21,sr90_22,sr90_23,sr90_24,sr90_25,sr90_26,sr90_27,sr90_28,sr90_29,sr90_30,sr90_31,sr90_32,sr90_33,sr90_34,sr90_35,sr90_36,sr90_37,sr90_38,sr90_39,sr90_40,sr90_41,sr90_42,sr90_43,sr90_44,sr90_45,sr90_46,sr90_47,sr90_48,sr90_49,sr90_50,sr90_51,sr90_52,sr90_53,sr90_54,sr90_55,sr90_56,sr90_57,sr90_58,sr90_59,sr90_60,sr90_61,sr90_62,sr90_63,sr90_64,sr90_65,sr90_66,sr90_67,sr90_68,sr90_69,sr90_70,sr90_71,sr90_72,sr90_73,sr90_74,sr90_75,sr90_76,sr90_77,sr90_78,sr90_79,sr90_80,sr90_81,sr90_82,sr90_83,sr90_84,sr90_85,sr90_86,sr90_87,sr90_88,sr90_89,sr90_90,sr90_91,sr90_92,sr90_93,sr90_94,sr90_95,sr90_96,sr90_97,sr90_98,sr90_99,sr90_100)
sr90_r$objective <- "Random"
sr90_r$species<-"90"


feature_a <- feature_abundances(r90, na.rm = FALSE)

sr90r<-left_join(sr90_r,feature_a)

#95
pu_dat95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r95 <- problem(pu_dat95, spec_dat95, cost_column = "cost", rij = puvsp_dat95) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat95$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr95 <- solve(r95)

sr95<-as.data.frame(sr95) %>% as_tibble()

sr95$objective<-"Random"
sr95$solution<-"95"

# Features Dataset
sr95_1 <- feature_representation(r95, sr95[, "solution_1"])
sr95_1$run <- "solution_1"
sr95_2 <- feature_representation(r95, sr95[, "solution_2"])
sr95_2$run <- "solution_2"
sr95_3 <- feature_representation(r95, sr95[, "solution_3"])
sr95_3$run <- "solution_3"
sr95_4 <- feature_representation(r95, sr95[, "solution_4"])
sr95_4$run <- "solution_4"
sr95_5 <- feature_representation(r95, sr95[, "solution_5"])
sr95_5$run <- "solution_5"
sr95_6 <- feature_representation(r95, sr95[, "solution_6"])
sr95_6$run <- "solution_6"
sr95_7 <- feature_representation(r95, sr95[, "solution_7"])
sr95_7$run <- "solution_7"
sr95_8 <- feature_representation(r95, sr95[, "solution_8"])
sr95_8$run <- "solution_8"
sr95_9 <- feature_representation(r95, sr95[, "solution_9"])
sr95_9$run <- "solution_9"
sr95_10 <- feature_representation(r95, sr95[, "solution_10"])
sr95_10$run <- "solution_10"
sr95_11 <- feature_representation(r95, sr95[, "solution_11"])
sr95_11$run <- "solution_11"
sr95_12 <- feature_representation(r95, sr95[, "solution_12"])
sr95_12$run <- "solution_12"
sr95_13 <- feature_representation(r95, sr95[, "solution_13"])
sr95_13$run <- "solution_13"
sr95_14 <- feature_representation(r95, sr95[, "solution_14"])
sr95_14$run <- "solution_14"
sr95_15 <- feature_representation(r95, sr95[, "solution_15"])
sr95_15$run <- "solution_15"
sr95_16 <- feature_representation(r95, sr95[, "solution_16"])
sr95_16$run <- "solution_16"
sr95_17 <- feature_representation(r95, sr95[, "solution_17"])
sr95_17$run <- "solution_17"
sr95_18 <- feature_representation(r95, sr95[, "solution_18"])
sr95_18$run <- "solution_18"
sr95_19 <- feature_representation(r95, sr95[, "solution_19"])
sr95_19$run <- "solution_19"
sr95_20 <- feature_representation(r95, sr95[, "solution_20"])
sr95_20$run <- "solution_20"
sr95_21 <- feature_representation(r95, sr95[, "solution_21"])
sr95_21$run <- "solution_21"
sr95_22 <- feature_representation(r95, sr95[, "solution_22"])
sr95_22$run <- "solution_22"
sr95_23 <- feature_representation(r95, sr95[, "solution_23"])
sr95_23$run <- "solution_23"
sr95_24 <- feature_representation(r95, sr95[, "solution_24"])
sr95_24$run <- "solution_24"
sr95_25 <- feature_representation(r95, sr95[, "solution_25"])
sr95_25$run <- "solution_25"
sr95_26 <- feature_representation(r95, sr95[, "solution_26"])
sr95_26$run <- "solution_26"
sr95_27 <- feature_representation(r95, sr95[, "solution_27"])
sr95_27$run <- "solution_27"
sr95_28 <- feature_representation(r95, sr95[, "solution_28"])
sr95_28$run <- "solution_28"
sr95_29 <- feature_representation(r95, sr95[, "solution_29"])
sr95_29$run <- "solution_29"
sr95_30 <- feature_representation(r95, sr95[, "solution_30"])
sr95_30$run <- "solution_30"
sr95_31 <- feature_representation(r95, sr95[, "solution_31"])
sr95_31$run <- "solution_31"
sr95_32 <- feature_representation(r95, sr95[, "solution_32"])
sr95_32$run <- "solution_32"
sr95_33 <- feature_representation(r95, sr95[, "solution_33"])
sr95_33$run <- "solution_33"
sr95_34 <- feature_representation(r95, sr95[, "solution_34"])
sr95_34$run <- "solution_34"
sr95_35 <- feature_representation(r95, sr95[, "solution_35"])
sr95_35$run <- "solution_35"
sr95_36 <- feature_representation(r95, sr95[, "solution_36"])
sr95_36$run <- "solution_36"
sr95_37 <- feature_representation(r95, sr95[, "solution_37"])
sr95_37$run <- "solution_37"
sr95_38 <- feature_representation(r95, sr95[, "solution_38"])
sr95_38$run <- "solution_38"
sr95_39 <- feature_representation(r95, sr95[, "solution_39"])
sr95_39$run <- "solution_39"
sr95_40 <- feature_representation(r95, sr95[, "solution_40"])
sr95_40$run <- "solution_40"
sr95_41 <- feature_representation(r95, sr95[, "solution_41"])
sr95_41$run <- "solution_41"
sr95_42 <- feature_representation(r95, sr95[, "solution_42"])
sr95_42$run <- "solution_42"
sr95_43 <- feature_representation(r95, sr95[, "solution_43"])
sr95_43$run <- "solution_43"
sr95_44 <- feature_representation(r95, sr95[, "solution_44"])
sr95_44$run <- "solution_44"
sr95_45 <- feature_representation(r95, sr95[, "solution_45"])
sr95_45$run <- "solution_45"
sr95_46 <- feature_representation(r95, sr95[, "solution_46"])
sr95_46$run <- "solution_46"
sr95_47 <- feature_representation(r95, sr95[, "solution_47"])
sr95_47$run <- "solution_47"
sr95_48 <- feature_representation(r95, sr95[, "solution_48"])
sr95_48$run <- "solution_48"
sr95_49 <- feature_representation(r95, sr95[, "solution_49"])
sr95_49$run <- "solution_49"
sr95_50 <- feature_representation(r95, sr95[, "solution_50"])
sr95_50$run <- "solution_50"
sr95_51 <- feature_representation(r95, sr95[, "solution_51"])
sr95_51$run <- "solution_51"
sr95_52 <- feature_representation(r95, sr95[, "solution_52"])
sr95_52$run <- "solution_52"
sr95_53 <- feature_representation(r95, sr95[, "solution_53"])
sr95_53$run <- "solution_53"
sr95_54 <- feature_representation(r95, sr95[, "solution_54"])
sr95_54$run <- "solution_54"
sr95_55 <- feature_representation(r95, sr95[, "solution_55"])
sr95_55$run <- "solution_55"
sr95_56 <- feature_representation(r95, sr95[, "solution_56"])
sr95_56$run <- "solution_56"
sr95_57 <- feature_representation(r95, sr95[, "solution_57"])
sr95_57$run <- "solution_57"
sr95_58 <- feature_representation(r95, sr95[, "solution_58"])
sr95_58$run <- "solution_58"
sr95_59 <- feature_representation(r95, sr95[, "solution_59"])
sr95_59$run <- "solution_59"
sr95_60 <- feature_representation(r95, sr95[, "solution_60"])
sr95_60$run <- "solution_60"
sr95_61 <- feature_representation(r95, sr95[, "solution_61"])
sr95_61$run <- "solution_61"
sr95_62 <- feature_representation(r95, sr95[, "solution_62"])
sr95_62$run <- "solution_62"
sr95_63 <- feature_representation(r95, sr95[, "solution_63"])
sr95_63$run <- "solution_63"
sr95_64 <- feature_representation(r95, sr95[, "solution_64"])
sr95_64$run <- "solution_64"
sr95_65 <- feature_representation(r95, sr95[, "solution_65"])
sr95_65$run <- "solution_65"
sr95_66 <- feature_representation(r95, sr95[, "solution_66"])
sr95_66$run <- "solution_66"
sr95_67 <- feature_representation(r95, sr95[, "solution_67"])
sr95_67$run <- "solution_67"
sr95_68 <- feature_representation(r95, sr95[, "solution_68"])
sr95_68$run <- "solution_68"
sr95_69 <- feature_representation(r95, sr95[, "solution_69"])
sr95_69$run <- "solution_69"
sr95_70 <- feature_representation(r95, sr95[, "solution_70"])
sr95_70$run <- "solution_70"
sr95_71 <- feature_representation(r95, sr95[, "solution_71"])
sr95_71$run <- "solution_71"
sr95_72 <- feature_representation(r95, sr95[, "solution_72"])
sr95_72$run <- "solution_72"
sr95_73 <- feature_representation(r95, sr95[, "solution_73"])
sr95_73$run <- "solution_73"
sr95_74 <- feature_representation(r95, sr95[, "solution_74"])
sr95_74$run <- "solution_74"
sr95_75 <- feature_representation(r95, sr95[, "solution_75"])
sr95_75$run <- "solution_75"
sr95_76 <- feature_representation(r95, sr95[, "solution_76"])
sr95_76$run <- "solution_76"
sr95_77 <- feature_representation(r95, sr95[, "solution_77"])
sr95_77$run <- "solution_77"
sr95_78 <- feature_representation(r95, sr95[, "solution_78"])
sr95_78$run <- "solution_78"
sr95_79 <- feature_representation(r95, sr95[, "solution_79"])
sr95_79$run <- "solution_79"
sr95_80 <- feature_representation(r95, sr95[, "solution_80"])
sr95_80$run <- "solution_80"
sr95_81 <- feature_representation(r95, sr95[, "solution_81"])
sr95_81$run <- "solution_81"
sr95_82 <- feature_representation(r95, sr95[, "solution_82"])
sr95_82$run <- "solution_82"
sr95_83 <- feature_representation(r95, sr95[, "solution_83"])
sr95_83$run <- "solution_83"
sr95_84 <- feature_representation(r95, sr95[, "solution_84"])
sr95_84$run <- "solution_84"
sr95_85 <- feature_representation(r95, sr95[, "solution_85"])
sr95_85$run <- "solution_85"
sr95_86 <- feature_representation(r95, sr95[, "solution_86"])
sr95_86$run <- "solution_86"
sr95_87 <- feature_representation(r95, sr95[, "solution_87"])
sr95_87$run <- "solution_87"
sr95_88 <- feature_representation(r95, sr95[, "solution_88"])
sr95_88$run <- "solution_88"
sr95_89 <- feature_representation(r95, sr95[, "solution_89"])
sr95_89$run <- "solution_89"
sr95_90 <- feature_representation(r95, sr95[, "solution_90"])
sr95_90$run <- "solution_90"
sr95_91 <- feature_representation(r95, sr95[, "solution_91"])
sr95_91$run <- "solution_91"
sr95_92 <- feature_representation(r95, sr95[, "solution_92"])
sr95_92$run <- "solution_92"
sr95_93 <- feature_representation(r95, sr95[, "solution_93"])
sr95_93$run <- "solution_93"
sr95_94 <- feature_representation(r95, sr95[, "solution_94"])
sr95_94$run <- "solution_94"
sr95_95 <- feature_representation(r95, sr95[, "solution_95"])
sr95_95$run <- "solution_95"
sr95_96 <- feature_representation(r95, sr95[, "solution_96"])
sr95_96$run <- "solution_96"
sr95_97 <- feature_representation(r95, sr95[, "solution_97"])
sr95_97$run <- "solution_97"
sr95_98 <- feature_representation(r95, sr95[, "solution_98"])
sr95_98$run <- "solution_98"
sr95_99 <- feature_representation(r95, sr95[, "solution_99"])
sr95_99$run <- "solution_99"
sr95_100 <- feature_representation(r95, sr95[, "solution_100"])
sr95_100$run <- "solution_100"
sr95_r<-bind_rows(sr95_1,sr95_2,sr95_3,sr95_4,sr95_5,sr95_6,sr95_7,sr95_8,sr95_9,sr95_10,sr95_11,sr95_12,sr95_13,sr95_14,sr95_15,sr95_16,sr95_17,sr95_18,sr95_19,sr95_20,sr95_21,sr95_22,sr95_23,sr95_24,sr95_25,sr95_26,sr95_27,sr95_28,sr95_29,sr95_30,sr95_31,sr95_32,sr95_33,sr95_34,sr95_35,sr95_36,sr95_37,sr95_38,sr95_39,sr95_40,sr95_41,sr95_42,sr95_43,sr95_44,sr95_45,sr95_46,sr95_47,sr95_48,sr95_49,sr95_50,sr95_51,sr95_52,sr95_53,sr95_54,sr95_55,sr95_56,sr95_57,sr95_58,sr95_59,sr95_60,sr95_61,sr95_62,sr95_63,sr95_64,sr95_65,sr95_66,sr95_67,sr95_68,sr95_69,sr95_70,sr95_71,sr95_72,sr95_73,sr95_74,sr95_75,sr95_76,sr95_77,sr95_78,sr95_79,sr95_80,sr95_81,sr95_82,sr95_83,sr95_84,sr95_85,sr95_86,sr95_87,sr95_88,sr95_89,sr95_90,sr95_91,sr95_92,sr95_93,sr95_94,sr95_95,sr95_96,sr95_97,sr95_98,sr95_99,sr95_100)
sr95_r$objective <- "Random"
sr95_r$species<-"95"


feature_a <- feature_abundances(r95, na.rm = FALSE)

sr95r<-left_join(sr95_r,feature_a)


#100
pu_dat100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r100 <- problem(pu_dat100, spec_dat100, cost_column = "cost", rij = puvsp_dat100) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat100$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr100 <- solve(r100)

sr100<-as.data.frame(sr100) %>% as_tibble()

sr100$objective<-"Random"
sr100$solution<-"100"

# Features Dataset
sr100_1 <- feature_representation(r100, sr100[, "solution_1"])
sr100_1$run <- "solution_1"
sr100_2 <- feature_representation(r100, sr100[, "solution_2"])
sr100_2$run <- "solution_2"
sr100_3 <- feature_representation(r100, sr100[, "solution_3"])
sr100_3$run <- "solution_3"
sr100_4 <- feature_representation(r100, sr100[, "solution_4"])
sr100_4$run <- "solution_4"
sr100_5 <- feature_representation(r100, sr100[, "solution_5"])
sr100_5$run <- "solution_5"
sr100_6 <- feature_representation(r100, sr100[, "solution_6"])
sr100_6$run <- "solution_6"
sr100_7 <- feature_representation(r100, sr100[, "solution_7"])
sr100_7$run <- "solution_7"
sr100_8 <- feature_representation(r100, sr100[, "solution_8"])
sr100_8$run <- "solution_8"
sr100_9 <- feature_representation(r100, sr100[, "solution_9"])
sr100_9$run <- "solution_9"
sr100_10 <- feature_representation(r100, sr100[, "solution_10"])
sr100_10$run <- "solution_10"
sr100_11 <- feature_representation(r100, sr100[, "solution_11"])
sr100_11$run <- "solution_11"
sr100_12 <- feature_representation(r100, sr100[, "solution_12"])
sr100_12$run <- "solution_12"
sr100_13 <- feature_representation(r100, sr100[, "solution_13"])
sr100_13$run <- "solution_13"
sr100_14 <- feature_representation(r100, sr100[, "solution_14"])
sr100_14$run <- "solution_14"
sr100_15 <- feature_representation(r100, sr100[, "solution_15"])
sr100_15$run <- "solution_15"
sr100_16 <- feature_representation(r100, sr100[, "solution_16"])
sr100_16$run <- "solution_16"
sr100_17 <- feature_representation(r100, sr100[, "solution_17"])
sr100_17$run <- "solution_17"
sr100_18 <- feature_representation(r100, sr100[, "solution_18"])
sr100_18$run <- "solution_18"
sr100_19 <- feature_representation(r100, sr100[, "solution_19"])
sr100_19$run <- "solution_19"
sr100_20 <- feature_representation(r100, sr100[, "solution_20"])
sr100_20$run <- "solution_20"
sr100_21 <- feature_representation(r100, sr100[, "solution_21"])
sr100_21$run <- "solution_21"
sr100_22 <- feature_representation(r100, sr100[, "solution_22"])
sr100_22$run <- "solution_22"
sr100_23 <- feature_representation(r100, sr100[, "solution_23"])
sr100_23$run <- "solution_23"
sr100_24 <- feature_representation(r100, sr100[, "solution_24"])
sr100_24$run <- "solution_24"
sr100_25 <- feature_representation(r100, sr100[, "solution_25"])
sr100_25$run <- "solution_25"
sr100_26 <- feature_representation(r100, sr100[, "solution_26"])
sr100_26$run <- "solution_26"
sr100_27 <- feature_representation(r100, sr100[, "solution_27"])
sr100_27$run <- "solution_27"
sr100_28 <- feature_representation(r100, sr100[, "solution_28"])
sr100_28$run <- "solution_28"
sr100_29 <- feature_representation(r100, sr100[, "solution_29"])
sr100_29$run <- "solution_29"
sr100_30 <- feature_representation(r100, sr100[, "solution_30"])
sr100_30$run <- "solution_30"
sr100_31 <- feature_representation(r100, sr100[, "solution_31"])
sr100_31$run <- "solution_31"
sr100_32 <- feature_representation(r100, sr100[, "solution_32"])
sr100_32$run <- "solution_32"
sr100_33 <- feature_representation(r100, sr100[, "solution_33"])
sr100_33$run <- "solution_33"
sr100_34 <- feature_representation(r100, sr100[, "solution_34"])
sr100_34$run <- "solution_34"
sr100_35 <- feature_representation(r100, sr100[, "solution_35"])
sr100_35$run <- "solution_35"
sr100_36 <- feature_representation(r100, sr100[, "solution_36"])
sr100_36$run <- "solution_36"
sr100_37 <- feature_representation(r100, sr100[, "solution_37"])
sr100_37$run <- "solution_37"
sr100_38 <- feature_representation(r100, sr100[, "solution_38"])
sr100_38$run <- "solution_38"
sr100_39 <- feature_representation(r100, sr100[, "solution_39"])
sr100_39$run <- "solution_39"
sr100_40 <- feature_representation(r100, sr100[, "solution_40"])
sr100_40$run <- "solution_40"
sr100_41 <- feature_representation(r100, sr100[, "solution_41"])
sr100_41$run <- "solution_41"
sr100_42 <- feature_representation(r100, sr100[, "solution_42"])
sr100_42$run <- "solution_42"
sr100_43 <- feature_representation(r100, sr100[, "solution_43"])
sr100_43$run <- "solution_43"
sr100_44 <- feature_representation(r100, sr100[, "solution_44"])
sr100_44$run <- "solution_44"
sr100_45 <- feature_representation(r100, sr100[, "solution_45"])
sr100_45$run <- "solution_45"
sr100_46 <- feature_representation(r100, sr100[, "solution_46"])
sr100_46$run <- "solution_46"
sr100_47 <- feature_representation(r100, sr100[, "solution_47"])
sr100_47$run <- "solution_47"
sr100_48 <- feature_representation(r100, sr100[, "solution_48"])
sr100_48$run <- "solution_48"
sr100_49 <- feature_representation(r100, sr100[, "solution_49"])
sr100_49$run <- "solution_49"
sr100_50 <- feature_representation(r100, sr100[, "solution_50"])
sr100_50$run <- "solution_50"
sr100_51 <- feature_representation(r100, sr100[, "solution_51"])
sr100_51$run <- "solution_51"
sr100_52 <- feature_representation(r100, sr100[, "solution_52"])
sr100_52$run <- "solution_52"
sr100_53 <- feature_representation(r100, sr100[, "solution_53"])
sr100_53$run <- "solution_53"
sr100_54 <- feature_representation(r100, sr100[, "solution_54"])
sr100_54$run <- "solution_54"
sr100_55 <- feature_representation(r100, sr100[, "solution_55"])
sr100_55$run <- "solution_55"
sr100_56 <- feature_representation(r100, sr100[, "solution_56"])
sr100_56$run <- "solution_56"
sr100_57 <- feature_representation(r100, sr100[, "solution_57"])
sr100_57$run <- "solution_57"
sr100_58 <- feature_representation(r100, sr100[, "solution_58"])
sr100_58$run <- "solution_58"
sr100_59 <- feature_representation(r100, sr100[, "solution_59"])
sr100_59$run <- "solution_59"
sr100_60 <- feature_representation(r100, sr100[, "solution_60"])
sr100_60$run <- "solution_60"
sr100_61 <- feature_representation(r100, sr100[, "solution_61"])
sr100_61$run <- "solution_61"
sr100_62 <- feature_representation(r100, sr100[, "solution_62"])
sr100_62$run <- "solution_62"
sr100_63 <- feature_representation(r100, sr100[, "solution_63"])
sr100_63$run <- "solution_63"
sr100_64 <- feature_representation(r100, sr100[, "solution_64"])
sr100_64$run <- "solution_64"
sr100_65 <- feature_representation(r100, sr100[, "solution_65"])
sr100_65$run <- "solution_65"
sr100_66 <- feature_representation(r100, sr100[, "solution_66"])
sr100_66$run <- "solution_66"
sr100_67 <- feature_representation(r100, sr100[, "solution_67"])
sr100_67$run <- "solution_67"
sr100_68 <- feature_representation(r100, sr100[, "solution_68"])
sr100_68$run <- "solution_68"
sr100_69 <- feature_representation(r100, sr100[, "solution_69"])
sr100_69$run <- "solution_69"
sr100_70 <- feature_representation(r100, sr100[, "solution_70"])
sr100_70$run <- "solution_70"
sr100_71 <- feature_representation(r100, sr100[, "solution_71"])
sr100_71$run <- "solution_71"
sr100_72 <- feature_representation(r100, sr100[, "solution_72"])
sr100_72$run <- "solution_72"
sr100_73 <- feature_representation(r100, sr100[, "solution_73"])
sr100_73$run <- "solution_73"
sr100_74 <- feature_representation(r100, sr100[, "solution_74"])
sr100_74$run <- "solution_74"
sr100_75 <- feature_representation(r100, sr100[, "solution_75"])
sr100_75$run <- "solution_75"
sr100_76 <- feature_representation(r100, sr100[, "solution_76"])
sr100_76$run <- "solution_76"
sr100_77 <- feature_representation(r100, sr100[, "solution_77"])
sr100_77$run <- "solution_77"
sr100_78 <- feature_representation(r100, sr100[, "solution_78"])
sr100_78$run <- "solution_78"
sr100_79 <- feature_representation(r100, sr100[, "solution_79"])
sr100_79$run <- "solution_79"
sr100_80 <- feature_representation(r100, sr100[, "solution_80"])
sr100_80$run <- "solution_80"
sr100_81 <- feature_representation(r100, sr100[, "solution_81"])
sr100_81$run <- "solution_81"
sr100_82 <- feature_representation(r100, sr100[, "solution_82"])
sr100_82$run <- "solution_82"
sr100_83 <- feature_representation(r100, sr100[, "solution_83"])
sr100_83$run <- "solution_83"
sr100_84 <- feature_representation(r100, sr100[, "solution_84"])
sr100_84$run <- "solution_84"
sr100_85 <- feature_representation(r100, sr100[, "solution_85"])
sr100_85$run <- "solution_85"
sr100_86 <- feature_representation(r100, sr100[, "solution_86"])
sr100_86$run <- "solution_86"
sr100_87 <- feature_representation(r100, sr100[, "solution_87"])
sr100_87$run <- "solution_87"
sr100_88 <- feature_representation(r100, sr100[, "solution_88"])
sr100_88$run <- "solution_88"
sr100_89 <- feature_representation(r100, sr100[, "solution_89"])
sr100_89$run <- "solution_89"
sr100_90 <- feature_representation(r100, sr100[, "solution_90"])
sr100_90$run <- "solution_90"
sr100_91 <- feature_representation(r100, sr100[, "solution_91"])
sr100_91$run <- "solution_91"
sr100_92 <- feature_representation(r100, sr100[, "solution_92"])
sr100_92$run <- "solution_92"
sr100_93 <- feature_representation(r100, sr100[, "solution_93"])
sr100_93$run <- "solution_93"
sr100_94 <- feature_representation(r100, sr100[, "solution_94"])
sr100_94$run <- "solution_94"
sr100_95 <- feature_representation(r100, sr100[, "solution_95"])
sr100_95$run <- "solution_95"
sr100_96 <- feature_representation(r100, sr100[, "solution_96"])
sr100_96$run <- "solution_96"
sr100_97 <- feature_representation(r100, sr100[, "solution_97"])
sr100_97$run <- "solution_97"
sr100_98 <- feature_representation(r100, sr100[, "solution_98"])
sr100_98$run <- "solution_98"
sr100_99 <- feature_representation(r100, sr100[, "solution_99"])
sr100_99$run <- "solution_99"
sr100_100 <- feature_representation(r100, sr100[, "solution_100"])
sr100_100$run <- "solution_100"
sr100_r<-bind_rows(sr100_1,sr100_2,sr100_3,sr100_4,sr100_5,sr100_6,sr100_7,sr100_8,sr100_9,sr100_10,sr100_11,sr100_12,sr100_13,sr100_14,sr100_15,sr100_16,sr100_17,sr100_18,sr100_19,sr100_20,sr100_21,sr100_22,sr100_23,sr100_24,sr100_25,sr100_26,sr100_27,sr100_28,sr100_29,sr100_30,sr100_31,sr100_32,sr100_33,sr100_34,sr100_35,sr100_36,sr100_37,sr100_38,sr100_39,sr100_40,sr100_41,sr100_42,sr100_43,sr100_44,sr100_45,sr100_46,sr100_47,sr100_48,sr100_49,sr100_50,sr100_51,sr100_52,sr100_53,sr100_54,sr100_55,sr100_56,sr100_57,sr100_58,sr100_59,sr100_60,sr100_61,sr100_62,sr100_63,sr100_64,sr100_65,sr100_66,sr100_67,sr100_68,sr100_69,sr100_70,sr100_71,sr100_72,sr100_73,sr100_74,sr100_75,sr100_76,sr100_77,sr100_78,sr100_79,sr100_80,sr100_81,sr100_82,sr100_83,sr100_84,sr100_85,sr100_86,sr100_87,sr100_88,sr100_89,sr100_90,sr100_91,sr100_92,sr100_93,sr100_94,sr100_95,sr100_96,sr100_97,sr100_98,sr100_99,sr100_100)
sr100_r$objective <- "Random"
sr100_r$species<-"100"


feature_a <- feature_abundances(r100, na.rm = FALSE)

sr100r<-left_join(sr100_r,feature_a)


#105
pu_dat105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r105 <- problem(pu_dat105, spec_dat105, cost_column = "cost", rij = puvsp_dat105) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat105$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr105 <- solve(r105)

sr105<-as.data.frame(sr105) %>% as_tibble()

sr105$objective<-"Random"
sr105$solution<-"105"

# Features Dataset
sr105_1 <- feature_representation(r105, sr105[, "solution_1"])
sr105_1$run <- "solution_1"
sr105_2 <- feature_representation(r105, sr105[, "solution_2"])
sr105_2$run <- "solution_2"
sr105_3 <- feature_representation(r105, sr105[, "solution_3"])
sr105_3$run <- "solution_3"
sr105_4 <- feature_representation(r105, sr105[, "solution_4"])
sr105_4$run <- "solution_4"
sr105_5 <- feature_representation(r105, sr105[, "solution_5"])
sr105_5$run <- "solution_5"
sr105_6 <- feature_representation(r105, sr105[, "solution_6"])
sr105_6$run <- "solution_6"
sr105_7 <- feature_representation(r105, sr105[, "solution_7"])
sr105_7$run <- "solution_7"
sr105_8 <- feature_representation(r105, sr105[, "solution_8"])
sr105_8$run <- "solution_8"
sr105_9 <- feature_representation(r105, sr105[, "solution_9"])
sr105_9$run <- "solution_9"
sr105_10 <- feature_representation(r105, sr105[, "solution_10"])
sr105_10$run <- "solution_10"
sr105_11 <- feature_representation(r105, sr105[, "solution_11"])
sr105_11$run <- "solution_11"
sr105_12 <- feature_representation(r105, sr105[, "solution_12"])
sr105_12$run <- "solution_12"
sr105_13 <- feature_representation(r105, sr105[, "solution_13"])
sr105_13$run <- "solution_13"
sr105_14 <- feature_representation(r105, sr105[, "solution_14"])
sr105_14$run <- "solution_14"
sr105_15 <- feature_representation(r105, sr105[, "solution_15"])
sr105_15$run <- "solution_15"
sr105_16 <- feature_representation(r105, sr105[, "solution_16"])
sr105_16$run <- "solution_16"
sr105_17 <- feature_representation(r105, sr105[, "solution_17"])
sr105_17$run <- "solution_17"
sr105_18 <- feature_representation(r105, sr105[, "solution_18"])
sr105_18$run <- "solution_18"
sr105_19 <- feature_representation(r105, sr105[, "solution_19"])
sr105_19$run <- "solution_19"
sr105_20 <- feature_representation(r105, sr105[, "solution_20"])
sr105_20$run <- "solution_20"
sr105_21 <- feature_representation(r105, sr105[, "solution_21"])
sr105_21$run <- "solution_21"
sr105_22 <- feature_representation(r105, sr105[, "solution_22"])
sr105_22$run <- "solution_22"
sr105_23 <- feature_representation(r105, sr105[, "solution_23"])
sr105_23$run <- "solution_23"
sr105_24 <- feature_representation(r105, sr105[, "solution_24"])
sr105_24$run <- "solution_24"
sr105_25 <- feature_representation(r105, sr105[, "solution_25"])
sr105_25$run <- "solution_25"
sr105_26 <- feature_representation(r105, sr105[, "solution_26"])
sr105_26$run <- "solution_26"
sr105_27 <- feature_representation(r105, sr105[, "solution_27"])
sr105_27$run <- "solution_27"
sr105_28 <- feature_representation(r105, sr105[, "solution_28"])
sr105_28$run <- "solution_28"
sr105_29 <- feature_representation(r105, sr105[, "solution_29"])
sr105_29$run <- "solution_29"
sr105_30 <- feature_representation(r105, sr105[, "solution_30"])
sr105_30$run <- "solution_30"
sr105_31 <- feature_representation(r105, sr105[, "solution_31"])
sr105_31$run <- "solution_31"
sr105_32 <- feature_representation(r105, sr105[, "solution_32"])
sr105_32$run <- "solution_32"
sr105_33 <- feature_representation(r105, sr105[, "solution_33"])
sr105_33$run <- "solution_33"
sr105_34 <- feature_representation(r105, sr105[, "solution_34"])
sr105_34$run <- "solution_34"
sr105_35 <- feature_representation(r105, sr105[, "solution_35"])
sr105_35$run <- "solution_35"
sr105_36 <- feature_representation(r105, sr105[, "solution_36"])
sr105_36$run <- "solution_36"
sr105_37 <- feature_representation(r105, sr105[, "solution_37"])
sr105_37$run <- "solution_37"
sr105_38 <- feature_representation(r105, sr105[, "solution_38"])
sr105_38$run <- "solution_38"
sr105_39 <- feature_representation(r105, sr105[, "solution_39"])
sr105_39$run <- "solution_39"
sr105_40 <- feature_representation(r105, sr105[, "solution_40"])
sr105_40$run <- "solution_40"
sr105_41 <- feature_representation(r105, sr105[, "solution_41"])
sr105_41$run <- "solution_41"
sr105_42 <- feature_representation(r105, sr105[, "solution_42"])
sr105_42$run <- "solution_42"
sr105_43 <- feature_representation(r105, sr105[, "solution_43"])
sr105_43$run <- "solution_43"
sr105_44 <- feature_representation(r105, sr105[, "solution_44"])
sr105_44$run <- "solution_44"
sr105_45 <- feature_representation(r105, sr105[, "solution_45"])
sr105_45$run <- "solution_45"
sr105_46 <- feature_representation(r105, sr105[, "solution_46"])
sr105_46$run <- "solution_46"
sr105_47 <- feature_representation(r105, sr105[, "solution_47"])
sr105_47$run <- "solution_47"
sr105_48 <- feature_representation(r105, sr105[, "solution_48"])
sr105_48$run <- "solution_48"
sr105_49 <- feature_representation(r105, sr105[, "solution_49"])
sr105_49$run <- "solution_49"
sr105_50 <- feature_representation(r105, sr105[, "solution_50"])
sr105_50$run <- "solution_50"
sr105_51 <- feature_representation(r105, sr105[, "solution_51"])
sr105_51$run <- "solution_51"
sr105_52 <- feature_representation(r105, sr105[, "solution_52"])
sr105_52$run <- "solution_52"
sr105_53 <- feature_representation(r105, sr105[, "solution_53"])
sr105_53$run <- "solution_53"
sr105_54 <- feature_representation(r105, sr105[, "solution_54"])
sr105_54$run <- "solution_54"
sr105_55 <- feature_representation(r105, sr105[, "solution_55"])
sr105_55$run <- "solution_55"
sr105_56 <- feature_representation(r105, sr105[, "solution_56"])
sr105_56$run <- "solution_56"
sr105_57 <- feature_representation(r105, sr105[, "solution_57"])
sr105_57$run <- "solution_57"
sr105_58 <- feature_representation(r105, sr105[, "solution_58"])
sr105_58$run <- "solution_58"
sr105_59 <- feature_representation(r105, sr105[, "solution_59"])
sr105_59$run <- "solution_59"
sr105_60 <- feature_representation(r105, sr105[, "solution_60"])
sr105_60$run <- "solution_60"
sr105_61 <- feature_representation(r105, sr105[, "solution_61"])
sr105_61$run <- "solution_61"
sr105_62 <- feature_representation(r105, sr105[, "solution_62"])
sr105_62$run <- "solution_62"
sr105_63 <- feature_representation(r105, sr105[, "solution_63"])
sr105_63$run <- "solution_63"
sr105_64 <- feature_representation(r105, sr105[, "solution_64"])
sr105_64$run <- "solution_64"
sr105_65 <- feature_representation(r105, sr105[, "solution_65"])
sr105_65$run <- "solution_65"
sr105_66 <- feature_representation(r105, sr105[, "solution_66"])
sr105_66$run <- "solution_66"
sr105_67 <- feature_representation(r105, sr105[, "solution_67"])
sr105_67$run <- "solution_67"
sr105_68 <- feature_representation(r105, sr105[, "solution_68"])
sr105_68$run <- "solution_68"
sr105_69 <- feature_representation(r105, sr105[, "solution_69"])
sr105_69$run <- "solution_69"
sr105_70 <- feature_representation(r105, sr105[, "solution_70"])
sr105_70$run <- "solution_70"
sr105_71 <- feature_representation(r105, sr105[, "solution_71"])
sr105_71$run <- "solution_71"
sr105_72 <- feature_representation(r105, sr105[, "solution_72"])
sr105_72$run <- "solution_72"
sr105_73 <- feature_representation(r105, sr105[, "solution_73"])
sr105_73$run <- "solution_73"
sr105_74 <- feature_representation(r105, sr105[, "solution_74"])
sr105_74$run <- "solution_74"
sr105_75 <- feature_representation(r105, sr105[, "solution_75"])
sr105_75$run <- "solution_75"
sr105_76 <- feature_representation(r105, sr105[, "solution_76"])
sr105_76$run <- "solution_76"
sr105_77 <- feature_representation(r105, sr105[, "solution_77"])
sr105_77$run <- "solution_77"
sr105_78 <- feature_representation(r105, sr105[, "solution_78"])
sr105_78$run <- "solution_78"
sr105_79 <- feature_representation(r105, sr105[, "solution_79"])
sr105_79$run <- "solution_79"
sr105_80 <- feature_representation(r105, sr105[, "solution_80"])
sr105_80$run <- "solution_80"
sr105_81 <- feature_representation(r105, sr105[, "solution_81"])
sr105_81$run <- "solution_81"
sr105_82 <- feature_representation(r105, sr105[, "solution_82"])
sr105_82$run <- "solution_82"
sr105_83 <- feature_representation(r105, sr105[, "solution_83"])
sr105_83$run <- "solution_83"
sr105_84 <- feature_representation(r105, sr105[, "solution_84"])
sr105_84$run <- "solution_84"
sr105_85 <- feature_representation(r105, sr105[, "solution_85"])
sr105_85$run <- "solution_85"
sr105_86 <- feature_representation(r105, sr105[, "solution_86"])
sr105_86$run <- "solution_86"
sr105_87 <- feature_representation(r105, sr105[, "solution_87"])
sr105_87$run <- "solution_87"
sr105_88 <- feature_representation(r105, sr105[, "solution_88"])
sr105_88$run <- "solution_88"
sr105_89 <- feature_representation(r105, sr105[, "solution_89"])
sr105_89$run <- "solution_89"
sr105_90 <- feature_representation(r105, sr105[, "solution_90"])
sr105_90$run <- "solution_90"
sr105_91 <- feature_representation(r105, sr105[, "solution_91"])
sr105_91$run <- "solution_91"
sr105_92 <- feature_representation(r105, sr105[, "solution_92"])
sr105_92$run <- "solution_92"
sr105_93 <- feature_representation(r105, sr105[, "solution_93"])
sr105_93$run <- "solution_93"
sr105_94 <- feature_representation(r105, sr105[, "solution_94"])
sr105_94$run <- "solution_94"
sr105_95 <- feature_representation(r105, sr105[, "solution_95"])
sr105_95$run <- "solution_95"
sr105_96 <- feature_representation(r105, sr105[, "solution_96"])
sr105_96$run <- "solution_96"
sr105_97 <- feature_representation(r105, sr105[, "solution_97"])
sr105_97$run <- "solution_97"
sr105_98 <- feature_representation(r105, sr105[, "solution_98"])
sr105_98$run <- "solution_98"
sr105_99 <- feature_representation(r105, sr105[, "solution_99"])
sr105_99$run <- "solution_99"
sr105_100 <- feature_representation(r105, sr105[, "solution_100"])
sr105_100$run <- "solution_100"
sr105_r<-bind_rows(sr105_1,sr105_2,sr105_3,sr105_4,sr105_5,sr105_6,sr105_7,sr105_8,sr105_9,sr105_10,sr105_11,sr105_12,sr105_13,sr105_14,sr105_15,sr105_16,sr105_17,sr105_18,sr105_19,sr105_20,sr105_21,sr105_22,sr105_23,sr105_24,sr105_25,sr105_26,sr105_27,sr105_28,sr105_29,sr105_30,sr105_31,sr105_32,sr105_33,sr105_34,sr105_35,sr105_36,sr105_37,sr105_38,sr105_39,sr105_40,sr105_41,sr105_42,sr105_43,sr105_44,sr105_45,sr105_46,sr105_47,sr105_48,sr105_49,sr105_50,sr105_51,sr105_52,sr105_53,sr105_54,sr105_55,sr105_56,sr105_57,sr105_58,sr105_59,sr105_60,sr105_61,sr105_62,sr105_63,sr105_64,sr105_65,sr105_66,sr105_67,sr105_68,sr105_69,sr105_70,sr105_71,sr105_72,sr105_73,sr105_74,sr105_75,sr105_76,sr105_77,sr105_78,sr105_79,sr105_80,sr105_81,sr105_82,sr105_83,sr105_84,sr105_85,sr105_86,sr105_87,sr105_88,sr105_89,sr105_90,sr105_91,sr105_92,sr105_93,sr105_94,sr105_95,sr105_96,sr105_97,sr105_98,sr105_99,sr105_100)
sr105_r$objective <- "Random"
sr105_r$species<-"105"


feature_a <- feature_abundances(r105, na.rm = FALSE)

sr105r<-left_join(sr105_r,feature_a)


#110
pu_dat110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r110 <- problem(pu_dat110, spec_dat110, cost_column = "cost", rij = puvsp_dat110) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat110$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr110 <- solve(r110)

sr110<-as.data.frame(sr110) %>% as_tibble()

sr110$objective<-"Random"
sr110$solution<-"110"


# Features Dataset
sr110_1 <- feature_representation(r110, sr110[, "solution_1"])
sr110_1$run <- "solution_1"
sr110_2 <- feature_representation(r110, sr110[, "solution_2"])
sr110_2$run <- "solution_2"
sr110_3 <- feature_representation(r110, sr110[, "solution_3"])
sr110_3$run <- "solution_3"
sr110_4 <- feature_representation(r110, sr110[, "solution_4"])
sr110_4$run <- "solution_4"
sr110_5 <- feature_representation(r110, sr110[, "solution_5"])
sr110_5$run <- "solution_5"
sr110_6 <- feature_representation(r110, sr110[, "solution_6"])
sr110_6$run <- "solution_6"
sr110_7 <- feature_representation(r110, sr110[, "solution_7"])
sr110_7$run <- "solution_7"
sr110_8 <- feature_representation(r110, sr110[, "solution_8"])
sr110_8$run <- "solution_8"
sr110_9 <- feature_representation(r110, sr110[, "solution_9"])
sr110_9$run <- "solution_9"
sr110_10 <- feature_representation(r110, sr110[, "solution_10"])
sr110_10$run <- "solution_10"
sr110_11 <- feature_representation(r110, sr110[, "solution_11"])
sr110_11$run <- "solution_11"
sr110_12 <- feature_representation(r110, sr110[, "solution_12"])
sr110_12$run <- "solution_12"
sr110_13 <- feature_representation(r110, sr110[, "solution_13"])
sr110_13$run <- "solution_13"
sr110_14 <- feature_representation(r110, sr110[, "solution_14"])
sr110_14$run <- "solution_14"
sr110_15 <- feature_representation(r110, sr110[, "solution_15"])
sr110_15$run <- "solution_15"
sr110_16 <- feature_representation(r110, sr110[, "solution_16"])
sr110_16$run <- "solution_16"
sr110_17 <- feature_representation(r110, sr110[, "solution_17"])
sr110_17$run <- "solution_17"
sr110_18 <- feature_representation(r110, sr110[, "solution_18"])
sr110_18$run <- "solution_18"
sr110_19 <- feature_representation(r110, sr110[, "solution_19"])
sr110_19$run <- "solution_19"
sr110_20 <- feature_representation(r110, sr110[, "solution_20"])
sr110_20$run <- "solution_20"
sr110_21 <- feature_representation(r110, sr110[, "solution_21"])
sr110_21$run <- "solution_21"
sr110_22 <- feature_representation(r110, sr110[, "solution_22"])
sr110_22$run <- "solution_22"
sr110_23 <- feature_representation(r110, sr110[, "solution_23"])
sr110_23$run <- "solution_23"
sr110_24 <- feature_representation(r110, sr110[, "solution_24"])
sr110_24$run <- "solution_24"
sr110_25 <- feature_representation(r110, sr110[, "solution_25"])
sr110_25$run <- "solution_25"
sr110_26 <- feature_representation(r110, sr110[, "solution_26"])
sr110_26$run <- "solution_26"
sr110_27 <- feature_representation(r110, sr110[, "solution_27"])
sr110_27$run <- "solution_27"
sr110_28 <- feature_representation(r110, sr110[, "solution_28"])
sr110_28$run <- "solution_28"
sr110_29 <- feature_representation(r110, sr110[, "solution_29"])
sr110_29$run <- "solution_29"
sr110_30 <- feature_representation(r110, sr110[, "solution_30"])
sr110_30$run <- "solution_30"
sr110_31 <- feature_representation(r110, sr110[, "solution_31"])
sr110_31$run <- "solution_31"
sr110_32 <- feature_representation(r110, sr110[, "solution_32"])
sr110_32$run <- "solution_32"
sr110_33 <- feature_representation(r110, sr110[, "solution_33"])
sr110_33$run <- "solution_33"
sr110_34 <- feature_representation(r110, sr110[, "solution_34"])
sr110_34$run <- "solution_34"
sr110_35 <- feature_representation(r110, sr110[, "solution_35"])
sr110_35$run <- "solution_35"
sr110_36 <- feature_representation(r110, sr110[, "solution_36"])
sr110_36$run <- "solution_36"
sr110_37 <- feature_representation(r110, sr110[, "solution_37"])
sr110_37$run <- "solution_37"
sr110_38 <- feature_representation(r110, sr110[, "solution_38"])
sr110_38$run <- "solution_38"
sr110_39 <- feature_representation(r110, sr110[, "solution_39"])
sr110_39$run <- "solution_39"
sr110_40 <- feature_representation(r110, sr110[, "solution_40"])
sr110_40$run <- "solution_40"
sr110_41 <- feature_representation(r110, sr110[, "solution_41"])
sr110_41$run <- "solution_41"
sr110_42 <- feature_representation(r110, sr110[, "solution_42"])
sr110_42$run <- "solution_42"
sr110_43 <- feature_representation(r110, sr110[, "solution_43"])
sr110_43$run <- "solution_43"
sr110_44 <- feature_representation(r110, sr110[, "solution_44"])
sr110_44$run <- "solution_44"
sr110_45 <- feature_representation(r110, sr110[, "solution_45"])
sr110_45$run <- "solution_45"
sr110_46 <- feature_representation(r110, sr110[, "solution_46"])
sr110_46$run <- "solution_46"
sr110_47 <- feature_representation(r110, sr110[, "solution_47"])
sr110_47$run <- "solution_47"
sr110_48 <- feature_representation(r110, sr110[, "solution_48"])
sr110_48$run <- "solution_48"
sr110_49 <- feature_representation(r110, sr110[, "solution_49"])
sr110_49$run <- "solution_49"
sr110_50 <- feature_representation(r110, sr110[, "solution_50"])
sr110_50$run <- "solution_50"
sr110_51 <- feature_representation(r110, sr110[, "solution_51"])
sr110_51$run <- "solution_51"
sr110_52 <- feature_representation(r110, sr110[, "solution_52"])
sr110_52$run <- "solution_52"
sr110_53 <- feature_representation(r110, sr110[, "solution_53"])
sr110_53$run <- "solution_53"
sr110_54 <- feature_representation(r110, sr110[, "solution_54"])
sr110_54$run <- "solution_54"
sr110_55 <- feature_representation(r110, sr110[, "solution_55"])
sr110_55$run <- "solution_55"
sr110_56 <- feature_representation(r110, sr110[, "solution_56"])
sr110_56$run <- "solution_56"
sr110_57 <- feature_representation(r110, sr110[, "solution_57"])
sr110_57$run <- "solution_57"
sr110_58 <- feature_representation(r110, sr110[, "solution_58"])
sr110_58$run <- "solution_58"
sr110_59 <- feature_representation(r110, sr110[, "solution_59"])
sr110_59$run <- "solution_59"
sr110_60 <- feature_representation(r110, sr110[, "solution_60"])
sr110_60$run <- "solution_60"
sr110_61 <- feature_representation(r110, sr110[, "solution_61"])
sr110_61$run <- "solution_61"
sr110_62 <- feature_representation(r110, sr110[, "solution_62"])
sr110_62$run <- "solution_62"
sr110_63 <- feature_representation(r110, sr110[, "solution_63"])
sr110_63$run <- "solution_63"
sr110_64 <- feature_representation(r110, sr110[, "solution_64"])
sr110_64$run <- "solution_64"
sr110_65 <- feature_representation(r110, sr110[, "solution_65"])
sr110_65$run <- "solution_65"
sr110_66 <- feature_representation(r110, sr110[, "solution_66"])
sr110_66$run <- "solution_66"
sr110_67 <- feature_representation(r110, sr110[, "solution_67"])
sr110_67$run <- "solution_67"
sr110_68 <- feature_representation(r110, sr110[, "solution_68"])
sr110_68$run <- "solution_68"
sr110_69 <- feature_representation(r110, sr110[, "solution_69"])
sr110_69$run <- "solution_69"
sr110_70 <- feature_representation(r110, sr110[, "solution_70"])
sr110_70$run <- "solution_70"
sr110_71 <- feature_representation(r110, sr110[, "solution_71"])
sr110_71$run <- "solution_71"
sr110_72 <- feature_representation(r110, sr110[, "solution_72"])
sr110_72$run <- "solution_72"
sr110_73 <- feature_representation(r110, sr110[, "solution_73"])
sr110_73$run <- "solution_73"
sr110_74 <- feature_representation(r110, sr110[, "solution_74"])
sr110_74$run <- "solution_74"
sr110_75 <- feature_representation(r110, sr110[, "solution_75"])
sr110_75$run <- "solution_75"
sr110_76 <- feature_representation(r110, sr110[, "solution_76"])
sr110_76$run <- "solution_76"
sr110_77 <- feature_representation(r110, sr110[, "solution_77"])
sr110_77$run <- "solution_77"
sr110_78 <- feature_representation(r110, sr110[, "solution_78"])
sr110_78$run <- "solution_78"
sr110_79 <- feature_representation(r110, sr110[, "solution_79"])
sr110_79$run <- "solution_79"
sr110_80 <- feature_representation(r110, sr110[, "solution_80"])
sr110_80$run <- "solution_80"
sr110_81 <- feature_representation(r110, sr110[, "solution_81"])
sr110_81$run <- "solution_81"
sr110_82 <- feature_representation(r110, sr110[, "solution_82"])
sr110_82$run <- "solution_82"
sr110_83 <- feature_representation(r110, sr110[, "solution_83"])
sr110_83$run <- "solution_83"
sr110_84 <- feature_representation(r110, sr110[, "solution_84"])
sr110_84$run <- "solution_84"
sr110_85 <- feature_representation(r110, sr110[, "solution_85"])
sr110_85$run <- "solution_85"
sr110_86 <- feature_representation(r110, sr110[, "solution_86"])
sr110_86$run <- "solution_86"
sr110_87 <- feature_representation(r110, sr110[, "solution_87"])
sr110_87$run <- "solution_87"
sr110_88 <- feature_representation(r110, sr110[, "solution_88"])
sr110_88$run <- "solution_88"
sr110_89 <- feature_representation(r110, sr110[, "solution_89"])
sr110_89$run <- "solution_89"
sr110_90 <- feature_representation(r110, sr110[, "solution_90"])
sr110_90$run <- "solution_90"
sr110_91 <- feature_representation(r110, sr110[, "solution_91"])
sr110_91$run <- "solution_91"
sr110_92 <- feature_representation(r110, sr110[, "solution_92"])
sr110_92$run <- "solution_92"
sr110_93 <- feature_representation(r110, sr110[, "solution_93"])
sr110_93$run <- "solution_93"
sr110_94 <- feature_representation(r110, sr110[, "solution_94"])
sr110_94$run <- "solution_94"
sr110_95 <- feature_representation(r110, sr110[, "solution_95"])
sr110_95$run <- "solution_95"
sr110_96 <- feature_representation(r110, sr110[, "solution_96"])
sr110_96$run <- "solution_96"
sr110_97 <- feature_representation(r110, sr110[, "solution_97"])
sr110_97$run <- "solution_97"
sr110_98 <- feature_representation(r110, sr110[, "solution_98"])
sr110_98$run <- "solution_98"
sr110_99 <- feature_representation(r110, sr110[, "solution_99"])
sr110_99$run <- "solution_99"
sr110_100 <- feature_representation(r110, sr110[, "solution_100"])
sr110_100$run <- "solution_100"
sr110_r<-bind_rows(sr110_1,sr110_2,sr110_3,sr110_4,sr110_5,sr110_6,sr110_7,sr110_8,sr110_9,sr110_10,sr110_11,sr110_12,sr110_13,sr110_14,sr110_15,sr110_16,sr110_17,sr110_18,sr110_19,sr110_20,sr110_21,sr110_22,sr110_23,sr110_24,sr110_25,sr110_26,sr110_27,sr110_28,sr110_29,sr110_30,sr110_31,sr110_32,sr110_33,sr110_34,sr110_35,sr110_36,sr110_37,sr110_38,sr110_39,sr110_40,sr110_41,sr110_42,sr110_43,sr110_44,sr110_45,sr110_46,sr110_47,sr110_48,sr110_49,sr110_50,sr110_51,sr110_52,sr110_53,sr110_54,sr110_55,sr110_56,sr110_57,sr110_58,sr110_59,sr110_60,sr110_61,sr110_62,sr110_63,sr110_64,sr110_65,sr110_66,sr110_67,sr110_68,sr110_69,sr110_70,sr110_71,sr110_72,sr110_73,sr110_74,sr110_75,sr110_76,sr110_77,sr110_78,sr110_79,sr110_80,sr110_81,sr110_82,sr110_83,sr110_84,sr110_85,sr110_86,sr110_87,sr110_88,sr110_89,sr110_90,sr110_91,sr110_92,sr110_93,sr110_94,sr110_95,sr110_96,sr110_97,sr110_98,sr110_99,sr110_100)
sr110_r$objective <- "Random"
sr110_r$species<-"110"


feature_a <- feature_abundances(r110, na.rm = FALSE)

sr110r<-left_join(sr110_r,feature_a)

#115
pu_dat115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r115 <- problem(pu_dat115, spec_dat115, cost_column = "cost", rij = puvsp_dat115) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat115$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr115 <- solve(r115)

sr115<-as.data.frame(sr115) %>% as_tibble()

sr115$objective<-"Random"
sr115$solution<-"115"

# Features Dataset
sr115_1 <- feature_representation(r115, sr115[, "solution_1"])
sr115_1$run <- "solution_1"
sr115_2 <- feature_representation(r115, sr115[, "solution_2"])
sr115_2$run <- "solution_2"
sr115_3 <- feature_representation(r115, sr115[, "solution_3"])
sr115_3$run <- "solution_3"
sr115_4 <- feature_representation(r115, sr115[, "solution_4"])
sr115_4$run <- "solution_4"
sr115_5 <- feature_representation(r115, sr115[, "solution_5"])
sr115_5$run <- "solution_5"
sr115_6 <- feature_representation(r115, sr115[, "solution_6"])
sr115_6$run <- "solution_6"
sr115_7 <- feature_representation(r115, sr115[, "solution_7"])
sr115_7$run <- "solution_7"
sr115_8 <- feature_representation(r115, sr115[, "solution_8"])
sr115_8$run <- "solution_8"
sr115_9 <- feature_representation(r115, sr115[, "solution_9"])
sr115_9$run <- "solution_9"
sr115_10 <- feature_representation(r115, sr115[, "solution_10"])
sr115_10$run <- "solution_10"
sr115_11 <- feature_representation(r115, sr115[, "solution_11"])
sr115_11$run <- "solution_11"
sr115_12 <- feature_representation(r115, sr115[, "solution_12"])
sr115_12$run <- "solution_12"
sr115_13 <- feature_representation(r115, sr115[, "solution_13"])
sr115_13$run <- "solution_13"
sr115_14 <- feature_representation(r115, sr115[, "solution_14"])
sr115_14$run <- "solution_14"
sr115_15 <- feature_representation(r115, sr115[, "solution_15"])
sr115_15$run <- "solution_15"
sr115_16 <- feature_representation(r115, sr115[, "solution_16"])
sr115_16$run <- "solution_16"
sr115_17 <- feature_representation(r115, sr115[, "solution_17"])
sr115_17$run <- "solution_17"
sr115_18 <- feature_representation(r115, sr115[, "solution_18"])
sr115_18$run <- "solution_18"
sr115_19 <- feature_representation(r115, sr115[, "solution_19"])
sr115_19$run <- "solution_19"
sr115_20 <- feature_representation(r115, sr115[, "solution_20"])
sr115_20$run <- "solution_20"
sr115_21 <- feature_representation(r115, sr115[, "solution_21"])
sr115_21$run <- "solution_21"
sr115_22 <- feature_representation(r115, sr115[, "solution_22"])
sr115_22$run <- "solution_22"
sr115_23 <- feature_representation(r115, sr115[, "solution_23"])
sr115_23$run <- "solution_23"
sr115_24 <- feature_representation(r115, sr115[, "solution_24"])
sr115_24$run <- "solution_24"
sr115_25 <- feature_representation(r115, sr115[, "solution_25"])
sr115_25$run <- "solution_25"
sr115_26 <- feature_representation(r115, sr115[, "solution_26"])
sr115_26$run <- "solution_26"
sr115_27 <- feature_representation(r115, sr115[, "solution_27"])
sr115_27$run <- "solution_27"
sr115_28 <- feature_representation(r115, sr115[, "solution_28"])
sr115_28$run <- "solution_28"
sr115_29 <- feature_representation(r115, sr115[, "solution_29"])
sr115_29$run <- "solution_29"
sr115_30 <- feature_representation(r115, sr115[, "solution_30"])
sr115_30$run <- "solution_30"
sr115_31 <- feature_representation(r115, sr115[, "solution_31"])
sr115_31$run <- "solution_31"
sr115_32 <- feature_representation(r115, sr115[, "solution_32"])
sr115_32$run <- "solution_32"
sr115_33 <- feature_representation(r115, sr115[, "solution_33"])
sr115_33$run <- "solution_33"
sr115_34 <- feature_representation(r115, sr115[, "solution_34"])
sr115_34$run <- "solution_34"
sr115_35 <- feature_representation(r115, sr115[, "solution_35"])
sr115_35$run <- "solution_35"
sr115_36 <- feature_representation(r115, sr115[, "solution_36"])
sr115_36$run <- "solution_36"
sr115_37 <- feature_representation(r115, sr115[, "solution_37"])
sr115_37$run <- "solution_37"
sr115_38 <- feature_representation(r115, sr115[, "solution_38"])
sr115_38$run <- "solution_38"
sr115_39 <- feature_representation(r115, sr115[, "solution_39"])
sr115_39$run <- "solution_39"
sr115_40 <- feature_representation(r115, sr115[, "solution_40"])
sr115_40$run <- "solution_40"
sr115_41 <- feature_representation(r115, sr115[, "solution_41"])
sr115_41$run <- "solution_41"
sr115_42 <- feature_representation(r115, sr115[, "solution_42"])
sr115_42$run <- "solution_42"
sr115_43 <- feature_representation(r115, sr115[, "solution_43"])
sr115_43$run <- "solution_43"
sr115_44 <- feature_representation(r115, sr115[, "solution_44"])
sr115_44$run <- "solution_44"
sr115_45 <- feature_representation(r115, sr115[, "solution_45"])
sr115_45$run <- "solution_45"
sr115_46 <- feature_representation(r115, sr115[, "solution_46"])
sr115_46$run <- "solution_46"
sr115_47 <- feature_representation(r115, sr115[, "solution_47"])
sr115_47$run <- "solution_47"
sr115_48 <- feature_representation(r115, sr115[, "solution_48"])
sr115_48$run <- "solution_48"
sr115_49 <- feature_representation(r115, sr115[, "solution_49"])
sr115_49$run <- "solution_49"
sr115_50 <- feature_representation(r115, sr115[, "solution_50"])
sr115_50$run <- "solution_50"
sr115_51 <- feature_representation(r115, sr115[, "solution_51"])
sr115_51$run <- "solution_51"
sr115_52 <- feature_representation(r115, sr115[, "solution_52"])
sr115_52$run <- "solution_52"
sr115_53 <- feature_representation(r115, sr115[, "solution_53"])
sr115_53$run <- "solution_53"
sr115_54 <- feature_representation(r115, sr115[, "solution_54"])
sr115_54$run <- "solution_54"
sr115_55 <- feature_representation(r115, sr115[, "solution_55"])
sr115_55$run <- "solution_55"
sr115_56 <- feature_representation(r115, sr115[, "solution_56"])
sr115_56$run <- "solution_56"
sr115_57 <- feature_representation(r115, sr115[, "solution_57"])
sr115_57$run <- "solution_57"
sr115_58 <- feature_representation(r115, sr115[, "solution_58"])
sr115_58$run <- "solution_58"
sr115_59 <- feature_representation(r115, sr115[, "solution_59"])
sr115_59$run <- "solution_59"
sr115_60 <- feature_representation(r115, sr115[, "solution_60"])
sr115_60$run <- "solution_60"
sr115_61 <- feature_representation(r115, sr115[, "solution_61"])
sr115_61$run <- "solution_61"
sr115_62 <- feature_representation(r115, sr115[, "solution_62"])
sr115_62$run <- "solution_62"
sr115_63 <- feature_representation(r115, sr115[, "solution_63"])
sr115_63$run <- "solution_63"
sr115_64 <- feature_representation(r115, sr115[, "solution_64"])
sr115_64$run <- "solution_64"
sr115_65 <- feature_representation(r115, sr115[, "solution_65"])
sr115_65$run <- "solution_65"
sr115_66 <- feature_representation(r115, sr115[, "solution_66"])
sr115_66$run <- "solution_66"
sr115_67 <- feature_representation(r115, sr115[, "solution_67"])
sr115_67$run <- "solution_67"
sr115_68 <- feature_representation(r115, sr115[, "solution_68"])
sr115_68$run <- "solution_68"
sr115_69 <- feature_representation(r115, sr115[, "solution_69"])
sr115_69$run <- "solution_69"
sr115_70 <- feature_representation(r115, sr115[, "solution_70"])
sr115_70$run <- "solution_70"
sr115_71 <- feature_representation(r115, sr115[, "solution_71"])
sr115_71$run <- "solution_71"
sr115_72 <- feature_representation(r115, sr115[, "solution_72"])
sr115_72$run <- "solution_72"
sr115_73 <- feature_representation(r115, sr115[, "solution_73"])
sr115_73$run <- "solution_73"
sr115_74 <- feature_representation(r115, sr115[, "solution_74"])
sr115_74$run <- "solution_74"
sr115_75 <- feature_representation(r115, sr115[, "solution_75"])
sr115_75$run <- "solution_75"
sr115_76 <- feature_representation(r115, sr115[, "solution_76"])
sr115_76$run <- "solution_76"
sr115_77 <- feature_representation(r115, sr115[, "solution_77"])
sr115_77$run <- "solution_77"
sr115_78 <- feature_representation(r115, sr115[, "solution_78"])
sr115_78$run <- "solution_78"
sr115_79 <- feature_representation(r115, sr115[, "solution_79"])
sr115_79$run <- "solution_79"
sr115_80 <- feature_representation(r115, sr115[, "solution_80"])
sr115_80$run <- "solution_80"
sr115_81 <- feature_representation(r115, sr115[, "solution_81"])
sr115_81$run <- "solution_81"
sr115_82 <- feature_representation(r115, sr115[, "solution_82"])
sr115_82$run <- "solution_82"
sr115_83 <- feature_representation(r115, sr115[, "solution_83"])
sr115_83$run <- "solution_83"
sr115_84 <- feature_representation(r115, sr115[, "solution_84"])
sr115_84$run <- "solution_84"
sr115_85 <- feature_representation(r115, sr115[, "solution_85"])
sr115_85$run <- "solution_85"
sr115_86 <- feature_representation(r115, sr115[, "solution_86"])
sr115_86$run <- "solution_86"
sr115_87 <- feature_representation(r115, sr115[, "solution_87"])
sr115_87$run <- "solution_87"
sr115_88 <- feature_representation(r115, sr115[, "solution_88"])
sr115_88$run <- "solution_88"
sr115_89 <- feature_representation(r115, sr115[, "solution_89"])
sr115_89$run <- "solution_89"
sr115_90 <- feature_representation(r115, sr115[, "solution_90"])
sr115_90$run <- "solution_90"
sr115_91 <- feature_representation(r115, sr115[, "solution_91"])
sr115_91$run <- "solution_91"
sr115_92 <- feature_representation(r115, sr115[, "solution_92"])
sr115_92$run <- "solution_92"
sr115_93 <- feature_representation(r115, sr115[, "solution_93"])
sr115_93$run <- "solution_93"
sr115_94 <- feature_representation(r115, sr115[, "solution_94"])
sr115_94$run <- "solution_94"
sr115_95 <- feature_representation(r115, sr115[, "solution_95"])
sr115_95$run <- "solution_95"
sr115_96 <- feature_representation(r115, sr115[, "solution_96"])
sr115_96$run <- "solution_96"
sr115_97 <- feature_representation(r115, sr115[, "solution_97"])
sr115_97$run <- "solution_97"
sr115_98 <- feature_representation(r115, sr115[, "solution_98"])
sr115_98$run <- "solution_98"
sr115_99 <- feature_representation(r115, sr115[, "solution_99"])
sr115_99$run <- "solution_99"
sr115_100 <- feature_representation(r115, sr115[, "solution_100"])
sr115_100$run <- "solution_100"
sr115_r<-bind_rows(sr115_1,sr115_2,sr115_3,sr115_4,sr115_5,sr115_6,sr115_7,sr115_8,sr115_9,sr115_10,sr115_11,sr115_12,sr115_13,sr115_14,sr115_15,sr115_16,sr115_17,sr115_18,sr115_19,sr115_20,sr115_21,sr115_22,sr115_23,sr115_24,sr115_25,sr115_26,sr115_27,sr115_28,sr115_29,sr115_30,sr115_31,sr115_32,sr115_33,sr115_34,sr115_35,sr115_36,sr115_37,sr115_38,sr115_39,sr115_40,sr115_41,sr115_42,sr115_43,sr115_44,sr115_45,sr115_46,sr115_47,sr115_48,sr115_49,sr115_50,sr115_51,sr115_52,sr115_53,sr115_54,sr115_55,sr115_56,sr115_57,sr115_58,sr115_59,sr115_60,sr115_61,sr115_62,sr115_63,sr115_64,sr115_65,sr115_66,sr115_67,sr115_68,sr115_69,sr115_70,sr115_71,sr115_72,sr115_73,sr115_74,sr115_75,sr115_76,sr115_77,sr115_78,sr115_79,sr115_80,sr115_81,sr115_82,sr115_83,sr115_84,sr115_85,sr115_86,sr115_87,sr115_88,sr115_89,sr115_90,sr115_91,sr115_92,sr115_93,sr115_94,sr115_95,sr115_96,sr115_97,sr115_98,sr115_99,sr115_100)
sr115_r$objective <- "Random"
sr115_r$species<-"115"


feature_a <- feature_abundances(r115, na.rm = FALSE)

sr115r<-left_join(sr115_r,feature_a)


#120
pu_dat120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r120 <- problem(pu_dat120, spec_dat120, cost_column = "cost", rij = puvsp_dat120) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat120$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr120 <- solve(r120)

sr120<-as.data.frame(sr120) %>% as_tibble()

sr120$objective<-"Random"
sr120$solution<-"120"

# Features Dataset
sr120_1 <- feature_representation(r120, sr120[, "solution_1"])
sr120_1$run <- "solution_1"
sr120_2 <- feature_representation(r120, sr120[, "solution_2"])
sr120_2$run <- "solution_2"
sr120_3 <- feature_representation(r120, sr120[, "solution_3"])
sr120_3$run <- "solution_3"
sr120_4 <- feature_representation(r120, sr120[, "solution_4"])
sr120_4$run <- "solution_4"
sr120_5 <- feature_representation(r120, sr120[, "solution_5"])
sr120_5$run <- "solution_5"
sr120_6 <- feature_representation(r120, sr120[, "solution_6"])
sr120_6$run <- "solution_6"
sr120_7 <- feature_representation(r120, sr120[, "solution_7"])
sr120_7$run <- "solution_7"
sr120_8 <- feature_representation(r120, sr120[, "solution_8"])
sr120_8$run <- "solution_8"
sr120_9 <- feature_representation(r120, sr120[, "solution_9"])
sr120_9$run <- "solution_9"
sr120_10 <- feature_representation(r120, sr120[, "solution_10"])
sr120_10$run <- "solution_10"
sr120_11 <- feature_representation(r120, sr120[, "solution_11"])
sr120_11$run <- "solution_11"
sr120_12 <- feature_representation(r120, sr120[, "solution_12"])
sr120_12$run <- "solution_12"
sr120_13 <- feature_representation(r120, sr120[, "solution_13"])
sr120_13$run <- "solution_13"
sr120_14 <- feature_representation(r120, sr120[, "solution_14"])
sr120_14$run <- "solution_14"
sr120_15 <- feature_representation(r120, sr120[, "solution_15"])
sr120_15$run <- "solution_15"
sr120_16 <- feature_representation(r120, sr120[, "solution_16"])
sr120_16$run <- "solution_16"
sr120_17 <- feature_representation(r120, sr120[, "solution_17"])
sr120_17$run <- "solution_17"
sr120_18 <- feature_representation(r120, sr120[, "solution_18"])
sr120_18$run <- "solution_18"
sr120_19 <- feature_representation(r120, sr120[, "solution_19"])
sr120_19$run <- "solution_19"
sr120_20 <- feature_representation(r120, sr120[, "solution_20"])
sr120_20$run <- "solution_20"
sr120_21 <- feature_representation(r120, sr120[, "solution_21"])
sr120_21$run <- "solution_21"
sr120_22 <- feature_representation(r120, sr120[, "solution_22"])
sr120_22$run <- "solution_22"
sr120_23 <- feature_representation(r120, sr120[, "solution_23"])
sr120_23$run <- "solution_23"
sr120_24 <- feature_representation(r120, sr120[, "solution_24"])
sr120_24$run <- "solution_24"
sr120_25 <- feature_representation(r120, sr120[, "solution_25"])
sr120_25$run <- "solution_25"
sr120_26 <- feature_representation(r120, sr120[, "solution_26"])
sr120_26$run <- "solution_26"
sr120_27 <- feature_representation(r120, sr120[, "solution_27"])
sr120_27$run <- "solution_27"
sr120_28 <- feature_representation(r120, sr120[, "solution_28"])
sr120_28$run <- "solution_28"
sr120_29 <- feature_representation(r120, sr120[, "solution_29"])
sr120_29$run <- "solution_29"
sr120_30 <- feature_representation(r120, sr120[, "solution_30"])
sr120_30$run <- "solution_30"
sr120_31 <- feature_representation(r120, sr120[, "solution_31"])
sr120_31$run <- "solution_31"
sr120_32 <- feature_representation(r120, sr120[, "solution_32"])
sr120_32$run <- "solution_32"
sr120_33 <- feature_representation(r120, sr120[, "solution_33"])
sr120_33$run <- "solution_33"
sr120_34 <- feature_representation(r120, sr120[, "solution_34"])
sr120_34$run <- "solution_34"
sr120_35 <- feature_representation(r120, sr120[, "solution_35"])
sr120_35$run <- "solution_35"
sr120_36 <- feature_representation(r120, sr120[, "solution_36"])
sr120_36$run <- "solution_36"
sr120_37 <- feature_representation(r120, sr120[, "solution_37"])
sr120_37$run <- "solution_37"
sr120_38 <- feature_representation(r120, sr120[, "solution_38"])
sr120_38$run <- "solution_38"
sr120_39 <- feature_representation(r120, sr120[, "solution_39"])
sr120_39$run <- "solution_39"
sr120_40 <- feature_representation(r120, sr120[, "solution_40"])
sr120_40$run <- "solution_40"
sr120_41 <- feature_representation(r120, sr120[, "solution_41"])
sr120_41$run <- "solution_41"
sr120_42 <- feature_representation(r120, sr120[, "solution_42"])
sr120_42$run <- "solution_42"
sr120_43 <- feature_representation(r120, sr120[, "solution_43"])
sr120_43$run <- "solution_43"
sr120_44 <- feature_representation(r120, sr120[, "solution_44"])
sr120_44$run <- "solution_44"
sr120_45 <- feature_representation(r120, sr120[, "solution_45"])
sr120_45$run <- "solution_45"
sr120_46 <- feature_representation(r120, sr120[, "solution_46"])
sr120_46$run <- "solution_46"
sr120_47 <- feature_representation(r120, sr120[, "solution_47"])
sr120_47$run <- "solution_47"
sr120_48 <- feature_representation(r120, sr120[, "solution_48"])
sr120_48$run <- "solution_48"
sr120_49 <- feature_representation(r120, sr120[, "solution_49"])
sr120_49$run <- "solution_49"
sr120_50 <- feature_representation(r120, sr120[, "solution_50"])
sr120_50$run <- "solution_50"
sr120_51 <- feature_representation(r120, sr120[, "solution_51"])
sr120_51$run <- "solution_51"
sr120_52 <- feature_representation(r120, sr120[, "solution_52"])
sr120_52$run <- "solution_52"
sr120_53 <- feature_representation(r120, sr120[, "solution_53"])
sr120_53$run <- "solution_53"
sr120_54 <- feature_representation(r120, sr120[, "solution_54"])
sr120_54$run <- "solution_54"
sr120_55 <- feature_representation(r120, sr120[, "solution_55"])
sr120_55$run <- "solution_55"
sr120_56 <- feature_representation(r120, sr120[, "solution_56"])
sr120_56$run <- "solution_56"
sr120_57 <- feature_representation(r120, sr120[, "solution_57"])
sr120_57$run <- "solution_57"
sr120_58 <- feature_representation(r120, sr120[, "solution_58"])
sr120_58$run <- "solution_58"
sr120_59 <- feature_representation(r120, sr120[, "solution_59"])
sr120_59$run <- "solution_59"
sr120_60 <- feature_representation(r120, sr120[, "solution_60"])
sr120_60$run <- "solution_60"
sr120_61 <- feature_representation(r120, sr120[, "solution_61"])
sr120_61$run <- "solution_61"
sr120_62 <- feature_representation(r120, sr120[, "solution_62"])
sr120_62$run <- "solution_62"
sr120_63 <- feature_representation(r120, sr120[, "solution_63"])
sr120_63$run <- "solution_63"
sr120_64 <- feature_representation(r120, sr120[, "solution_64"])
sr120_64$run <- "solution_64"
sr120_65 <- feature_representation(r120, sr120[, "solution_65"])
sr120_65$run <- "solution_65"
sr120_66 <- feature_representation(r120, sr120[, "solution_66"])
sr120_66$run <- "solution_66"
sr120_67 <- feature_representation(r120, sr120[, "solution_67"])
sr120_67$run <- "solution_67"
sr120_68 <- feature_representation(r120, sr120[, "solution_68"])
sr120_68$run <- "solution_68"
sr120_69 <- feature_representation(r120, sr120[, "solution_69"])
sr120_69$run <- "solution_69"
sr120_70 <- feature_representation(r120, sr120[, "solution_70"])
sr120_70$run <- "solution_70"
sr120_71 <- feature_representation(r120, sr120[, "solution_71"])
sr120_71$run <- "solution_71"
sr120_72 <- feature_representation(r120, sr120[, "solution_72"])
sr120_72$run <- "solution_72"
sr120_73 <- feature_representation(r120, sr120[, "solution_73"])
sr120_73$run <- "solution_73"
sr120_74 <- feature_representation(r120, sr120[, "solution_74"])
sr120_74$run <- "solution_74"
sr120_75 <- feature_representation(r120, sr120[, "solution_75"])
sr120_75$run <- "solution_75"
sr120_76 <- feature_representation(r120, sr120[, "solution_76"])
sr120_76$run <- "solution_76"
sr120_77 <- feature_representation(r120, sr120[, "solution_77"])
sr120_77$run <- "solution_77"
sr120_78 <- feature_representation(r120, sr120[, "solution_78"])
sr120_78$run <- "solution_78"
sr120_79 <- feature_representation(r120, sr120[, "solution_79"])
sr120_79$run <- "solution_79"
sr120_80 <- feature_representation(r120, sr120[, "solution_80"])
sr120_80$run <- "solution_80"
sr120_81 <- feature_representation(r120, sr120[, "solution_81"])
sr120_81$run <- "solution_81"
sr120_82 <- feature_representation(r120, sr120[, "solution_82"])
sr120_82$run <- "solution_82"
sr120_83 <- feature_representation(r120, sr120[, "solution_83"])
sr120_83$run <- "solution_83"
sr120_84 <- feature_representation(r120, sr120[, "solution_84"])
sr120_84$run <- "solution_84"
sr120_85 <- feature_representation(r120, sr120[, "solution_85"])
sr120_85$run <- "solution_85"
sr120_86 <- feature_representation(r120, sr120[, "solution_86"])
sr120_86$run <- "solution_86"
sr120_87 <- feature_representation(r120, sr120[, "solution_87"])
sr120_87$run <- "solution_87"
sr120_88 <- feature_representation(r120, sr120[, "solution_88"])
sr120_88$run <- "solution_88"
sr120_89 <- feature_representation(r120, sr120[, "solution_89"])
sr120_89$run <- "solution_89"
sr120_90 <- feature_representation(r120, sr120[, "solution_90"])
sr120_90$run <- "solution_90"
sr120_91 <- feature_representation(r120, sr120[, "solution_91"])
sr120_91$run <- "solution_91"
sr120_92 <- feature_representation(r120, sr120[, "solution_92"])
sr120_92$run <- "solution_92"
sr120_93 <- feature_representation(r120, sr120[, "solution_93"])
sr120_93$run <- "solution_93"
sr120_94 <- feature_representation(r120, sr120[, "solution_94"])
sr120_94$run <- "solution_94"
sr120_95 <- feature_representation(r120, sr120[, "solution_95"])
sr120_95$run <- "solution_95"
sr120_96 <- feature_representation(r120, sr120[, "solution_96"])
sr120_96$run <- "solution_96"
sr120_97 <- feature_representation(r120, sr120[, "solution_97"])
sr120_97$run <- "solution_97"
sr120_98 <- feature_representation(r120, sr120[, "solution_98"])
sr120_98$run <- "solution_98"
sr120_99 <- feature_representation(r120, sr120[, "solution_99"])
sr120_99$run <- "solution_99"
sr120_100 <- feature_representation(r120, sr120[, "solution_100"])
sr120_100$run <- "solution_100"
sr120_r<-bind_rows(sr120_1,sr120_2,sr120_3,sr120_4,sr120_5,sr120_6,sr120_7,sr120_8,sr120_9,sr120_10,sr120_11,sr120_12,sr120_13,sr120_14,sr120_15,sr120_16,sr120_17,sr120_18,sr120_19,sr120_20,sr120_21,sr120_22,sr120_23,sr120_24,sr120_25,sr120_26,sr120_27,sr120_28,sr120_29,sr120_30,sr120_31,sr120_32,sr120_33,sr120_34,sr120_35,sr120_36,sr120_37,sr120_38,sr120_39,sr120_40,sr120_41,sr120_42,sr120_43,sr120_44,sr120_45,sr120_46,sr120_47,sr120_48,sr120_49,sr120_50,sr120_51,sr120_52,sr120_53,sr120_54,sr120_55,sr120_56,sr120_57,sr120_58,sr120_59,sr120_60,sr120_61,sr120_62,sr120_63,sr120_64,sr120_65,sr120_66,sr120_67,sr120_68,sr120_69,sr120_70,sr120_71,sr120_72,sr120_73,sr120_74,sr120_75,sr120_76,sr120_77,sr120_78,sr120_79,sr120_80,sr120_81,sr120_82,sr120_83,sr120_84,sr120_85,sr120_86,sr120_87,sr120_88,sr120_89,sr120_90,sr120_91,sr120_92,sr120_93,sr120_94,sr120_95,sr120_96,sr120_97,sr120_98,sr120_99,sr120_100)
sr120_r$objective <- "Random"
sr120_r$species<-"120"


feature_a <- feature_abundances(r120, na.rm = FALSE)

sr120r<-left_join(sr120_r,feature_a)


#125
pu_dat125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_dat125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

r125 <- problem(pu_dat125, spec_dat125, cost_column = "cost", rij = puvsp_dat125) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat125$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_shuffle_portfolio(number_solutions = 100, remove_duplicates=TRUE) # return 100 solutions

sr125 <- solve(r125)

sr125<-as.data.frame(sr125) %>% as_tibble()

colnames(sr125)
sr125$objective<-"Random"
sr125$solution<-"125"


# Features Dataset
sr125_1 <- feature_representation(r125, sr125[, "solution_1"])
sr125_1$run <- "solution_1"
sr125_2 <- feature_representation(r125, sr125[, "solution_2"])
sr125_2$run <- "solution_2"
sr125_3 <- feature_representation(r125, sr125[, "solution_3"])
sr125_3$run <- "solution_3"
sr125_4 <- feature_representation(r125, sr125[, "solution_4"])
sr125_4$run <- "solution_4"
sr125_5 <- feature_representation(r125, sr125[, "solution_5"])
sr125_5$run <- "solution_5"
sr125_6 <- feature_representation(r125, sr125[, "solution_6"])
sr125_6$run <- "solution_6"
sr125_7 <- feature_representation(r125, sr125[, "solution_7"])
sr125_7$run <- "solution_7"
sr125_8 <- feature_representation(r125, sr125[, "solution_8"])
sr125_8$run <- "solution_8"
sr125_9 <- feature_representation(r125, sr125[, "solution_9"])
sr125_9$run <- "solution_9"
sr125_10 <- feature_representation(r125, sr125[, "solution_10"])
sr125_10$run <- "solution_10"
sr125_11 <- feature_representation(r125, sr125[, "solution_11"])
sr125_11$run <- "solution_11"
sr125_12 <- feature_representation(r125, sr125[, "solution_12"])
sr125_12$run <- "solution_12"
sr125_13 <- feature_representation(r125, sr125[, "solution_13"])
sr125_13$run <- "solution_13"
sr125_14 <- feature_representation(r125, sr125[, "solution_14"])
sr125_14$run <- "solution_14"
sr125_15 <- feature_representation(r125, sr125[, "solution_15"])
sr125_15$run <- "solution_15"
sr125_16 <- feature_representation(r125, sr125[, "solution_16"])
sr125_16$run <- "solution_16"
sr125_17 <- feature_representation(r125, sr125[, "solution_17"])
sr125_17$run <- "solution_17"
sr125_18 <- feature_representation(r125, sr125[, "solution_18"])
sr125_18$run <- "solution_18"
sr125_19 <- feature_representation(r125, sr125[, "solution_19"])
sr125_19$run <- "solution_19"
sr125_20 <- feature_representation(r125, sr125[, "solution_20"])
sr125_20$run <- "solution_20"
sr125_21 <- feature_representation(r125, sr125[, "solution_21"])
sr125_21$run <- "solution_21"
sr125_22 <- feature_representation(r125, sr125[, "solution_22"])
sr125_22$run <- "solution_22"
sr125_23 <- feature_representation(r125, sr125[, "solution_23"])
sr125_23$run <- "solution_23"
sr125_24 <- feature_representation(r125, sr125[, "solution_24"])
sr125_24$run <- "solution_24"
sr125_25 <- feature_representation(r125, sr125[, "solution_25"])
sr125_25$run <- "solution_25"
sr125_26 <- feature_representation(r125, sr125[, "solution_26"])
sr125_26$run <- "solution_26"
sr125_27 <- feature_representation(r125, sr125[, "solution_27"])
sr125_27$run <- "solution_27"
sr125_28 <- feature_representation(r125, sr125[, "solution_28"])
sr125_28$run <- "solution_28"
sr125_29 <- feature_representation(r125, sr125[, "solution_29"])
sr125_29$run <- "solution_29"
sr125_30 <- feature_representation(r125, sr125[, "solution_30"])
sr125_30$run <- "solution_30"
sr125_31 <- feature_representation(r125, sr125[, "solution_31"])
sr125_31$run <- "solution_31"
sr125_32 <- feature_representation(r125, sr125[, "solution_32"])
sr125_32$run <- "solution_32"
sr125_33 <- feature_representation(r125, sr125[, "solution_33"])
sr125_33$run <- "solution_33"
sr125_34 <- feature_representation(r125, sr125[, "solution_34"])
sr125_34$run <- "solution_34"
sr125_35 <- feature_representation(r125, sr125[, "solution_35"])
sr125_35$run <- "solution_35"
sr125_36 <- feature_representation(r125, sr125[, "solution_36"])
sr125_36$run <- "solution_36"
sr125_37 <- feature_representation(r125, sr125[, "solution_37"])
sr125_37$run <- "solution_37"
sr125_38 <- feature_representation(r125, sr125[, "solution_38"])
sr125_38$run <- "solution_38"
sr125_39 <- feature_representation(r125, sr125[, "solution_39"])
sr125_39$run <- "solution_39"
sr125_40 <- feature_representation(r125, sr125[, "solution_40"])
sr125_40$run <- "solution_40"
sr125_41 <- feature_representation(r125, sr125[, "solution_41"])
sr125_41$run <- "solution_41"
sr125_42 <- feature_representation(r125, sr125[, "solution_42"])
sr125_42$run <- "solution_42"
sr125_43 <- feature_representation(r125, sr125[, "solution_43"])
sr125_43$run <- "solution_43"
sr125_44 <- feature_representation(r125, sr125[, "solution_44"])
sr125_44$run <- "solution_44"
sr125_45 <- feature_representation(r125, sr125[, "solution_45"])
sr125_45$run <- "solution_45"
sr125_46 <- feature_representation(r125, sr125[, "solution_46"])
sr125_46$run <- "solution_46"
sr125_47 <- feature_representation(r125, sr125[, "solution_47"])
sr125_47$run <- "solution_47"
sr125_48 <- feature_representation(r125, sr125[, "solution_48"])
sr125_48$run <- "solution_48"
sr125_49 <- feature_representation(r125, sr125[, "solution_49"])
sr125_49$run <- "solution_49"
sr125_50 <- feature_representation(r125, sr125[, "solution_50"])
sr125_50$run <- "solution_50"
sr125_51 <- feature_representation(r125, sr125[, "solution_51"])
sr125_51$run <- "solution_51"
sr125_52 <- feature_representation(r125, sr125[, "solution_52"])
sr125_52$run <- "solution_52"
sr125_53 <- feature_representation(r125, sr125[, "solution_53"])
sr125_53$run <- "solution_53"
sr125_54 <- feature_representation(r125, sr125[, "solution_54"])
sr125_54$run <- "solution_54"
sr125_55 <- feature_representation(r125, sr125[, "solution_55"])
sr125_55$run <- "solution_55"
sr125_56 <- feature_representation(r125, sr125[, "solution_56"])
sr125_56$run <- "solution_56"
sr125_57 <- feature_representation(r125, sr125[, "solution_57"])
sr125_57$run <- "solution_57"
sr125_58 <- feature_representation(r125, sr125[, "solution_58"])
sr125_58$run <- "solution_58"
sr125_59 <- feature_representation(r125, sr125[, "solution_59"])
sr125_59$run <- "solution_59"
sr125_60 <- feature_representation(r125, sr125[, "solution_60"])
sr125_60$run <- "solution_60"
sr125_61 <- feature_representation(r125, sr125[, "solution_61"])
sr125_61$run <- "solution_61"
sr125_62 <- feature_representation(r125, sr125[, "solution_62"])
sr125_62$run <- "solution_62"
sr125_63 <- feature_representation(r125, sr125[, "solution_63"])
sr125_63$run <- "solution_63"
sr125_64 <- feature_representation(r125, sr125[, "solution_64"])
sr125_64$run <- "solution_64"
sr125_65 <- feature_representation(r125, sr125[, "solution_65"])
sr125_65$run <- "solution_65"
sr125_66 <- feature_representation(r125, sr125[, "solution_66"])
sr125_66$run <- "solution_66"
sr125_67 <- feature_representation(r125, sr125[, "solution_67"])
sr125_67$run <- "solution_67"
sr125_68 <- feature_representation(r125, sr125[, "solution_68"])
sr125_68$run <- "solution_68"
sr125_69 <- feature_representation(r125, sr125[, "solution_69"])
sr125_69$run <- "solution_69"
sr125_70 <- feature_representation(r125, sr125[, "solution_70"])
sr125_70$run <- "solution_70"
sr125_71 <- feature_representation(r125, sr125[, "solution_71"])
sr125_71$run <- "solution_71"
sr125_72 <- feature_representation(r125, sr125[, "solution_72"])
sr125_72$run <- "solution_72"
sr125_73 <- feature_representation(r125, sr125[, "solution_73"])
sr125_73$run <- "solution_73"
sr125_74 <- feature_representation(r125, sr125[, "solution_74"])
sr125_74$run <- "solution_74"
sr125_75 <- feature_representation(r125, sr125[, "solution_75"])
sr125_75$run <- "solution_75"
sr125_76 <- feature_representation(r125, sr125[, "solution_76"])
sr125_76$run <- "solution_76"
sr125_77 <- feature_representation(r125, sr125[, "solution_77"])
sr125_77$run <- "solution_77"
sr125_78 <- feature_representation(r125, sr125[, "solution_78"])
sr125_78$run <- "solution_78"
sr125_79 <- feature_representation(r125, sr125[, "solution_79"])
sr125_79$run <- "solution_79"
sr125_80 <- feature_representation(r125, sr125[, "solution_80"])
sr125_80$run <- "solution_80"
sr125_81 <- feature_representation(r125, sr125[, "solution_81"])
sr125_81$run <- "solution_81"
sr125_82 <- feature_representation(r125, sr125[, "solution_82"])
sr125_82$run <- "solution_82"
sr125_83 <- feature_representation(r125, sr125[, "solution_83"])
sr125_83$run <- "solution_83"
sr125_84 <- feature_representation(r125, sr125[, "solution_84"])
sr125_84$run <- "solution_84"
sr125_85 <- feature_representation(r125, sr125[, "solution_85"])
sr125_85$run <- "solution_85"
sr125_86 <- feature_representation(r125, sr125[, "solution_86"])
sr125_86$run <- "solution_86"
sr125_87 <- feature_representation(r125, sr125[, "solution_87"])
sr125_87$run <- "solution_87"
sr125_88 <- feature_representation(r125, sr125[, "solution_88"])
sr125_88$run <- "solution_88"
sr125_89 <- feature_representation(r125, sr125[, "solution_89"])
sr125_89$run <- "solution_89"
sr125_90 <- feature_representation(r125, sr125[, "solution_90"])
sr125_90$run <- "solution_90"
sr125_91 <- feature_representation(r125, sr125[, "solution_91"])
sr125_91$run <- "solution_91"
sr125_92 <- feature_representation(r125, sr125[, "solution_92"])
sr125_92$run <- "solution_92"
sr125_93 <- feature_representation(r125, sr125[, "solution_93"])
sr125_93$run <- "solution_93"
sr125_94 <- feature_representation(r125, sr125[, "solution_94"])
sr125_94$run <- "solution_94"
sr125_95 <- feature_representation(r125, sr125[, "solution_95"])
sr125_95$run <- "solution_95"
sr125_96 <- feature_representation(r125, sr125[, "solution_96"])
sr125_96$run <- "solution_96"
sr125_97 <- feature_representation(r125, sr125[, "solution_97"])
sr125_97$run <- "solution_97"
sr125_98 <- feature_representation(r125, sr125[, "solution_98"])
sr125_98$run <- "solution_98"
sr125_99 <- feature_representation(r125, sr125[, "solution_99"])
sr125_99$run <- "solution_99"
sr125_100 <- feature_representation(r125, sr125[, "solution_100"])
sr125_100$run <- "solution_100"
sr125_r<-bind_rows(sr125_1,sr125_2,sr125_3,sr125_4,sr125_5,sr125_6,sr125_7,sr125_8,sr125_9,sr125_10,sr125_11,sr125_12,sr125_13,sr125_14,sr125_15,sr125_16,sr125_17,sr125_18,sr125_19,sr125_20,sr125_21,sr125_22,sr125_23,sr125_24,sr125_25,sr125_26,sr125_27,sr125_28,sr125_29,sr125_30,sr125_31,sr125_32,sr125_33,sr125_34,sr125_35,sr125_36,sr125_37,sr125_38,sr125_39,sr125_40,sr125_41,sr125_42,sr125_43,sr125_44,sr125_45,sr125_46,sr125_47,sr125_48,sr125_49,sr125_50,sr125_51,sr125_52,sr125_53,sr125_54,sr125_55,sr125_56,sr125_57,sr125_58,sr125_59,sr125_60,sr125_61,sr125_62,sr125_63,sr125_64,sr125_65,sr125_66,sr125_67,sr125_68,sr125_69,sr125_70,sr125_71,sr125_72,sr125_73,sr125_74,sr125_75,sr125_76,sr125_77,sr125_78,sr125_79,sr125_80,sr125_81,sr125_82,sr125_83,sr125_84,sr125_85,sr125_86,sr125_87,sr125_88,sr125_89,sr125_90,sr125_91,sr125_92,sr125_93,sr125_94,sr125_95,sr125_96,sr125_97,sr125_98,sr125_99,sr125_100)
sr125_r$objective <- "Random"
sr125_r$species<-"125"


feature_a <- feature_abundances(r125, na.rm = FALSE)

sr125r<-left_join(sr125_r,feature_a)


# Random Features Dataset
random.features <- bind_rows(sr5r,sr10r,sr15r,sr20r,sr25r,sr30r,sr35r,sr40r,sr45r,sr50r,sr55r,sr60r,sr65r,sr70r,sr75r,sr80r,sr85r,sr90r,sr95r,sr100r,sr105r,sr110r,sr115r,sr120r,sr125r)

View(random.features)

write.csv(random.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random.features.csv")


#objective.features
master.features <- bind_rows(objective.features,random.features)
View(master.features)

dmf<-distinct(master.features,objective, species)
View(dmf)
write.csv(master.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master.features.csv")


# bind wrangle for fig 1
random_dat <- bind_rows(sr5,sr10,sr15,sr20,sr25,sr30,sr35,sr40,sr45,sr50,sr55,sr60,sr65,sr70,sr75,sr80,sr85,sr90,sr95,sr100,sr105,sr110,sr115,sr120,sr125)

colnames(random_dat)
random_dat <- random_dat %>% mutate(sf = rowSums(.[6:105])) 

colnames(random_dat)

random_datt <- random_dat %>%  gather(run, selection,7:106) 
  #filter(selection != 0) 

head(random_datt)

write.csv(random_datt,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_dat.csv")

random_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
random_dat$total_solution<-"100"

head(random_dat)


colnames(comp_s)
comp_st <- comp_s %>%  gather(run, selection,7:38) 
  #filter(selection != 0) 
  
head(comp_st)

head(bfly_s)

bfly_st <- bfly_s %>%  gather(run, selection,7:8) 
  #filter(selection != 0) 

head(bfly_st)

colnames(bird_s)

bird_st <- bird_s %>%  gather(run, selection,7:30) 
  #filter(selection != 0) 

head(bird_st)

colnames(dom_s)

dom_st <- dom_s %>%  gather(run, selection,solution_1) 
  #filter(selection != 0) 

head(dom_st)

colnames(pdivf_s)

pdivf_st <- pdivf_s %>%  gather(run, selection,7:106) 
  #filter(selection != 0) 

head(pdivf_st)

colnames(pdivg_s)

pdivg_st <- pdivg_s %>%  gather(run, selection,7:106) 
  #filter(selection != 0) 

head(pdivg_st)

colnames(pwf_s)

pwf_st <- pwf_s %>%  gather(run, selection,7:101) 
  #filter(selection != 0) 

head(pwf_st)

colnames(pwg_s)

pwg_st <- pwg_s %>%  gather(run, selection,7:106) 
  #filter(selection != 0) 

head(pwg_st)

dat<-bind_rows(comp_st,bfly_st,bird_st,dom_st,pdivf_st,pdivg_st,pwf_st,pwg_st) 
View(dat)
write.csv(dat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/solution_dat.csv")

solution_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/solution_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
random_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
random_dat$total_solution<-"100"

solution_dat$solution <- as.numeric(solution_dat$solution)
random_dat$solution <- as.numeric(random_dat$solution)

summary(solution_dat)
summary(random_dat)
random_dat$total_solution <- as.numeric(random_dat$total_solution)
random_dat$selection <- as.numeric(random_dat$selection)

master_dat<-bind_rows(solution_dat,random_dat)

View(master_dat)

dat1 <- master_dat %>% as_tibble() %>%
  group_by(id,objective) %>%
  filter(run == "solution_1") %>%
  filter(selection != 0) 

View(dat1)

levels(dat1$objective)


dat1 <- dat1 %>% mutate( objective = fct_recode( objective,  "Lepidoptera Relationship (n=35)" = "Butterfly (n=35)",
                                                 "Pairwise Lepidoptera + Plant Rich Family (n=47)" = "Pairwise Butterfly Div Family (n=47)",
                                                 "Pairwise Lepidoptera + Plant Rich Genus (n=119)" = "Pairwise Butterfly Div Genus (n=119)"
))


# Figure 2 Draft 1
ggplot(dat1,aes(x=objective, y=totalcf,color=objective),alpha=0.5,dotsize=0.5) + 
  geom_violin(trim = FALSE) +
  geom_jitter(position = position_jitter(0.2),alpha=0.4) +
  #geom_dotplot(aes(fill=objective), alpha=0.5,dotsize=0.5,binaxis = "y",  stackdir = "center", stackratio=0.5) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Trait Sum',title='Objectives',fill='Objective',color='Objective') + 
  theme_classic()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + theme(legend.position="bottom")


# Figure 2
# wrap text axis labels
dat1$objective2 = str_wrap(dat1$objective, width = 10)
dat1$objective<- as.factor(dat1$objective)
levels(dat1$objective)


dat1 <- dat1 %>% mutate( objective = fct_recode( objective,  "Lepidoptera Relationship (n=35)" = "Butterfly (n=35)",
                                                 "Pairwise Lepidoptera + Plant Rich Family (n=47)" = "Pairwise Butterfly Div Family (n=47)",
                                                 "Pairwise Lepidoptera + Plant Rich Genus (n=119)" = "Pairwise Butterfly Div Genus (n=119)",
                                                 "Plant Rich Family (n=34)" = "Plant Div Family (n=34)",
                                                 "Plant Rich Genus (n=115)" = "Plant Div Genus (n=115)"
))

dat1$objective <- factor(dat1$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)",  "Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))

#"#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#000000","#252525","#525252"

ggplot(dat1,aes(x=objective, y=totalcf, color=objective),alpha=0.6,dotsize=0.5) + 
  geom_violin(aes(color = NA, fill=objective), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  #geom_dotplot(aes(fill=objective), alpha=0.5,dotsize=0.5,binaxis = "y",  stackdir = "center", stackratio=0.5) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Attribute Sum',fill='Objective',color='Objective') + 
  scale_color_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  scale_fill_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  theme_classic() + theme(legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#Expanded Fig 2
head(dat1)
dat1$solution <- as.factor(as.character(dat1$solution))
levels(dat1$solution)
View(dat1)
levels(dat1$objective)

dat.sep <- dat1 %>% filter(objective == "Bird (n=5)"  | objective == "Lepidoptera Relationship (n=35)" | 
                             objective == "Dominants (n=37)" | objective == "Plant Rich Genus (n=115)"   )

dat.sep$dot<- "."

dat.sep <- dat.sep %>% unite( solution, dot, solution, sep="")

View(dat.sep)

dat2 <- dat1 %>%  filter(!objective == "Bird (n=5)"  ) %>% filter( !objective == "Lepidoptera Relationship (n=35)"  ) %>% filter( 
                           !objective == "Dominants (n=37)"  ) %>% filter( !objective == "Plant Rich Genus (n=115)"   ) %>% droplevels() %>%
 bind_rows(dat.sep)


View(dat2)
dat2$solution <- factor(dat2$solution, levels=c("5",".5","10","15","20","25","30","34","35",".35","37",".37","39","40","45","47","50","55","60","65","70","75","80","85","90","95","100","105","110","115",".115","116","119","120","121","125"))
dat2$objective <- factor(dat2$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)",  "Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))

ggplot(dat2,aes(x=solution, y=totalcf, color=objective),alpha=0.6,dotsize=0.5) + 
  geom_violin(aes(color = NA, fill=objective), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  #geom_dotplot(aes(fill=objective), alpha=0.5,dotsize=0.5,binaxis = "y",  stackdir = "center", stackratio=0.5) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Attribute Sum',fill='Objective',color='Objective') + 
  scale_color_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  scale_fill_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF" ,"#15983DFF","#1F78B4","#666666")) +
  theme_classic() + theme(legend.position="bottom") #+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


View(master_dat)
master_dat$sf <- as.numeric(master_dat$sf)
master_dat$total_solution <- as.numeric(master_dat$total_solution)
master_dat$percent_sf <- (master_dat$sf/master_dat$total_solution) * 100

View(master_dat)


write.csv(master_dat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master_dat.csv")
master_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# Figure 3 : Irreplaceability
colnames(master_dat)


master_dat$Importance <- ifelse(master_dat$percent_sf ==0, 'Redundant',
                         ifelse(master_dat$percent_sf >=1 & master_dat$percent_sf <=99, 'Variable',
                         ifelse(master_dat$percent_sf >=100, 'Irreplaceable', 'other')))

View(master_dat)
dat2 <- master_dat %>% as_tibble() %>%
  group_by(id,objective) %>%
  filter(run == "solution_1") 
  #filter(selection != 0) 


View(dat2)
#"#7F8624FF","#52194CFF","#751029FF"



dat2 <- dat2 %>%  mutate( objective = fct_recode( objective,  "Lepidoptera Relationship (n=35)" = "Butterfly (n=35)",
                                                     "Pairwise Lepidoptera + Plant Rich Family (n=47)" = "Pairwise Butterfly Div Family (n=47)",
                                                     "Pairwise Lepidoptera + Plant Rich Genus (n=119)" = "Pairwise Butterfly Div Genus (n=119)",
                                                     "Plant Rich Family (n=34)" = "Plant Div Family (n=34)",
                                                     "Plant Rich Genus (n=115)" = "Plant Div Genus (n=115)"
))


dat2$objective <- factor(dat2$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)",  "Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))
dat2$Importance <- factor(dat2$Importance, levels=c("Redundant","Variable","Irreplaceable"))

# Figure 3

ggplot(dat2,aes(x=Importance, y=totalcf,color=Importance),alpha=0.6,dotsize=0.5) + 
  facet_wrap(facets= "objective") +
  geom_violin(aes(color = NA, fill=Importance), alpha=0.4,trim = FALSE,color=NA) +
  geom_jitter(position = position_jitter(0.2),alpha=0.6) +
  #geom_dotplot(aes(fill=objective), alpha=0.5,dotsize=0.5,binaxis = "y",  stackdir = "center", stackratio=0.5) + 
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               fun.y=mean, geom="pointrange", size=0.6,  fill="black",color="black",alpha=0.6) + 
  labs(x = '',y = 'Attribute Sum',fill='Importance',color='Importance') + 
 # ylim(0,70)+
  scale_color_manual(values=c("#751029FF","#52194CFF","#7F8624FF")) +
  scale_fill_manual(values=c("#751029FF","#52194CFF","#7F8624FF")) +
  theme_classic() + theme(legend.position="bottom") + theme(axis.title.x=element_blank(),
                                                          axis.text.x=element_blank(),
                                                          axis.ticks.x=element_blank())


View(master_dat)

# Figure 4
master.features <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master.features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
head(master.features)

distinct(master.features,objective, species)

master.features$percent_held <- master.features$relative_held * 100

master.features$name<-master.features$feature
# load and bind all spec data
#"Bird (n=6)","Plant Div Family (n=34)","Butterfly (n=37)", "Comprehensive (n=38)", "Dominants (n=38)","Pairwise Butterfly Div Family (n=48)", "Plant Div Genus (n=115)","Pairwise Butterfly Div Genus (n=120)", "Random"))

comp_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
comp_spec$objective <- "Comprehensive (n=37)"
head(comp_spec)
bfly_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
bfly_spec$objective <- "Butterfly (n=35)"
bird_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
bird_spec$objective <- "Bird (n=5)"
pdivf_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
pdivf_spec$objective <- "Plant Div Family (n=34)"
pdivg_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
pdivg_spec$objective <- "Plant Div Genus (n=115)"
pwf_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
pwf_spec$objective <- "Pairwise Butterfly Div Family (n=47)"
pwg_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
pwg_spec$objective <- "Pairwise Butterfly Div Genus (n=119)"

ob.feat<- droplevels(subset(master.features, objective == "Bird (n=5)"| objective ==  "Plant Div Family (n=34)"|objective == "Butterfly (n=35)"|objective ==  "Comprehensive (n=37)"|objective ==  "Dominants (n=37)"|objective == "Pairwise Butterfly Div Family (n=47)"|objective ==  "Plant Div Genus (n=115)"|objective == "Pairwise Butterfly Div Genus (n=119)"))

ob.spec<- bind_rows(comp_spec,bfly_spec,bird_spec,pdivf_spec,pdivg_spec,pwf_spec,pwg_spec)

ob.feats<-left_join(ob.feat,ob.spec)


ob.feats$pres <- ifelse(ob.feats$absolute_held ==0, '0',
                 ifelse(ob.feats$absolute_held >=1, '1', 'other'))

ob.feats$value<- "1"
head(ob.feats)
write.csv(ob.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/objective_features2.csv")

ob.feats$pres<-as.numeric(ob.feats$pres)
ob.feats$value<-as.numeric(ob.feats$value)
summ.feats<- ob.feats %>% group_by(category, objective, run, species) %>%
  summarise(absolute_held= sum(absolute_held),absolute_abundance=sum(absolute_abundance),target=sum(target),pres=sum(pres), value=sum(value))


summ.feats$percent_targs<-(summ.feats$pres/summ.feats$value)*100
View(summ.feats)

summ.feats <- summ.feats %>%
  filter(category != "NA") %>% 
  droplevels()

write.csv(summ.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_objective_features.csv")

# Random
r5_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r5_spec$objective <- "Random"
r5_spec$species <- "5"
r10_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r10_spec$objective <- "Random"
r10_spec$species <- "10"
r15_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r15_spec$objective <- "Random"
r15_spec$species <- "15"
r20_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r20_spec$objective <- "Random"
r20_spec$species <- "20"
r25_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r25_spec$objective <- "Random"
r25_spec$species <- "25"
r30_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r30_spec$objective <- "Random"
r30_spec$species <- "30"
r35_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r35_spec$objective <- "Random"
r35_spec$species <- "35"
r40_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r40_spec$objective <- "Random"
r40_spec$species <- "40"
r45_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r45_spec$objective <- "Random"
r45_spec$species <- "45"
r50_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r50_spec$objective <- "Random"
r50_spec$species <- "50"
r55_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r55_spec$objective <- "Random"
r55_spec$species <- "55"
r60_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r60_spec$objective <- "Random"
r60_spec$species <- "60"
r65_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r65_spec$objective <- "Random"
r65_spec$species <- "65"
r70_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r70_spec$objective <- "Random"
r70_spec$species <- "70"
r75_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r75_spec$objective <- "Random"
r75_spec$species <- "75"
r80_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r80_spec$objective <- "Random"
r80_spec$species <- "80"
r85_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r85_spec$objective <- "Random"
r85_spec$species <- "85"
r90_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r90_spec$objective <- "Random"
r90_spec$species <- "90"
r95_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r95_spec$objective <- "Random"
r95_spec$species <- "95"
r100_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r100_spec$objective <- "Random"
r100_spec$species <- "100"
r105_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r105_spec$objective <- "Random"
r105_spec$species <- "105"
r110_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r110_spec$objective <- "Random"
r110_spec$species <- "110"
r115_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r115_spec$objective <- "Random"
r115_spec$species <- "115"
r120_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r120_spec$objective <- "Random"
r120_spec$species <- "120"
r125_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
r125_spec$objective <- "Random"
r125_spec$species <- "125"

random.spec<-bind_rows(r5_spec,r10_spec,r15_spec,r20_spec,r25_spec,r30_spec,r35_spec,r40_spec,r45_spec,r50_spec,r55_spec,r60_spec,r65_spec,r70_spec,r75_spec,r80_spec,r85_spec,r90_spec,r95_spec,r100_spec,r105_spec,r110_spec,r115_spec,r120_spec,r125_spec)
random.feat<- master.features %>% filter( objective == "Random")

View(random.feat)
levels(random.feat$feature)[levels(random.feat$feature)=="Random"] <- "Species"

random.feat$species<-as.character(as.factor(random.feat$species))
random.feats<-left_join(random.feat,random.spec)

head(random.feats)
# load and bind

random.feats$pres <- ifelse(random.feats$absolute_held ==0, '0',
                        ifelse(random.feats$absolute_held >=1, '1', 'other'))

random.feats$value<- "1"
head(random.feats)
write.csv(random.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_features2.csv")

random.feats$pres<-as.numeric(random.feats$pres)
random.feats$value<-as.numeric(random.feats$value)
random.summ.feats<- random.feats %>% group_by(category, objective, run, species) %>%
  summarise(absolute_held= sum(absolute_held),absolute_abundance=sum(absolute_abundance),target=sum(target),pres=sum(pres), value=sum(value))


random.summ.feats$percent_targs<-(random.summ.feats$pres/random.summ.feats$value)*100
head(random.summ.feats)

random.summ.feats <- random.summ.feats %>%
  filter(category != "Species") %>% 
  droplevels()

View(random.summ.feats)
write.csv(random.summ.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features.csv")

random.summ.feat<- random.summ.feats %>% group_by(category, objective, species) %>%
  summarise(mean.held = quantile(percent_targs, probs = 0.50),
            lower.held = quantile(percent_targs, probs = 0.025),
            upper.held = quantile(percent_targs, probs = 0.975))

View(random.summ.feat)

write.csv(random.summ.feat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features2.csv")



# Random cloud
library(ggplot2)
library(tidyverse)
pdata<- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

View(pdata)
head(pdata)
pdata$category <- as.factor(pdata$category)
levels(pdata$category)


pdata <- pdata %>% mutate( category = fct_recode( category,  "Plant Rich Family" = "Family",
                                                      "Plant Rich Genus" = "Genus",
                                                      "Lepidoptera Pollinator"   =  "Butterfly Pollinator",
                                                      "Lepidoptera Pollinator" = "Butterfly Nectar",
                                                      "Lepidoptera Herbivory"  = "Butterfly Larval" ))

pdata$category <- factor(pdata$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome", "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))


ggplot(pdata, aes(x=species, y=percent_targs)) +
  geom_jitter(aes(color=species), size=6,width = 0.5, height = 0.5,alpha=0.7)+
  facet_grid(. ~ category)+ facet_wrap(~category, labeller = labeller(groupwrap = label_wrap_gen(10)))+
  labs(x = 'Number of Plant Species',y = 'Percentage of Targets Met',color=' Number of Plant Species') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+ theme(axis.text.x=element_text(angle=45, hjust=1)) 



#MIXED LAYERS CLOUD + MEAN + QUANTILES ONL OF RANDOM
edata<- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_objective_features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
rdata<- read.table("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_random_features2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rdata.l <- rdata %>% gather(quantile,percent_targs,mean.held:upper.held)


rdata.l$category <- as.factor(rdata.l$category)
levels(rdata.l$category)
levels(rdata$category)
levels(edata$category)


rdata.l <- rdata.l %>% mutate( category = fct_recode( category,  "Plant Rich Family" = "Family",
                                                  "Plant Rich Genus" = "Genus",
                                                  "Lepidoptera Pollinator"   =  "Butterfly Pollinator",
                                                  "Lepidoptera Pollinator" = "Butterfly Nectar",
                                                  "Lepidoptera Herbivory"  = "Butterfly Larval"))

rdata.l$category <- factor(rdata.l$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome", "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))

edata <- edata %>% mutate( category = fct_recode( category,  "Plant Rich Family" = "Family",
                                                      "Plant Rich Genus" = "Genus",
                                                  "Lepidoptera Pollinator"   =  "Butterfly Pollinator",
                                                  "Lepidoptera Pollinator" = "Butterfly Nectar",
                                                  "Lepidoptera Herbivory"  = "Butterfly Larval"
                                                  ))

edata$category <- factor(edata$category, levels=c( "Bird Trophic","Bird Herbivory", "Bird Shelter", "Dispersal Syndrome",  "Lepidoptera Pollinator", "Lepidoptera Herbivory","Pollination Syndrome", "Mammal Herbivory", "Nitrogen Fixation", "Flowering Month","Plant Rich Genus","Plant Rich Family"))





edata <- edata %>% mutate( objective = fct_recode( objective,  "Lepidoptera Relationship (n=35)" = "Butterfly (n=35)",
                                                       "Pairwise Lepidoptera + Plant Rich Family (n=47)" = "Pairwise Butterfly Div Family (n=47)",
                                                       "Pairwise Lepidoptera + Plant Rich Genus (n=119)" = "Pairwise Butterfly Div Genus (n=119)",
                                                       "Plant Rich Family (n=34)" = "Plant Div Family (n=34)",
                                                       "Plant Rich Genus (n=115)" = "Plant Div Genus (n=115)"
))

edata$objective <- factor(edata$objective, levels=c("Bird (n=5)","Plant Rich Family (n=34)","Lepidoptera Relationship (n=35)", "Comprehensive (n=37)", "Dominants (n=37)","Pairwise Lepidoptera + Plant Rich Family (n=47)" , "Plant Rich Genus (n=115)","Pairwise Lepidoptera + Plant Rich Genus (n=119)", "Random"))


head(edata)
distinct(edata,objective, species)

ggplot() +
  geom_jitter(data=rdata.l, aes(x=species, y=percent_targs),color="#666666", size=5,width = 0.5, height = 0.5, alpha=0.4) +
  geom_jitter(data=edata, aes(x=species, y=percent_targs,color=objective), size=6,width = 0.5, height = 0.5, alpha=0.8) +
  scale_colour_manual(values=c("#E7298A","#66A61E","#7570B3","#D95F02","#E6AB02","#16A08CFF","#15983DFF","#1F78B4","#666666")) + 
  facet_grid(. ~ category)+ facet_wrap(~category, labeller = labeller(groupwrap = label_wrap_gen(10)))+
  labs(x = 'Number of Plant Species',y = 'Percentage of Targets Met',color='Objective') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+ theme(axis.text.x=element_text(angle=45, hjust=1)) 





# Table S1

master_new <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master_new.csv")

master_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



View(master_dat)

master_dat$objective<- as.factor(master_dat$objective)
levels(master_dat$objective)

# add percent sf for each sp
sf_p <- master_dat %>%  filter(objective == "Bird (n=5)"  | objective == "Butterfly (n=35)"  | 
                                 objective == "Comprehensive (n=37)" | 
                                 objective == "Pairwise Butterfly Div Family (n=47)" | objective == "Pairwise Butterfly Div Genus (n=119)") %>%
   dplyr::select(s, totalcf, objective, percent_sf) %>% distinct() %>% spread(objective, percent_sf)



View(sf_p)

colnames(master_new)

master_dat2 <- sf_p %>% left_join(master_new) %>%
  mutate(  Habitat_Frequency = freq6170   ) %>%
  dplyr::select(-c("X", "Species", "freq6150","freq6210","freq6230","freq651020", "growthform","freq6170"))

colnames(master_dat2)
head(master_dat2)


# add solution 1
solution_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/solution_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(solution_dat)

s_dat <- solution_dat %>% as_tibble() %>%
  group_by(id,objective) %>%
  filter(run == "solution_1") %>%
  filter(objective == "Bird (n=5)"  | objective == "Butterfly (n=35)"  | 
           objective == "Comprehensive (n=37)" | 
           objective == "Pairwise Butterfly Div Family (n=47)" | objective == "Pairwise Butterfly Div Genus (n=119)") %>%
  dplyr::select(s,objective,selection)  %>% spread(objective, selection)

View(s_dat)

master_dat3 <- master_dat2 %>% left_join(s_dat, by =("s"))

colnames(master_dat3)


write.csv(master_dat3, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/3_Table S1_new.csv")




