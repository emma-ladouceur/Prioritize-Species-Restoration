


# In this script we:
# _________________________________________________________
# ############# CREATE PROBLEMS ###################
# ############# FIND SOLUTIONS ###################
# ############# WRANGLE DATA ###################
# _________________________________________________________


# load packages
library(tidyverse)
library(prioritizr)
library(data.table)
library(gurobi)
library(slam)
library(purrr)


# _________________________________________________________
# ########################################################
# ############# PROBLEMS & SOLUTIONS ###################
# ########################################################
# _________________________________________________________

# Comprehensive Objective
# tabular planning unit (pu) data, which is in this case, a list of plants
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data, which in this case, is our plant species attributes
comp_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# pu vs sp - planning units vs features matrix, or in this case a plant species- attributes matrix- in long format
puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

??add_gap_portfolio
?? add_gurobi_solver

# create the problem
p1 <- problem(pu_dat, comp_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(comp_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0)  %>% # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

# a bit of an example on exploring the problem here
# print problem
print(p1)
# print number of planning units
number_of_planning_units(p1)
# print number of features
number_of_features(p1)

# solve the problem
s1 <- solve(p1)

# print solution
print(s1)

# create a vector of solution names
solution_col_names <-  s1 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions 
fr_comp <-  solution_col_names %>% 
  map( ~feature_representation(p1, s1[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
comp_prep <- as.data.frame(s1) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
comp_s <- comp_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(comp_prep) %>%
  mutate(objective = "Comprehensive (n=37)",
         solution_amount = "37") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_comp <- feature_abundances(p1, na.rm = FALSE)

# join feature representation in solution and total feature abundance
compr <- left_join(fr_comp,feature_comp) %>%
  mutate(objective = "Comprehensive (n=37)",
         species = "37")


# Lepidoptera Relationships
# planning units
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
bfly_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# pu v sp
puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Butterfly/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# create problem
p2 <- problem(pu_dat, bfly_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(bfly_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0)  %>% # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

# solve
s2 <- solve(p2)

solution_col_names <-  s2 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_bfly <-  solution_col_names %>% 
  map( ~feature_representation(p2, s2[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
bfly_prep <- as.data.frame(s2) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
bfly_s <- bfly_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(blfy_prep) %>%
  mutate(objective = "Lepidoptera Relationship (n=35)",
         solution_amount = "35") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_bfly <- feature_abundances(p2, na.rm = FALSE)

# join feature representation in solution and total feature abundance
bflyr <- left_join(fr_bfly,feature_bfly)  %>%
  mutate(objective = "Lepidoptera Relationship (n=35)",
         species = "35")
head(bflyr)


# Birds
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
bird_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Bird/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# create problem
p3 <- problem(pu_dat, bird_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(bird_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s3 <- solve(p3)

solution_col_names <-  s3 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_bird <-  solution_col_names %>% 
  map( ~feature_representation(p3, s3[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
bird_prep <- as.data.frame(s3) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
bird_s <- bird_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(bird_prep) %>%
  mutate(objective = "Bird (n=5)",
         solution_amount = "5") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_bird <- feature_abundances(p3, na.rm = FALSE)

# join feature representation in solution and total feature abundance
birdr <- left_join(fr_bird,feature_bird) %>%
  mutate(objective = "Bird (n=5)",
         species = "5")
head(birdr)


# Dominants
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
dom_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Dominants/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# create problem
p4 <- problem(pu_dat, dom_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(dom_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s4 <- solve(p4)

solution_col_names <-  s4 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_dom <-  solution_col_names %>% 
  map( ~feature_representation(p4, s4[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
dom_prep <- as.data.frame(s4) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
dom_s <- dom_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(dom_prep) %>%
  mutate(objective = "Dominants (n=37)",
         solution_amount = "37") %>% group_by(solution_n) %>%
   arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_dom <- feature_abundances(p4, na.rm = FALSE)

# join feature representation in solution and total feature abundance
domr <- left_join(fr_dom,feature_dom) %>%
  mutate(objective = "Dominants (n=37)",
         species = "37") 
head(domr)


# Plant Rich Family 
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
prichf_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Fam/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p5 <- problem(pu_dat, prichf_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(prichf_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s5 <- solve(p5)


solution_col_names <-  s5 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_prichf <-  solution_col_names %>% 
  map( ~feature_representation(p5, s5[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
prichf_prep <- as.data.frame(s5) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
prichf_s <- prichf_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(prichf_prep) %>%
  mutate(objective =  "Plant Rich Family (n=34)",
         solution_amount = "34") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_prichf <- feature_abundances(p5, na.rm = FALSE)

# join feature representation in solution and total feature abundance
prichfr <- left_join(fr_prichf,feature_prichf) %>%
  mutate(objective =  "Plant Rich Family (n=34)",
         species = "34")
head(prichfr)


# Plant Rich Genus
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
prichg_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PDiv_Gen/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p6 <- problem(pu_dat, prichg_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(prichg_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s6 <- solve(p6)


solution_col_names <-  s6 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_prichg <-  solution_col_names %>% 
  map( ~feature_representation(p6, s6[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
prichg_prep <- as.data.frame(s6) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
prichg_s <- prichg_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(prichg_prep) %>%
  mutate(objective =  "Plant Rich Genus (n=115)",
         solution_amount = "115") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_prichg <- feature_abundances(p6, na.rm = FALSE)

# join feature representation in solution and total feature abundance
prichgr <- left_join(fr_prichg,feature_prichg)  %>%
  mutate(objective =  "Plant Rich Genus (n=115)",
         species = "115")
head(prichgr)


# Pairwise Lepidoptera + Plant Rich Family
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
pwf_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_F/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# create problem
p7 <- problem(pu_dat, pwf_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(pwf_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s7 <- solve(p7)


solution_col_names <-  s7 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_pwf <-  solution_col_names %>% 
  map( ~feature_representation(p7, s7[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
pwf_prep <- as.data.frame(s7) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
pwf_s <- pwf_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(pwf_prep) %>%
  mutate(objective =   "Pairwise Lepidoptera + Plant Rich Family (n=47)",
         solution_amount = "47") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_pwf <- feature_abundances(p7, na.rm = FALSE)

# join feature representation in solution and total feature abundance
pwfr <- left_join(fr_pwf,feature_pwf) %>%
  mutate(objective =   "Pairwise Lepidoptera + Plant Rich Family (n=47)",
         species = "47")
head(pwfr)


# Pairwise Lepidoptera + Plant Rich Genus
pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# feature data
pwg_spec <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/PW_Butterfly_G/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# create problem
p8 <- problem(pu_dat, pwg_spec, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(pwg_spec$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  #optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

s8 <- solve(p8)


solution_col_names <-  s8 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_pwg <-  solution_col_names %>% 
  map( ~feature_representation(p8, s8[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
pwg_prep <- as.data.frame(s8) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
pwg_s <- pwg_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(pwg_prep) %>%
  mutate(objective =     "Pairwise Lepidoptera + Plant Rich Genus (n=119)",
         solution_amount = "119") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s) %>% as_tibble()

# pull out feature abundance
feature_pwg <- feature_abundances(p8, na.rm = FALSE)

# join feature representation in solution and total feature abundance
pwgr <- left_join(fr_pwg,feature_pwg) %>%
  mutate(objective =     "Pairwise Lepidoptera + Plant Rich Genus (n=119)",
         species = "119")




# RANDOM
# 5
pu_dat5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat5 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/5/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp5 <- problem(pu_dat5, spec_r5, cost_column = "cost", rij = puvsp_dat5) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r5$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs5 <- solve(rp5)


solution_col_names <-  rs5 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr5 <-  solution_col_names %>% 
  map( ~feature_representation(rp5, rs5[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr5_prep <- as.data.frame(rs5) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr5_s <- sr5_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr5_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "5") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr5 <- feature_abundances(rp5, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr5r <- left_join(fr_sr5,feature_sr5) %>%
  mutate(objective =   "Random",
         species = "5")




# RANDOM
# 10
pu_dat10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat10 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/10/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp10 <- problem(pu_dat10, spec_r10, cost_column = "cost", rij = puvsp_dat10) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r10$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs10 <- solve(rp10)


solution_col_names <-  rs10 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr10 <-  solution_col_names %>% 
  map( ~feature_representation(rp10, rs10[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr10_prep <- as.data.frame(rs10) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr10_s <- sr10_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr10_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "10") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr10 <- feature_abundances(rp10, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr10r <- left_join(fr_sr10,feature_sr10) %>%
  mutate(objective =   "Random",
         species = "10")
head(sr10r)


# RANDOM
# 15
pu_dat15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat15 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/15/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp15 <- problem(pu_dat15, spec_r15, cost_column = "cost", rij = puvsp_dat15) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r15$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs15 <- solve(rp15)


solution_col_names <-  rs15 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr15 <-  solution_col_names %>% 
  map( ~feature_representation(rp15, rs15[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr15_prep <- as.data.frame(rs15) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr15_s <- sr15_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr15_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "15") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr15 <- feature_abundances(rp15, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr15r <- left_join(fr_sr15,feature_sr15) %>%
  mutate(objective =   "Random",
         species = "15")
head(sr15r)


# RANDOM
# 20
pu_dat20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat20 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/20/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp20 <- problem(pu_dat20, spec_r20, cost_column = "cost", rij = puvsp_dat20) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r20$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs20 <- solve(rp20)


solution_col_names <-  rs20 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr20 <-  solution_col_names %>% 
  map( ~feature_representation(rp20, rs20[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr20_prep <- as.data.frame(rs20) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr20_s <- sr20_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr20_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "20") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr20 <- feature_abundances(rp20, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr20r <- left_join(fr_sr20,feature_sr20) %>%
  mutate(objective =   "Random",
         species = "20")
head(sr20r)



# RANDOM
# 25
pu_dat25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat25 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/25/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp25 <- problem(pu_dat25, spec_r25, cost_column = "cost", rij = puvsp_dat25) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r25$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs25 <- solve(rp25)


solution_col_names <-  rs25 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr25 <-  solution_col_names %>% 
  map( ~feature_representation(rp25, rs25[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr25_prep <- as.data.frame(rs25) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr25_s <- sr25_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr25_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "25") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr25 <- feature_abundances(rp25, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr25r <- left_join(fr_sr25,feature_sr25) %>%
  mutate(objective =   "Random",
         species = "25")
head(sr25r)


# RANDOM
# 30
pu_dat30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat30 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/30/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp30 <- problem(pu_dat30, spec_r30, cost_column = "cost", rij = puvsp_dat30) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r30$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs30 <- solve(rp30)


solution_col_names <-  rs30 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr30 <-  solution_col_names %>% 
  map( ~feature_representation(rp30, rs30[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr30_prep <- as.data.frame(rs30) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr30_s <- sr30_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr30_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "30") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr30 <- feature_abundances(rp30, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr30r <- left_join(fr_sr30,feature_sr30) %>%
  mutate(objective =   "Random",
         species = "30")
head(sr30r)


# RANDOM
# 35
pu_dat35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat35 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/35/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp35 <- problem(pu_dat35, spec_r35, cost_column = "cost", rij = puvsp_dat35) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r35$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs35 <- solve(rp35)


solution_col_names <-  rs35 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr35 <-  solution_col_names %>% 
  map( ~feature_representation(rp35, rs35[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr35_prep <- as.data.frame(rs35) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr35_s <- sr35_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr35_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "35") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr35 <- feature_abundances(rp35, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr35r <- left_join(fr_sr35,feature_sr35) %>%
  mutate(objective =   "Random",
         species = "35")
head(sr35r)



# RANDOM
# 40
pu_dat40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat40 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/40/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp40 <- problem(pu_dat40, spec_r40, cost_column = "cost", rij = puvsp_dat40) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r40$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs40 <- solve(rp40)


solution_col_names <-  rs40 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr40 <-  solution_col_names %>% 
  map( ~feature_representation(rp40, rs40[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr40_prep <- as.data.frame(rs40) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr40_s <- sr40_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr40_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "40") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr40 <- feature_abundances(rp40, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr40r <- left_join(fr_sr40,feature_sr40) %>%
  mutate(objective =   "Random",
         species = "40")
head(sr40r)


# RANDOM
# 45
pu_dat45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat45 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/45/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp45 <- problem(pu_dat45, spec_r45, cost_column = "cost", rij = puvsp_dat45) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r45$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs45 <- solve(rp45)


solution_col_names <-  rs45 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr45 <-  solution_col_names %>% 
  map( ~feature_representation(rp45, rs45[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr45_prep <- as.data.frame(rs45) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr45_s <- sr45_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr45_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "45") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr45 <- feature_abundances(rp45, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr45r <- left_join(fr_sr45,feature_sr45) %>%
  mutate(objective =   "Random",
         species = "45")
head(sr45r)


# RANDOM
# 50
pu_dat50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat50 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/50/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp50 <- problem(pu_dat50, spec_r50, cost_column = "cost", rij = puvsp_dat50) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r50$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs50 <- solve(rp50)


solution_col_names <-  rs50 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr50 <-  solution_col_names %>% 
  map( ~feature_representation(rp50, rs50[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr50_prep <- as.data.frame(rs50) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr50_s <- sr50_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr50_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "50") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr50 <- feature_abundances(rp50, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr50r <- left_join(fr_sr50,feature_sr50) %>%
  mutate(objective =   "Random",
         species = "50")
head(sr50r)



# RANDOM
# 55
pu_dat55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat55 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/55/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp55 <- problem(pu_dat55, spec_r55, cost_column = "cost", rij = puvsp_dat55) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r55$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs55 <- solve(rp55)


solution_col_names <-  rs55 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr55 <-  solution_col_names %>% 
  map( ~feature_representation(rp55, rs55[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr55_prep <- as.data.frame(rs55) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr55_s <- sr55_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr55_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "55") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr55 <- feature_abundances(rp55, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr55r <- left_join(fr_sr55,feature_sr55) %>%
  mutate(objective =   "Random",
         species = "55")
head(sr55r)


# RANDOM
# 60
pu_dat60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat60 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/60/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp60 <- problem(pu_dat60, spec_r60, cost_column = "cost", rij = puvsp_dat60) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r60$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs60 <- solve(rp60)


solution_col_names <-  rs60 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr60 <-  solution_col_names %>% 
  map( ~feature_representation(rp60, rs60[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr60_prep <- as.data.frame(rs60) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr60_s <- sr60_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr60_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "60") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr60 <- feature_abundances(rp60, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr60r <- left_join(fr_sr60,feature_sr60) %>%
  mutate(objective =   "Random",
         species = "60")
head(sr60r)


# RANDOM
# 65
pu_dat65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat65 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/65/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp65 <- problem(pu_dat65, spec_r65, cost_column = "cost", rij = puvsp_dat65) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r65$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs65 <- solve(rp65)


solution_col_names <-  rs65 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr65 <-  solution_col_names %>% 
  map( ~feature_representation(rp65, rs65[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr65_prep <- as.data.frame(rs65) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr65_s <- sr65_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr65_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "65") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr65 <- feature_abundances(rp65, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr65r <- left_join(fr_sr65,feature_sr65) %>%
  mutate(objective =   "Random",
         species = "65")
head(sr65r)



# RANDOM
# 70
pu_dat70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat70 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/70/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp70 <- problem(pu_dat70, spec_r70, cost_column = "cost", rij = puvsp_dat70) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r70$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs70 <- solve(rp70)


solution_col_names <-  rs70 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr70 <-  solution_col_names %>% 
  map( ~feature_representation(rp70, rs70[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr70_prep <- as.data.frame(rs70) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr70_s <- sr70_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr70_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "70") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr70 <- feature_abundances(rp70, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr70r <- left_join(fr_sr70,feature_sr70) %>%
  mutate(objective =   "Random",
         species = "70")
head(sr70r)


# RANDOM
# 75
pu_dat75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat75 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/75/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp75 <- problem(pu_dat75, spec_r75, cost_column = "cost", rij = puvsp_dat75) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r75$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs75 <- solve(rp75)


solution_col_names <-  rs75 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr75 <-  solution_col_names %>% 
  map( ~feature_representation(rp75, rs75[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr75_prep <- as.data.frame(rs75) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr75_s <- sr75_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr75_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "75") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr75 <- feature_abundances(rp75, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr75r <- left_join(fr_sr75,feature_sr75) %>%
  mutate(objective =   "Random",
         species = "75")
head(sr75r)

# RANDOM
# 80
pu_dat80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat80 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/80/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp80 <- problem(pu_dat80, spec_r80, cost_column = "cost", rij = puvsp_dat80) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r80$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs80 <- solve(rp80)


solution_col_names <-  rs80 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr80 <-  solution_col_names %>% 
  map( ~feature_representation(rp80, rs80[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr80_prep <- as.data.frame(rs80) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr80_s <- sr80_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr80_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "80") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr80 <- feature_abundances(rp80, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr80r <- left_join(fr_sr80,feature_sr80) %>%
  mutate(objective =   "Random",
         species = "80")
head(sr80r)


# RANDOM
# 85
pu_dat85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat85 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/85/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp85 <- problem(pu_dat85, spec_r85, cost_column = "cost", rij = puvsp_dat85) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r85$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs85 <- solve(rp85)


solution_col_names <-  rs85 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr85 <-  solution_col_names %>% 
  map( ~feature_representation(rp85, rs85[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr85_prep <- as.data.frame(rs85) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr85_s <- sr85_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr85_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "85") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr85 <- feature_abundances(rp85, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr85r <- left_join(fr_sr85,feature_sr85) %>%
  mutate(objective =   "Random",
         species = "85")
head(sr85r)


# RANDOM
# 90
pu_dat90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat90 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/90/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp90 <- problem(pu_dat90, spec_r90, cost_column = "cost", rij = puvsp_dat90) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r90$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs90 <- solve(rp90)


solution_col_names <-  rs90 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr90 <-  solution_col_names %>% 
  map( ~feature_representation(rp90, rs90[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr90_prep <- as.data.frame(rs90) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr90_s <- sr90_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr90_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "90") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr90 <- feature_abundances(rp90, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr90r <- left_join(fr_sr90,feature_sr90) %>%
  mutate(objective =   "Random",
         species = "90")
head(sr90r)


# RANDOM
# 95
pu_dat95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat95 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/95/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp95 <- problem(pu_dat95, spec_r95, cost_column = "cost", rij = puvsp_dat95) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r95$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs95 <- solve(rp95)


solution_col_names <-  rs95 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr95 <-  solution_col_names %>% 
  map( ~feature_representation(rp95, rs95[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr95_prep <- as.data.frame(rs95) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr95_s <- sr95_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr95_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "95") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr95 <- feature_abundances(rp95, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr95r <- left_join(fr_sr95,feature_sr95) %>%
  mutate(objective =   "Random",
         species = "95")
head(sr95r)


# RANDOM
# 100
pu_dat100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat100 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/100/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp100 <- problem(pu_dat100, spec_r100, cost_column = "cost", rij = puvsp_dat100) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r100$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs100 <- solve(rp100)


solution_col_names <-  rs100 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr100 <-  solution_col_names %>% 
  map( ~feature_representation(rp100, rs100[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr100_prep <- as.data.frame(rs100) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr100_s <- sr100_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr100_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "100") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr100 <- feature_abundances(rp100, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr100r <- left_join(fr_sr100,feature_sr100) %>%
  mutate(objective =   "Random",
         species = "100")
head(sr100r)


# RANDOM
# 105
pu_dat105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat105 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/105/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp105 <- problem(pu_dat105, spec_r105, cost_column = "cost", rij = puvsp_dat105) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r105$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs105 <- solve(rp105)


solution_col_names <-  rs105 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr105 <-  solution_col_names %>% 
  map( ~feature_representation(rp105, rs105[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr105_prep <- as.data.frame(rs105) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr105_s <- sr105_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr105_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "105") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr105 <- feature_abundances(rp105, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr105r <- left_join(fr_sr105,feature_sr105)%>%
  mutate(objective =   "Random",
         species = "105")
head(sr105r)



# RANDOM
# 110
pu_dat110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat110 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/110/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp110 <- problem(pu_dat110, spec_r110, cost_column = "cost", rij = puvsp_dat110) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r110$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs110 <- solve(rp110)


solution_col_names <-  rs110 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr110 <-  solution_col_names %>% 
  map( ~feature_representation(rp110, rs110[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr110_prep <- as.data.frame(rs110) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr110_s <- sr110_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr110_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "110") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr110 <- feature_abundances(rp110, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr110r <- left_join(fr_sr110,feature_sr110)%>%
  mutate(objective =   "Random",
         species = "110")
head(sr110r)


# RANDOM
# 115
pu_dat115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat115 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/115/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp115 <- problem(pu_dat115, spec_r115, cost_column = "cost", rij = puvsp_dat115) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r115$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs115 <- solve(rp115)


solution_col_names <-  rs115 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr115 <-  solution_col_names %>% 
  map( ~feature_representation(rp115, rs115[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr115_prep <- as.data.frame(rs115) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr115_s <- sr115_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr115_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "115") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr115 <- feature_abundances(rp115, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr115r <- left_join(fr_sr115,feature_sr115) %>%
  mutate(objective =   "Random",
         species = "115")
head(sr115r)


# RANDOM
# 120
pu_dat120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat120 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/120/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp120 <- problem(pu_dat120, spec_r120, cost_column = "cost", rij = puvsp_dat120) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r120$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs120 <- solve(rp120)


solution_col_names <-  rs120 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr120 <-  solution_col_names %>% 
  map( ~feature_representation(rp120, rs120[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr120_prep <- as.data.frame(rs120) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr120_s <- sr120_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr120_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "120") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr120 <- feature_abundances(rp120, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr120r <- left_join(fr_sr120,feature_sr120)%>%
  mutate(objective =   "Random",
         species = "120")
head(sr120r)


# RANDOM
# 125
pu_dat125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
spec_r125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
puvsp_dat125 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Random/125/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rp125 <- problem(pu_dat125, spec_r125, cost_column = "cost", rij = puvsp_dat125) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_r125$target) %>% # use targets column
  add_gurobi_solver(gap = 0) %>%  # optimality gap of zero to obtain the optimal solution
  add_gap_portfolio(number_solutions = 100) 

rs125 <- solve(rp125)


solution_col_names <-  rs125 %>% dplyr::select(contains("solution")) %>%
  colnames() 

# extract feature representation across all solutions
fr_sr125 <-  solution_col_names %>% 
  map( ~feature_representation(rp125, rs125[, .x, drop = FALSE])) %>%
  set_names(solution_col_names) %>% bind_rows(.id = 'solution')

# gather all solutions into a tidy data frame
sr125_prep <- as.data.frame(rs125) %>% group_by(s) %>%
  gather(solution_n, solution, solution_1:solution_100)

# name the objective, and the number of planning units
sr125_s <- sr125_prep %>% group_by(s) %>%
  dplyr::summarise(sf = sum(solution)) %>% left_join(sr125_prep) %>%
  mutate(objective =   "Random",
         solution_amount = "125") %>% group_by(solution_n) %>%
  dplyr::select(-X) %>% arrange(solution_n,s)

# pull out feature abundance
feature_sr125 <- feature_abundances(rp125, na.rm = FALSE)

# join feature representation in solution and total feature abundance
sr125r <- left_join(fr_sr125,feature_sr125) %>%
  mutate(objective =   "Random",
         species = "125") 
head(sr125r)


# _________________________________________________________
# ########################################################
# ############# WRANGLE  SOLUTION DATA ###################
# ########################################################
# _________________________________________________________

# Objective Features
objective.features <- bind_rows(compr,bflyr,birdr,domr,prichfr,prichgr,pwfr,pwgr)
head(objective.features)
write.csv(objective.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/objective.features.csv")

# Random Features 
random.features <- bind_rows(sr5r,sr10r,sr15r,sr20r,sr25r,sr30r,sr35r,sr40r,sr45r,sr50r,sr55r,sr60r,sr65r,sr70r,sr75r,sr80r,sr85r,sr90r,sr95r,sr100r,sr105r,sr110r,sr115r,sr120r,sr125r)

head(random.features)

write.csv(random.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random.features.csv")

# objective features +  random features = master features
master.features <- bind_rows(objective.features,random.features)
head(master.features)

dmf <- distinct(master.features,objective, species)
dmf

write.csv(master.features,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master.features.csv")


#  Data for Figure 1
random_dat <- bind_rows(sr5_s,sr10_s,sr15_s,sr20_s,sr25_s,sr30_s,sr35_s,sr40_s,sr45_s,sr50_s,sr55_s,sr60_s,sr65_s,sr70_s,sr75_s,sr80_s,sr85_s,sr90_s,sr95_s,sr100_s,sr105_s,sr110_s,sr115_s,sr120_s,sr125_s)

head(random_dat)

write.csv(random_dat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_dat.csv")

objective_dat <- bind_rows(comp_s,bfly_s,bird_s,dom_s,prichf_s,prichg_s,pwf_s,pwg_s) 

head(objective_dat)

write.csv(objective_dat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/solution_dat.csv")



# Figure 4 & Figure S2
master.features <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/master.features.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
head(master.features)

distinct(master.features,objective, species)

master.features <- master.features %>% 
  mutate( percent_held = (relative_held * 100),
          name = feature) %>% dplyr::select(- c("feature", "X"))

head(master.features)

ob.feats <-  master.features %>%  filter( !objective == "Random") %>%
 left_join( bind_rows(comp_spec,bfly_spec,bird_spec,dom_spec,prichf_spec,prichg_spec,pwf_spec,pwg_spec) 
 ) %>%
  mutate( pres = ifelse(ob.feats$absolute_held == 0, '0',
                        ifelse(ob.feats$absolute_held >= 1 , '1', 'other')),
          value = "1"
          )

head(ob.feats)

write.csv(ob.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/objective_features2.csv")

ob.feats$pres<-as.numeric(ob.feats$pres)
ob.feats$value<-as.numeric(ob.feats$value)
head(ob.feats)

summ.feats<- ob.feats %>% group_by(category, objective, solution, species) %>%
  summarise(absolute_held = sum(absolute_held), 
            absolute_abundance=sum(absolute_abundance), 
            target=sum(target), pres=sum(pres), value=sum(value) ) %>%
  mutate(percent_targs = ( (pres/value)*100) ) %>% 
  filter(category != "Dominants" ) %>% droplevels()

summ.feats$category <-as.factor(as.character(summ.feats$category))
levels(summ.feats$category)

write.csv(summ.feats,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/summary_objective_features.csv")


# Random
random.feat <- master.features %>% filter( objective == "Random") %>%
  left_join( bind_rows(spec_r5,spec_r10,spec_r15,spec_r20,spec_r25,spec_r30,spec_r35,spec_r40,spec_r45,spec_r50,spec_r55,spec_r60,spec_r65,spec_r70,spec_r75,spec_r80,spec_r85,spec_r90,spec_r95,spec_r100,spec_r105,spec_r110,spec_r115,spec_r120,spec_r125)
 ) %>%
  mutate ( name = fct_recode( name, Species = "Random") ,
           pres = ifelse(random.feats$absolute_held ==0, '0',
                            ifelse(random.feats$absolute_held >=1, '1', 'other')),
           value = "1")

head(random.feat)
write.csv(random.feat,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/random_features2.csv")

random.feat$pres<-as.numeric(random.feat$pres)
random.feat$value<-as.numeric(random.feat$value)

random.summ.feats<- random.feat %>% group_by(category, objective, solution, species) %>%
  summarise(absolute_held= sum(absolute_held),
            absolute_abundance=sum(absolute_abundance),
            target=sum(target),pres=sum(pres), 
            value=sum(value)) %>%
  mutate( percent_targs = ((pres/value)*100)  )

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
  filter(solution == "solution_1") %>%
  filter(objective == "Bird (n=5)"  | objective == "Butterfly (n=35)"  | 
           objective == "Comprehensive (n=37)" | 
           objective == "Pairwise Butterfly Div Family (n=47)" | objective == "Pairwise Butterfly Div Genus (n=119)") %>%
  dplyr::select(s,objective,selection)  %>% spread(objective, selection)

View(s_dat)

master_dat3 <- master_dat2 %>% left_join(s_dat, by =("s"))

colnames(master_dat3)


write.csv(master_dat3, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/3_Table S1_new.csv")



