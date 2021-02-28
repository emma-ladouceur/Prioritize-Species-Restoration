# Initialization
## load packages
library(prioritizr)
library(dplyr)

## specify file paths
rda_path <- "emmas-prioritizr-data.rda"

pu_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/pu.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# feature data
spec_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/spec.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

puvsp_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/Marxan/Data/Comprehensive/puvsp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# Preliminary processing
## load data
load(rda_path)

# Main processing
## make problem
p <-
  problem(pu_dat, spec_dat, cost_column = "cost", rij = puvsp_dat) %>%
  add_min_set_objective() %>% # Minimize the cost, targets are met
  add_absolute_targets(spec_dat$target) %>% # use targets column
  add_gurobi_solver(gap = 0)  %>% # optimality gap of zero
  add_gap_portfolio(number_solutions = 100)

## generation solutions
s <- solve(p)

## calculate feature representation
solution_col_names <- grep("solution", names(s), value = TRUE)
fr_all <- lapply(solution_col_names, function(x) {
  feature_representation(p, s[, x, drop = FALSE])
})


View(fr_all)
## extract relative held for each feature in each solution
fr_relative_data <-
  lapply(fr_all, `[[`, "relative_held") %>%
  as.data.frame() %>%
  setNames(solution_col_names) %>%
  dplyr::bind_cols(
    fr_all[[1]] %>%
    dplyr::select(feature)) %>%
  dplyr::select(feature, dplyr::everything()) %>%
  tibble::as_tibble()


View(fr_relative_data)


