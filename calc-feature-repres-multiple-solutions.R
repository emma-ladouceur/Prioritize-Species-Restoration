# set seed for reproducibility
set.seed(600)

# load packages
library(prioritizr)
library(dplyr)

# load data
data(sim_pu_raster, sim_features)

# create minimal problem with a portfolio containing 10 solutions within 20%
# of optimality
p1 <- problem(sim_pu_sf, sim_features, cost_column = "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.05) %>%
      add_gap_portfolio(number_solutions = 5, pool_gap = 0.2) %>%
      add_default_solver(gap = 0, verbose = FALSE)

# solve problem and generate portfolio
s1 <- solve(p1)

# print preview of result
print(s1)

# calculate feature representation for first solution
# Note that I am using the developmental version of prioritizr on GitHub,
# in this version the "feature_representation" function has been renamed
# to be called the "eval_feature_representation_summary" function.
# This change was for consistency with the other new functions being added
# to prioritizr
fr1 <- feature_representation(p1, s1[, "solution_1", drop = FALSE])

# preview feature representation for the first solution
print(fr1)


View(p1)
# now let's calculate feature representation for all solutions
## extract column names containing solutions in s1
solution_col_names <- grep("solution", names(s1), value = TRUE)
print(solution_col_names)

## calculate feature representation for each solution
# Note that I am using the developmental version of prioritizr on GitHub,
# in this version the "feature_representation" function has been renamed
# to be called the "eval_feature_representation_summary" function.
# This change was for consistency with the other new functions being added
# to prioritizr
fr_all <- lapply(solution_col_names, function(x) {
  feature_representation(p1, s1[, x])
})

# Now let's create a table containing the absolute amount held for
# each feature in each solution. In this table, each row corresponds
# to a different feature and each column corresponds to a different
# solution
## make table
fr_absolute_data <-
  lapply(fr_all, `[[`, "absolute_held") %>%
  as.data.frame() %>%
  setNames(solution_col_names) %>%
  dplyr::bind_cols(
    fr_all[[1]] %>%
    dplyr::select(summary, feature)) %>%
  dplyr::select(summary, feature, dplyr::everything()) %>%
  tibble::as_tibble()

## print table
print(fr_absolute_data)

# Now let's create a table containing the relative amount held for
# each feature in each solution. In this table, each row corresponds
# to a different feature and each column corresponds to a different
# solution
## make table
fr_relative_data <-
  lapply(fr_all, `[[`, "relative_held") %>%
  as.data.frame() %>%
  setNames(solution_col_names) %>%
  dplyr::bind_cols(
    fr_all[[1]] %>%
    dplyr::select(summary, feature)) %>%
  dplyr::select(summary, feature, dplyr::everything()) %>%
  tibble::as_tibble()


## print table
print(fr_relative_data)
