
initialize_formula <- function(outcome) {

  as.formula(paste(outcome, "~ ."))

}

initialize_metrics <- function(mode) {

  switch(
    mode,
    "classification" = metric_set(roc_auc),
    "regression" = metric_set(rsq),
    "censored regression" = metric_set(concordance_survival)
  )

}

initialize_data <- function(name, pkg) {

  data <- eval(parse(text = paste(pkg, name, sep = "::")))

  if(name == "Chicago") data %<>% select(-date)

  if(name == "ames") data %<>% mutate(Sale_Price = log(Sale_Price))

  if(name == "biomass") data %<>% select(-sample, -dataset)

  if(name == "car_prices") data %<>% mutate(Price = log(Price))

  if(name == "cells") data %<>% select(-case)

  data

}

# guide <- tar_read(guide_ames)
# outcome = "Sale_Price"
# mode = "regression"

initialize_models <- function(guide, outcome, mode){

  n_predictors <- guide$data %>%
    select(-all_of(outcome)) %>%
    ncol()

  # finalize() gives a max mtry that is too high
  max_mtry <- min(ceiling(sqrt(n_predictors) * 2), n_predictors)

  model_specs <- list(

    aorsf = rand_forest(trees = 1000,
                        mtry = tune(),
                        min_n = tune()) %>%
      set_engine('aorsf'),

    ranger = rand_forest(trees = 1000,
                         mtry = tune(),
                         min_n = tune()) %>%
      set_engine('ranger'),

    glmnet = switch(
      mode,
      "classification" = logistic_reg(penalty = tune(),
                                      mixture = tune()),
      "regression" = linear_reg(penalty = tune(),
                                mixture = tune()),
      "censored regression" = proportional_hazards(penalty = tune(),
                                                   mixture = tune())

    ) %>%
      set_engine("glmnet"),

    xgboost = boost_tree(trees = tune(),
                         tree_depth = tune(),
                         learn_rate = 0.01) %>%
      set_engine("xgboost")

  ) %>%
    map(set_mode, mode) %>%
    enframe(name = 'model_id', value = 'spec')

  model_grids <- list(

    glmnet = grid_regular(penalty(range = c(-3, 0)), levels = 20) %>%
      cross_join(grid_regular(mixture(), levels = 3)),

    aorsf = grid_regular(mtry(range = c(2, max_mtry)),
                         min_n(),
                         levels = 5),

    ranger = grid_regular(mtry(range = c(2, max_mtry)),
                          min_n(),
                          levels = 5),

    xgboost = grid_regular(trees(range = c(25, 500)), levels = 20) %>%
      cross_join(grid_regular(tree_depth(range = c(2, 4)), levels = 3))

  ) %>%
    enframe(name = 'model_id', value = 'grid')

  left_join(model_grids, model_specs) %>%
    mutate(n_predictors = n_predictors)

}
