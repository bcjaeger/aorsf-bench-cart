
source("packages.R")
source("conflicts.R")

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 10),
  workspace_on_error = TRUE
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data_meta <-
tibble::tribble(
              ~name,        ~pkg,            ~mode,               ~outcome,
            "meats", "modeldata",     "regression",              "protein",
  #            "ames", "modeldata",     "regression",           "Sale_Price",
  #         "Chicago", "modeldata",     "regression",            "ridership",
  #         "biomass", "modeldata",     "regression",                  "HHV",
  #      "car_prices", "modeldata",     "regression",                "Price",
  # "chem_proc_yield", "modeldata",     "regression",                "yield",
         "concrete", "modeldata",     "regression", "compressive_strength",
  "ischemic_stroke", "modeldata", "classification",               "stroke",
    #     "ad_data", "modeldata", "classification",                "Class",
    #   "attrition", "modeldata", "classification",            "Attrition",
    #       "cells", "modeldata", "classification",                "class",
    # "credit_data", "modeldata", "classification",               "Status"
    # "grants_other", "modeldata", "classification",                "class"
  )

tar_data <- tar_map(
  values = data_meta,
  names = name,

  tar_target(guide, command = {
    list(
      data = initialize_data(name, pkg),
      formula = initialize_formula(outcome),
      metrics = initialize_metrics(mode)
    )
  }),

  tar_target(models, command = {
    initialize_models(guide, outcome, mode) %>%
      expand_grid(tibble(vs_threshold = c(0, 2/5, 4/5))) %>%
      mutate(
        e_predictors = case_when(
          vs_threshold == 0 ~ n_predictors,
          TRUE ~ round(n_predictors * (1-vs_threshold))
        )
      ) %>%
      filter(e_predictors > 1)
  }),

  tar_target(resamples, nested_cv(guide$data,
                                  outside = vfold_cv(v = 5),
                                  inside = vfold_cv(v = 5))),


  tar_target(nested_cv_result,
             pattern = cross(models, resamples),
             tidy_eval = FALSE,
             command = {

               # guide         <- tar_read(guide_ischemic_stroke)
               # models        <- tar_read(models_ischemic_stroke)[1, ]
               # resamples     <- tar_read(resamples_ischemic_stroke)[1, ]

               preprocessor <- guide$data %>%
                 recipe(formula = guide$formula) %>%
                 step_impute_mean(all_numeric_predictors()) %>%
                 step_impute_mode(all_nominal_predictors()) %>%
                 step_dummy(all_nominal_predictors()) %>%
                 step_nzv(all_predictors())


               if(models$vs_threshold > 0){

                 preprocessor %<>%
                   step_select_forests(all_predictors(),
                                       outcome = outcome,
                                       threshold = models$vs_threshold)

               }

               time_tune_start <- Sys.time()

               tune_object <- tune_grid(
                 models$spec[[1]],
                 grid = models$grid[[1]],
                 preprocessor = preprocessor,
                 resamples = resamples$inner_resamples[[1]],
                 metrics = guide$metrics,
                 control = control_grid(save_workflow = TRUE)
               )

               time_tune_stop <- Sys.time()

               score_inner <- collect_metrics(tune_object) %>%
                 arrange(desc(mean)) %>%
                 dplyr::slice(1) %>%
                 pull(mean)


               time_fit_start <- Sys.time()

               fit_final <- fit_best(x = tune_object)

               time_fit_stop <- Sys.time()

               pred_col <- infer_pred_col(guide, mode, outcome)

               test <- testing(resamples$splits[[1]])

               pred <- predict(fit_final,
                               new_data = test,
                               type = infer_pred_type(mode))

               pred_final <- tibble(.pred = pred[[pred_col]],
                                    .outcome = test[[outcome]])

               guide$metrics(pred_final, truth = .outcome, .pred) %>%
                 rename(score_outer = .estimate) %>%
                 mutate(model_id = models$model_id[1],
                        fold_id = resamples$id,
                        vs_threshold = models$vs_threshold,
                        score_inner = score_inner,
                        time_tune = difftime(time_tune_stop,
                                             time_tune_start,
                                             units = 's'),
                        time_fit = difftime(time_fit_stop,
                                            time_fit_start,
                                            units = 's'))


             })

)


tar_scores <- tar_combine(
  scores,
  tar_data$nested_cv_result,
  command = bind_rows(!!!.x, .id = 'data') %>%
    mutate(data = str_remove(data, '^nested_cv_result_')) %>%
    separate(model_id, into = c("engine", ".iter")) %>%
    select(-starts_with(".")) %>%
    group_by(data, engine, fold_id) %>%
    arrange(desc(score_inner)) %>%
    slice(1) %>%
    ungroup()
)

tar_scores_summary <- tar_target(
  scores_summary,
  command = {
    scores %>%
      group_by(data, engine) %>%
      summarize(across(starts_with("score"), ~ mean(.x)))
  }
)

tar_ranks <- tar_target(
  ranks,
  command = {
    scores_summary %>%
      arrange(data, desc(score_outer)) %>%
      mutate(rank = seq(n())) %>%
      group_by(engine) %>%
      summarize(rank = mean(rank))
  }
)



list(
  tar_data,
  tar_scores,
  tar_scores_summary,
  tar_ranks
) %>%
  tar_hook_before(
    hook = {source("conflicts.R")},
    names = everything()
  )

