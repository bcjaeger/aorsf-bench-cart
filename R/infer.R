
infer_pred_col <- function(guide, mode, outcome) {

  y <- guide$data[[outcome]]

  switch(
    mode,
    "classification" = paste(".pred", levels(y)[1], sep = '_'),
    "regression" = ".pred",
    "censored regression" = ".pred"
  )

}

infer_pred_type <- function(mode){
  switch(
    mode,
    "classification" = "prob",
    "regression" = "numeric",
    "censored regression" = "survival"
  )
}
