# ez-build-output.R

ez_build_output <- function(model,
                            new_data,
                            preds,
                            ci_level,
                            re_form) {
  # Build a data frame with predicted values and 95 CIs (preds_df) cleanly with relevant predictors only
  # Extract variables from formula
  used_terms <- all.vars(formula(model))
  used_terms <- union(used_terms, ez_get_offset_vars(model))
  # Remove all components of response
  lhs_expr   <- formula(model)[[2]]
  response_vars <- all.vars(lhs_expr)
  # Assemble final variable list
  vars_to_include <- setdiff(used_terms, response_vars)
  
  # Build pred.df
  ci_label_lower <- paste0(ci_level * 100, "lcl")
  ci_label_upper <- paste0(ci_level * 100, "ucl")
  
  pred.df <- cbind(
    new_data[, intersect(vars_to_include, names(new_data)), drop = FALSE],
    fit = preds$fit
  )
  pred.df[[ci_label_lower]] <- preds$lower
  pred.df[[ci_label_upper]] <- preds$upper
  
  # warning attribute if random effects were excluded
  exclude_re <- identical(re_form, NA) ||
    (inherits(re_form, "formula") &&
       any(trimws(paste(deparse(re_form), collapse = " ")) %in% c("~0", "~ 0"))
    )
  
  if (exclude_re) {
    attr(pred.df, "re_form_warning") <- 
"When re_form = NA or ~0, random effects are ignored in predictions."
  }
  
  # Assign custom class to trigger print method
  class(pred.df) <- c("easyviz.pred.df", class(pred.df))
  pred.df
}