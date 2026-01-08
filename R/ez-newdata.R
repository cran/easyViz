# ez-newdata.R

ez_build_newdata <- function(prep,
                             pred_resolution,
                             pred_range_limit,
                             pred_range_limit_user,
                             by_breaks = NULL) {
  data       <- prep$data
  predictor  <- prep$predictor
  by         <- prep$by
  num_values <- prep$num_values
  cat_values <- prep$cat_values
  fix_values <- prep$fix_values
  offset_vars <- prep$offset_vars
  should_scale <- prep$should_scale
  predictor_is_numeric <- prep$predictor_is_numeric
  
  new_data <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(new_data) <- colnames(data)
  by_levels <- NULL
  
  if (!is.null(by)) {
    # Determine levels/values of the 'by' variable
    if (is.factor(prep$data[[by]])) {
      
      # Case 1 — categorical by
      by_levels <- levels(prep$data[[by]])

    } else if (is.numeric(prep$data[[by]])) {
      
      xby <- prep$data[[by]]
      uby <- sort(unique(xby))

      # Case 2 — numeric by with user-specified breaks
      if (!is.null(by_breaks)) {
        
        by_levels <- sort(by_breaks)

      } else {
        
        # Treat "binary/discrete numeric" as categorical:
        # - protects 0/1, 1/2, small sets, etc.
        if (length(uby) <= 6) {
          by_levels <- uby
        } else {
          # continuous-ish numeric: use quantiles (unique to avoid duplicates)
          q <- as.numeric(quantile(xby, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))
          
          xby_nn <- xby[!is.na(xby)]
          is_integer_like <- all(abs(xby_nn - round(xby_nn)) < sqrt(.Machine$double.eps))
          
          if (!is_integer_like) {
            q_u  <- sort(unique(q))
            q3u <- sort(unique(signif(q, digits = 3)))
            
            # if rounding collapsed values too much, use a bit more precision
            if (length(q3u) < length(q_u)) {
              by_levels <- sort(unique(signif(q, digits = 4)))
            } else {
              by_levels <- q3u
            }
          } else {
            by_levels <- sort(unique(q))
          }
        }
      }
      
    } else {
      
      # Fallback for non-numeric/non-factor (rare)
      by_levels <- unique(prep$data[[by]])
    }
    
    # pred_range_limit only applies when 'by' is a categorical grouping variable (factor)
    if (!is.factor(prep$data[[by]])) {
      if (pred_range_limit_user && isTRUE(pred_range_limit)) {
        message(
          "Note: `pred_range_limit = TRUE` is only applied when `by` is a factor.
Using full range instead."
        )
      }
      pred_range_limit <- FALSE
    }
    
    # If `by` is not NULL, vary the predictor within each level of `by`...
    # First build a data frame (`new_data`) that contains all combinations of the predictor and `by` levels
    if (predictor_is_numeric) {
      # Restrict prediction range to observed range within each `by` level (if pred_range_limit = TRUE)
      if (pred_range_limit && is.factor(prep$data[[by]])) {
        new_data_list <- lapply(by_levels, function(by_level) {
          subset_data <- prep$data[prep$data[[by]] == by_level, ]
          range_x <- range(subset_data[[predictor]], na.rm = TRUE)
          seq_values <- seq(from = range_x[1], to = range_x[2], length.out = pred_resolution)
          df <- data.frame(
            predictor = seq_values,
            by = rep(by_level, length(seq_values))
          )
          names(df) <- c(predictor, by)
          # keep 'by' as factor (important for predict() / contrasts)
          if (is.factor(data[[by]])) {
            df[[by]] <- factor(df[[by]], levels = levels(data[[by]]))
          }
          df
        })
        new_data <- do.call(rbind, new_data_list)
      } else {
        # Use full prediction range (across all data)
        seq_values <- seq(min(data[[predictor]], na.rm = TRUE), 
                          max(data[[predictor]], na.rm = TRUE), 
                          length.out = pred_resolution)
        new_data <- expand.grid(
          tmp_x  = seq_values,
          tmp_by = by_levels,
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
        names(new_data) <- c(predictor, by)
        
        # enforce factor type for 'by' if training data used a factor
        if (is.factor(data[[by]])) {
          new_data[[by]] <- factor(new_data[[by]], levels = levels(data[[by]]))
        }
      }
    } else { # categorical predictor
      levels_values <- levels(data[[predictor]])
      new_data <- expand.grid(
        tmp_x  = levels_values,
        tmp_by = by_levels,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      names(new_data) <- c(predictor, by)
      
      # enforce factor types to match training data
      new_data[[predictor]] <- factor(new_data[[predictor]], levels = levels(data[[predictor]]))
      if (is.factor(data[[by]])) {
        new_data[[by]] <- factor(new_data[[by]], levels = levels(data[[by]]))
      }
    }
    
    # ...then add fixed values for all other explanatory variables (those not in predictor or by)
    for (col_name in colnames(data)) {
      if (col_name != predictor && col_name != by) {
        if (is.numeric(data[[col_name]])) { # numeric variables
          new_data[[col_name]] <- num_values[col_name]
        } else if (is.factor(data[[col_name]])) { # categorical variables
          new_data[[col_name]] <- factor(cat_values[col_name], levels = levels(data[[col_name]]))
        }
      }
    }
    
  } else {
    # If `by` is NULL, vary only the predictor...
    # (first create a data frame with one row per predictor value (or level))
    if (predictor_is_numeric) { # numeric predictor
      seq_values <- seq(min(data[[predictor]], na.rm = TRUE),
                        max(data[[predictor]], na.rm = TRUE),
                        length.out = pred_resolution)
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(seq_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- seq_values
    } else { # categorical predictor
      levels_values <- levels(data[[predictor]])
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(levels_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- factor(levels_values, levels = levels(data[[predictor]]))
    }
    # ...then add fixed values for all other explanatory variables
    for (col_name in colnames(data)) {
      if (col_name != predictor) {
        if (is.numeric(data[[col_name]])) {
          new_data[[col_name]] <- num_values[col_name]
        } else if (is.factor(data[[col_name]])) {
          new_data[[col_name]] <- factor(cat_values[col_name], levels = levels(data[[col_name]]))
        }
      }
    }
  }
  
  # Fix values or levels if specified
  if (!is.null(fix_values)) {
    # Ensure fix_values is named
    if (is.null(names(fix_values))) {
      stop("fix_values must be a named vector or list, e.g., 
fix_values = c(x = 1, group = 'A') 
or fix_values = list(x = 1, group = 'A')")
    }
    # Loop through each specified variable in fix_values
    for (var_name in names(fix_values)) {
      if (var_name %in% colnames(new_data)) {
        # Fix the variable to the specified value
        value <- fix_values[[var_name]]
        if (is.factor(data[[var_name]])) {
          # Ensure value is coerced to the correct factor level
          if (!(value %in% levels(data[[var_name]]))) {
            stop(sprintf("Level '%s' not found in factor variable '%s'", value, var_name))
          }
          new_data[[var_name]] <- factor(value, levels = levels(data[[var_name]]))
        } else {
          new_data[[var_name]] <- value
        }
      } else {
        warning("fix_values variable '", var_name,
                "' not found in prediction grid; ignoring.",
                call. = FALSE)
      }
    }
  }
  
  # Automatic offset fixing for count models
  if (!is.null(offset_vars) && should_scale && prep$offset_is_log && !prep$offset_external) {
    
    if (is.null(fix_values) || !(offset_vars[1] %in% names(fix_values))) {
      
      # Force offset in new_data to 1 for prediction (rate scale)
      new_data[[offset_vars[1]]] <- 1
      message(
        sprintf("Looks like you are modeling a rate. 
Setting exposure '%s' to 1 so predictions are on the unit-rate scale. 
To obtain predictions on a different rate scale, specify the argument 
fix_values = c(%s = ...).",
                offset_vars[1],
                offset_vars[1])
      )
    }
    
  } else if (!is.null(offset_vars) && should_scale && !prep$offset_is_log && !prep$offset_external) {
    # Show the warning only if it wasn't already shown in ez-prepare-data()
    if (!isTRUE(prep$offset_warning_issued)) {
      message("Note: the offset is not of the form log(exposure).
easyViz will not fix the exposure to 1 in predictions.
For full control over scaling and exposure values, specify the offset as 
log(exposure), using the exposure variable untransformed.")
    }
  }
  
  list(
    new_data = new_data,
    by_levels = by_levels,
    pred_range_limit = pred_range_limit
  )
}