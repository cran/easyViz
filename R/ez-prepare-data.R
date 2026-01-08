# ez-prepare-data.R

ez_prepare_data <- function(model,
                            data,
                            predictor,
                            by,
                            by_breaks = NULL, 
                            num_conditioning,
                            cat_conditioning,
                            fix_values,
                            show_data_points,
                            point_col,
                            point_pch,
                            point_cex) {
  # Store original
  original_data <- data
  
  # --- Response extraction ---
  lhs_expr <- formula(model)[[2]] # Get the left-hand-side of the model formula
  unwrapped <- ez_unwrap_to_meaningful_expr(lhs_expr) # Remove outermost wrappers to simplify further analysis 
  
  # Warn for use of $ in the response variable
  if (ez_warn_on_env_prefix(lhs_expr)) {
    message("Careful! Using `$` in model formulas can produce unexpected results. 
It is safer to specify the model using the `data` argument instead.")
  }
  
  # Handle unwrapped response if it is a function call
  if (inherits(unwrapped, "call")) {
    fun_name <- as.character(unwrapped[[1]])
    
    # handle cbind(successes, failures) response (binomial proportions)
    if (fun_name == "cbind" && length(unwrapped) == 3) {
      successes_vals <- try(eval(unwrapped[[2]], envir = data), silent = TRUE)
      failures_vals  <- try(eval(unwrapped[[3]], envir = data), silent = TRUE)
      if (inherits(successes_vals, "try-error") || inherits(failures_vals, "try-error")) {
        stop("Could not evaluate cbind(successes, failures) response.")
      }
      prop_vals <- successes_vals / (successes_vals + failures_vals)
      response_name <- "Predicted proportion"
      while (response_name %in% names(data)) response_name <- paste0(response_name, "_")
      data[[response_name]] <- prop_vals
      response <- response_name
      if (show_data_points) {
        message("Detected cbind(successes, failures) response. Plotting success proportions.")
      }
      
      # handle successes / trials response (binomial proportions)
    } else if (fun_name == "/" && length(unwrapped) == 3) {
      successes_vals <- try(eval(unwrapped[[2]], envir = data), silent = TRUE)
      trials_vals    <- try(eval(unwrapped[[3]], envir = data), silent = TRUE)
      if (inherits(successes_vals, "try-error") || inherits(trials_vals, "try-error")) {
        stop("Could not evaluate successes / trials response.")
      }
      prop_vals <- successes_vals / trials_vals
      response_name <- "Predicted proportion"
      while (response_name %in% names(data)) response_name <- paste0(response_name, "_")
      data[[response_name]] <- prop_vals
      response <- response_name
      
      # Display message only if denominator is not a single constant scalar (avoid message when using, e.g., y/10)
      if (!(is.numeric(trials_vals) && length(trials_vals) == 1)) { 
        if (show_data_points) {
          message("Detected successes / trials response. Plotting success proportions.")
        }
      }
    } else { # fallback for any other call, e.g., log(y), sqrt(y), y^2, df$y, etc.
      response <- ez_extract_response_name(unwrapped)
    }
  } else { # fallback for non-calls, e.g., y
    response <- ez_extract_response_name(unwrapped)
  }
  
  # Convert binary response to numeric (0/1) if it's a factor
  if (is.factor(data[[response]]) && length(levels(data[[response]])) == 2) {
    data[[response]] <- as.numeric(data[[response]]) - 1
  }
  
  # --- Variables in formula, offsets, fix_values, by ---
  formula_vars <- ez_extract_clean_formula_vars(formula(model))

  # --- OFFSETS: detect variable names AND whether the offset is a log() call ---
  # Capture offset variables if specified inside the formula or outside via offset =
  # Initialize
  offset_warning_issued <- FALSE
  offset_is_log <- FALSE
  offset_exprs <- list()
  offset_vars <- character(0)

  # 1. Offsets inside formula (if model has terms)
  if (!inherits(model, "nls")) {
    term_obj <- terms(model)
    off_idx <- attr(term_obj, "offset")
    if (!is.null(off_idx) && length(off_idx) > 0) {
      vars <- attr(term_obj, "variables")
      for (i in off_idx) {
        this_expr <- vars[[i + 1]]
        offset_exprs[[length(offset_exprs) + 1]] <- this_expr
        offset_vars <- c(offset_vars, all.vars(this_expr))
      }
    }
  }
  
  # 2. Offsets supplied via offset = argument
  model_call <- tryCatch(getCall(model), error = function(e) NULL)
  if (!is.null(model_call) && "offset" %in% names(model_call)) {
    offset_exprs[[length(offset_exprs) + 1]] <- model_call$offset
    offset_vars <- c(offset_vars, all.vars(model_call$offset))
  }
  
  offset_vars <- unique(offset_vars)
  if (length(offset_vars) == 0) offset_vars <- NULL
  
  # Detect whether the offset is of the form log(exposure)
  offset_is_log <- FALSE
  
  if (length(offset_exprs) > 0) {
    for (expr in offset_exprs) {
      if (is.call(expr)) {
        fun_name <- as.character(expr[[1]])
        
        # Case 1: offset = log(exposure)
        if (fun_name == "log") {
          offset_is_log <- TRUE
          
          # Case 2: offset(log(exposure)) inside formula
        } else if (fun_name == "offset" && length(expr) >= 2 && is.call(expr[[2]])) {
          inner <- expr[[2]]
          inner_fun <- as.character(inner[[1]])
          if (inner_fun == "log") {
            offset_is_log <- TRUE
          }
        }
      }
    }
  }
  
  # Safely gather fix_values and by variables
  fix_vars <- if (!is.null(fix_values)) names(fix_values) else NULL
  by_var <- if (!is.null(by)) by else NULL
  
  # Combine all variables that must be present and complete
  model_vars <- unique(c(formula_vars, offset_vars, fix_vars, by_var))
  if (inherits(model, "nls")) { # For nls, exclude parameters from model_vars
    model_vars <- setdiff(model_vars, names(model$m$getPars()))
  }
  
  # Ensure all these are present in the data
  missing_in_data <- setdiff(model_vars, names(data))
  if (length(missing_in_data) > 0) {
    stop("The following required variables are missing from the data: ", 
         paste(missing_in_data, collapse = ", "))
  }
  
  # NA filtering
  data <- data[complete.cases(data[, model_vars, drop = FALSE]), ]
  # Save number of rows in cleaned data for aesthetic checks
  n_clean <- nrow(data)
  n_original <- nrow(original_data)
  
  # Aesthetic length checks
  ez_check_aesthetic_length(point_col, "point_col", n_clean, n_original, model_vars)
  ez_check_aesthetic_length(point_cex, "point_cex", n_clean, n_original, model_vars)
  ez_check_aesthetic_length(point_pch, "point_pch", n_clean, n_original, model_vars)
  
  # Fix_values: warn if variable is not in formula or offset
  if (!is.null(fix_values)) {
    for (var_name in names(fix_values)) {
      if (!(var_name %in% c(formula_vars, offset_vars))) {
        message(sprintf(
          "Heads up! Variable '%s' in fix_values is not used in the model formula. 
Fixing it will have no effect on predictions.
Consider removing it from your easyViz() call.",
          var_name
        ))
      }
    }
  }
  
  # Check for contradiction: predictor also listed in fix_values?
  if (!is.null(fix_values) && predictor %in% names(fix_values)) {
    stop("The variable specified in 'predictor' is also included in 'fix_values'. 
This is contradictory. Remove it from 'fix_values' to allow it to vary.")
  }
  
  # by not in model?
  if (!is.null(by) && !(by %in% c(formula_vars, offset_vars))) {
    message(sprintf("Heads up! The 'by' variable '%s' is not used in the model formula. 
Grouping by it will not affect predictions. 
Consider removing it from your easyViz() call.",
      by
    ))
  }
  
  # Convert all character columns to factors
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], as.factor)
  
  # Check if the predictor is in the dataframe
  if (!(predictor %in% colnames(data))) stop(paste("Predictor", predictor, "not found in data"))
  # Check if by is a valid column in the dataframe
  if (!is.null(by) && !(by %in% colnames(data))) stop(paste("Column", by, "not found in data"))
  
  # Model type, family, link
  model_type <- ez_detect_model_type(model)
  fam  <- ez_get_family_name(model)
  link <- ez_get_link(model)
  
  # --- COXPH special handling (Surv() is not a plottable response column) ---
  if (isTRUE(model_type$is_coxph)) {
    
    # We do not (yet) plot raw data points for coxph
    if (isTRUE(show_data_points)) {
      message("Detected a coxph model. Raw data points are not plotted for survival models.")
    }
    show_data_points <- FALSE
    
    # Placeholder response column to keep downstream plotting code stable
    response_name <- "coxph_response_placeholder"
    while (response_name %in% names(data)) response_name <- paste0(response_name, "_")
    data[[response_name]] <- NA_real_
    response <- response_name
  }
  
  # Detect whether an offset was specified outside the formula
  # and belongs to a model type that may ignore it (lme4 / gam)
  offset_external <- FALSE
  
  if (!is.null(model_call)) {
    if ("offset" %in% names(model_call)) {
      if (model_type$is_lme4 || model_type$is_gam) {
        offset_external <- TRUE
      }
    }
  }
  
  # Offset scaling logic
  # Transform the raw response using the offset only for log-link count models
  # (Poisson/quasi-Poisson/negative binomial/compois/genpois and truncated versions)
  # families where offset represents exposure
  scale_patterns <- c("poisson","quasipoisson","compois","genpois","nbinom","negative binomial")
  # require both count family and log link to scale data
  should_scale <- !is.na(fam) &&
    any(grepl(paste(scale_patterns, collapse="|"), fam)) &&
    identical(link, "log")
  
  offset_ref_value <- NULL  # will store the reference offset used for scaling
  
  if (!is.null(offset_vars) && should_scale && offset_is_log) {
    off <- offset_vars[1] 
    if (length(offset_vars) > 1) {
      warning("Multiple offsets detected. Using the first one for scaling raw data.")
    }
    
    # Determine reference offset: use fix_values if provided, otherwise 1
    if (!is.null(fix_values) && off %in% names(fix_values)) {
      offset_ref_value <- suppressWarnings(as.numeric(fix_values[[off]]))
      if (!is.finite(offset_ref_value) || offset_ref_value <= 0) {
        warning(sprintf("Invalid reference exposure specified in fix_values for '%s'. 
Using 1 instead.",
          off
        ))
        offset_ref_value <- 1
      }
    } else {
      offset_ref_value <- 1
    }
    
    if (is.numeric(data[[off]])) {
      original_response <- response
      response_scaled <- paste0(response, "_scaled")
      
      if (any(data[[off]] == 0)) {
        if (show_data_points) {
          warning(sprintf("Exposure variable '%s' contains zeros. 
Points for these rows will not be properly scaled.",
            off))
        }
        scaled <- (data[[response]] / ifelse(data[[off]] == 0, 1, data[[off]])) * offset_ref_value
        data[[response_scaled]] <- scaled
      } else {
        data[[response_scaled]] <- (data[[response]] / data[[off]]) * offset_ref_value
        if (show_data_points) {
          message(sprintf("Offset detected:
plotting (%s / %s) * %g (rate scaled to exposure = %g).",
            original_response, off, offset_ref_value, offset_ref_value
          ))
        }
      }
      response <- response_scaled
    }
  } else if (!is.null(offset_vars) && should_scale && !offset_is_log && !offset_external) {
    
    # Show message only when raw data are plotted
    if (show_data_points) {
      message("Note: the offset is not of the form log(exposure).
easyViz will not rescale raw data or fix the exposure to 1 in predictions.
For full control over scaling and exposure values, specify the offset as 
log(exposure), using the exposure variable untransformed.")
    }
    
    offset_warning_issued <- show_data_points
  }
  
  # numeric / categorical conditioning
  # Identify numeric and categorical predictors
  predictor_is_numeric <- is.numeric(data[[predictor]])
  # Conditioning numeric and categorical variables
  numeric_vars <- sapply(data, is.numeric)
  numeric_vars[predictor] <- FALSE  # Exclude predictor
  
  if (num_conditioning == "mean") {
    num_values <- sapply(data[, numeric_vars, drop = FALSE], mean, na.rm = TRUE)
  } else if (num_conditioning == "median") {
    num_values <- sapply(data[, numeric_vars, drop = FALSE], median, na.rm = TRUE)
  } else {
    stop("Invalid value for num_conditioning. Choose 'mean' or 'median'.")
  }
  
  categorical_vars <- sapply(data, is.factor)
  categorical_vars[predictor] <- FALSE  # Exclude predictor
  
  if (cat_conditioning == "mode") {
    cat_values <- sapply(data[, categorical_vars, drop = FALSE], ez_mode_factor)
    if (anyNA(cat_values)) {
      bad_vars <- names(cat_values)[is.na(cat_values)]
      stop("Could not determine mode for the following factor(s), 
likely due to all values being NA: ",
           paste(bad_vars, collapse = ", "))
    }
  } else if (cat_conditioning == "reference") {
    cat_values <- sapply(data[, categorical_vars, drop = FALSE], function(x) {
      levs <- levels(x)
      if (length(levs) == 0) return(NA_character_)
      levs[1]  # this is the reference level
    })
    if (anyNA(cat_values)) {  # Check for NAs and stop early with a helpful message
      bad_vars <- names(cat_values)[is.na(cat_values)]
      stop("Could not determine reference level for the following factor(s): ",
           paste(bad_vars, collapse = ", "))
    }
  } else {
    stop("Invalid value for cat_conditioning. Choose 'mode' or 'reference'.")
  }
  
  # Warn if offset was passed outside of the formula (and we're using gam/lmer/glmer)
  if (offset_external) {
    offset_expr <- model_call$offset
    message("Careful! The offset was specified outside the model formula.
For gam() and glmer() models, such offsets are treated as offset = 0 
during prediction (i.e., exposure = 1 in log-link count/rate models). 
This matches easyViz's default behavior of plotting rates at exposure = 1, 
but in this situation easyViz cannot vary the exposure level during prediction.
Use the offset in the formula (i.e., offset(log(exposure))) for full control.")
  }
  
  list(
    data = data,
    original_data = original_data,
    response = response,
    predictor = predictor,
    by = by,
    by_breaks = by_breaks, 
    model_vars = model_vars,
    predictor_is_numeric = predictor_is_numeric,
    num_values = num_values,
    cat_values = cat_values,
    offset_vars = offset_vars,
    should_scale = should_scale,
    fam = fam,
    link = link,
    model_type = model_type,
    fix_values = fix_values,
    show_data_points = show_data_points,
    offset_ref_value = offset_ref_value,
    offset_is_log = offset_is_log,
    offset_warning_issued = offset_warning_issued,
    offset_external = offset_external
  )
}