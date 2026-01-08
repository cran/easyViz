# ez-utils.R  (internal helpers)

# define helper to unwrap a cleaned-up expression object 
# (e.g., y, cbind(a, b), successes / trials) so we can analyze its structure...
ez_unwrap_to_meaningful_expr <- function(expr) {
  while (is.call(expr)) {
    head <- as.character(expr[[1]])
    if (head %in% c("(", "c")) {
      expr <- expr[[2]]
    } else {
      break
    }
  }
  expr
}

# define helper to extract a usable variable name from the object,
# if it’s not one of the special forms (binomial proportion data)
ez_extract_response_name <- function(expr) {
  while (is.call(expr) && as.character(expr[[1]]) %in% c("(", "c")) {
    expr <- expr[[2]]
  }
  if (is.call(expr) && as.character(expr[[1]]) %in% c("$")) {
    return(as.character(expr[[3]]))
  } else if (is.name(expr)) {
    return(as.character(expr))
  } else {
    return(all.vars(expr)[1])
  }
}

# Check for use of $ in the response variable
ez_warn_on_env_prefix <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  head <- as.character(expr[[1]])
  if (head == "$") return(TRUE)
  any(vapply(as.list(expr)[-1], ez_warn_on_env_prefix, logical(1)))
}

# Identify variables that should be complete (i.e., no NA)
ez_extract_clean_formula_vars <- function(expr) {
  raw_vars <- all.vars(expr)
  # Remove environment references like df in df$y
  remove_env_refs <- function(e) {
    if (!is.call(e)) return(character(0))
    head <- as.character(e[[1]])
    if (head %in% c("$")) {
      return(as.character(e[[2]])) # "df" in df$y
    }
    unlist(lapply(as.list(e)[-1], remove_env_refs))
  }
  env_refs <- remove_env_refs(expr)
  setdiff(raw_vars, env_refs)
}


# Validate color arguments
ez_validate_color <- function(color) {
  tryCatch({
    grDevices::col2rgb(color)
    TRUE
  }, error = function(e) FALSE)
}

# Validate user-supplied aesthetic lengths (if vector input)
# Only meaningful for point aesthetics.
# The warning is triggered when the user supplies a vector that matches the number of rows in the original (unfiltered) data
# but not in the cleaned data used for plotting — a sign that the aesthetic was constructed before NA filtering
# and may now be misaligned with the actual data being plotted.
# Typical example is the use of 'ifelse(your.data$x=="level1"...)' to change point aesthetics.
ez_check_aesthetic_length <- function(arg, name, n_clean, n_original, model_vars) {
  if (!is.null(arg) && length(arg) > 1 &&
      length(arg) != n_clean &&
      length(arg) == n_original) {
    warning(sprintf(
      "Length mismatch: '%s' has length %d, but the cleaned data frame 
used for predictions has %d rows. This may cause incorrect aesthetic mapping.
This usually happens when '%s' is constructed from the original data frame, 
which still includes rows with missing values. 
Remove rows with NA in the variables used in the model, for example:

clean.data <- your.data[complete.cases(your.data[, c(\"%s\")]), ]

Then construct '%s' using clean.data instead of the original data.",
      name, length(arg), n_clean, name,
      paste(model_vars, collapse = "\", \""),
      name
    ), call. = FALSE)
  }
}

# outer legend helpers
# For single-panel plots: compute a point just above the plot region
ez_out_top_xy <- function(offset_in = 0.08) {
  plt <- graphics::par("plt")  # NDC: c(x1, x2, y1, y2)
  
  # centered over plot region (in NDC)
  x_ndc <- mean(plt[1:2])
  
  # convert a physical offset (inches) to NDC units
  off_ndc <- graphics::grconvertY(offset_in, from = "inches", to = "ndc") -
    graphics::grconvertY(0,        from = "inches", to = "ndc")
  
  # just above plot region
  y_ndc <- plt[4] + off_ndc
  
  # NDC -> user coords for legend()
  x_usr <- graphics::grconvertX(x_ndc, from = "ndc", to = "user")
  y_usr <- graphics::grconvertY(y_ndc, from = "ndc", to = "user")
  
  c(x = x_usr, y = y_usr)
}

ez_apply_out_legend_preset <- function(legend_position,
                                       legend_horiz,
                                       legend_args,
                                       n_labels,
                                       threshold = 6,
                                       offset_small_in = 0.08,
                                       offset_large_in = 0.11,
                                       ncol_large = 6) {
  
  la <- if (is.null(legend_args)) list() else legend_args
  
  is_out <- is.character(legend_position) && identical(legend_position, "out")
  if (!is_out) {
    return(list(
      is_out = FALSE,
      legend_position = legend_position,
      legend_horiz = legend_horiz,
      legend_args = la
    ))
  }
  
  # allow drawing outside plot (margins)
  if (!("xpd" %in% names(la))) la$xpd <- TRUE
  
  # adaptive layout based on number of labels
  if (!is.null(n_labels) && is.finite(n_labels) && n_labels > threshold) {
    legend_horiz <- FALSE
    if (!("ncol" %in% names(la))) la$ncol <- ncol_large
    offset_in <- offset_large_in
  } else {
    legend_horiz <- TRUE
    offset_in <- offset_small_in
  }
  
  # ---- coordinate-based placement (NO inset) ----
  xy <- ez_out_top_xy(offset_in = offset_in)
  
  # anchor bottom-center at (x, y)
  if (!("xjust" %in% names(la))) la$xjust <- 0.5
  if (!("yjust" %in% names(la))) la$yjust <- 0
  if (!("bty"   %in% names(la))) la$bty   <- "n"
  
  # ignore inset / explicit x,y in "out" mode (prevents conflicts)
  if ("inset" %in% names(la)) la$inset <- NULL
  if ("x"     %in% names(la)) la$x     <- NULL
  if ("y"     %in% names(la)) la$y     <- NULL
  
  list(
    is_out = TRUE,
    legend_position = unname(xy),   # numeric c(x, y) used directly by legend()
    legend_horiz = legend_horiz,
    legend_args = la
  )
}


# Detect model type
ez_detect_model_type <- function(model) {
  list(
    is_lm      = inherits(model, "lm") && !inherits(model, c("rlm", "glm", "negbin", "gam")),
    is_rlm     = inherits(model, "rlm"),
    is_gls     = inherits(model, "gls"),
    is_nls     = inherits(model, "nls"),
    is_glm     = inherits(model, "glm"),
    is_betareg = inherits(model, "betareg"),
    is_lme4    = inherits(model, "glmerMod") || inherits(model, "lmerMod"),
    is_glmmTMB = inherits(model, "glmmTMB"),
    is_gam     = inherits(model, "gam"),
    is_coxph   = inherits(model, "coxph")
  )
}

# Function to get family name from model
ez_get_family_name <- function(model) {
  # survival::coxph
  if (inherits(model, "coxph")) return("coxph")
  
  fam_char <- tryCatch({
    fam_obj <- family(model)
    fam_obj$family
  }, error = function(e) NULL)
  if (!is.null(fam_char)) return(tolower(fam_char))
  if (inherits(model, "betareg")) return("beta")
  NA_character_
}

# Function to get link name from model
ez_get_link <- function(model) {
  # survival::coxph
  # linear predictor = log(relative hazard), so inverse-link is exp()
  if (inherits(model, "coxph")) return("log")
  # betareg models (special handling)
  if (inherits(model, "betareg")) {
    link <- tryCatch(
      {
        if (!is.null(model$link$mean)) model$link$mean$name
        else model$link$mu$name
      },
      error = function(e) NA_character_
    )
    return(tolower(link))
  }
  
  # MASS::glm.nb models (class: "negbin", "glm", "lm")
  # (could also be handled by the generic branch below, but this is fine)
  if (inherits(model, "negbin")) {
    return(tolower(model$family$link))
  }
  
  # Models that always have identity link
  if (inherits(model, "lm")  &&
      !inherits(model, c("rlm", "glm", "negbin", "gam"))) return("identity")
  if (inherits(model, c("rlm", "gls", "lme", "nls"))) return("identity")
  
  # Models storing link inside family(model)$link
  link <- tryCatch(family(model)$link, error = function(e) NULL)
  if (!is.null(link)) return(tolower(link))
  
  # Models storing link inside $modelInfo$family$link (e.g., glmmTMB)
  link <- tryCatch(model$modelInfo$family$link, error = function(e) NULL)
  if (!is.null(link)) return(tolower(link))
  
  # Models storing link inside $family$link
  link <- tryCatch(model$family$link, error = function(e) NULL)
  if (!is.null(link)) return(tolower(link))
  
  # Final fallback
  stop("Unable to determine the link function from the model.")
}

# get mode value for conditioning
ez_mode_factor <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(NA_character_)
  names(sort(table(x_no_na), decreasing = TRUE))[1]
}

# helper: extract grouping variables from ( ... | group ) terms
ez_extract_grouping_vars <- function(model) {
  f <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(f)) return(character(0))
  
  # try lme4 parser if available (works for lme4 formulas and usually for glmmTMB too)
  if (requireNamespace("lme4", quietly = TRUE)) {
    bars <- tryCatch(lme4::findbars(f), error = function(e) NULL)
    if (!is.null(bars) && length(bars) > 0) {
      grp <- unique(unlist(lapply(bars, function(b) all.vars(b[[3]]))))
      return(grp)
    }
  }
  
  # fallback: cheap parse of deparsed formula
  txt <- paste(deparse(f), collapse = " ")
  m <- gregexpr("\\|\\s*([[:alnum:]_.]+)", txt, perl = TRUE)
  hits <- regmatches(txt, m)[[1]]
  if (length(hits) == 0) return(character(0))
  unique(gsub("^\\|\\s*", "", hits))
}

# helper: identify random-effect smooth terms in mgcv::gam objects
ez_gam_re_smooths <- function(model) {
  if (!inherits(model, "gam") || is.null(model$smooth)) return(logical(0))
  
  vapply(model$smooth, function(sm) {
    inherits(sm, "random.effect") ||
      inherits(sm, "re.smooth") ||
      (!is.null(sm$xt) && !is.null(sm$xt$bs) && any(sm$xt$bs == "re")) ||
      (!is.null(sm$bs) && any(sm$bs == "re"))
  }, logical(1))
}

ez_has_random_effects <- function(model) {
  # lme4 models always imply random effects structure
  if (inherits(model, c("lmerMod", "glmerMod"))) return(TRUE)
  
  # glmmTMB: random effects only if formula contains | terms
  if (inherits(model, "glmmTMB")) {
    f <- tryCatch(stats::formula(model), error = function(e) NULL)
    if (is.null(f)) return(FALSE)
    if (!requireNamespace("lme4", quietly = TRUE)) return(FALSE)
    bars <- tryCatch(lme4::findbars(f), error = function(e) NULL)
    return(!is.null(bars) && length(bars) > 0)
  }
  
  # mgcv::gam: random effects if there are re smooths
  if (inherits(model, "gam")) {
    return(any(ez_gam_re_smooths(model)))
  }
  
  FALSE
}

ez_get_offset_vars <- function(model) {
  out <- character(0)
  
  # Offsets inside formula: offset(log(exposure))
  tt <- tryCatch(stats::terms(model), error = function(e) NULL)
  if (!is.null(tt)) {
    off_idx <- attr(tt, "offset")
    if (!is.null(off_idx) && length(off_idx) > 0) {
      vars_in_tt <- attr(tt, "variables")
      off_exprs  <- vars_in_tt[off_idx + 1]   # +1 because variables[[1]] is the response
      out <- union(out, unique(unlist(lapply(off_exprs, all.vars))))
    }
  }
  
  # Offsets supplied as a separate argument: betareg(..., offset = log(exposure))
  cl <- tryCatch(getCall(model), error = function(e) NULL)
  if (!is.null(cl) && !is.null(cl$offset)) {
    out <- union(out, all.vars(cl$offset))
  }
  
  out
}

# conditioning message
ez_message_conditioning <- function(model, prep, new_data, re_form,
                                    by_breaks = NULL, fix_values = NULL) {
  
  predictor <- prep$predictor
  by        <- prep$by %||% NULL
  
  ## ---- Random-effects message --------------------------------------------
  re_note <- NULL
  has_re <- ez_has_random_effects(model)
  
  # compute once, reuse everywhere
  exclude_re <- identical(re_form, NA) ||
    (inherits(re_form, "formula") &&
       trimws(paste(deparse(re_form), collapse = " ")) %in% c("~0", "~ 0"))
  
  if (has_re) {
    if (is.null(re_form)) {
      re_note <- "INCLUDED (conditional predictions; re_form = NULL)"
    } else if (exclude_re) {
      re_note <- "EXCLUDED (population-level predictions; re_form = NA or ~0)"
    } else {
      re_note <- paste0("per re_form = ", paste(deparse(re_form), collapse = " "))
    }
  }
  
  ## ---- Restrict to model vars (RHS + offsets) -----------------------------
  tt <- stats::terms(model)
  tt_rhs <- stats::delete.response(tt)
  model_vars <- all.vars(tt_rhs)
  
  # NEW: include offset variables (also catches betareg offset= argument)
  model_vars <- union(model_vars, ez_get_offset_vars(model))
  
  cols <- intersect(names(new_data), model_vars)
  cols <- union(cols, intersect(c(predictor, by), names(new_data)))
  
  ## ---- If random effects are excluded, don't report grouping vars as "held fixed" ----
  if (has_re && exclude_re) {
    # grouping variables from (1|group) etc. (lme4/glmmTMB)
    grp_vars <- ez_extract_grouping_vars(model)
    # ALSO handle mgcv::gam random-effect smooths: s(group, bs="re")
    if (inherits(model, "gam") && !is.null(model$smooth)) {
      re_terms <- unique(unlist(lapply(model$smooth[ez_gam_re_smooths(model)], `[[`, "term")))
      grp_vars <- union(grp_vars, re_terms)
    }
    
    # remove these vars from the conditioning summary (unless they are predictor/by)
    grp_vars <- setdiff(grp_vars, c(predictor, by))
    cols <- setdiff(cols, grp_vars)
  }
  ## ---- Identify fixed vs varying -----------------------------------------
  uniq_n <- vapply(cols, function(v) length(unique(new_data[[v]])), integer(1))
  fixed_cols   <- cols[uniq_n == 1]
  varying_cols <- cols[uniq_n > 1]
  
  fixed_cols <- setdiff(fixed_cols, c(predictor, by))
  varying_cols <- union(varying_cols, intersect(c(predictor, by), cols))
  
  ## ---- formatting ---------------------------------------------------------
  fmt <- function(x) {
    if (is.factor(x) || is.character(x)) as.character(x)[1]
    else signif(as.numeric(x)[1], 6)
  }
  
  fixed_pairs <- vapply(
    fixed_cols,
    function(v) paste0(v, " = ", fmt(new_data[[v]])),
    character(1)
  )
  
  ## ---- NEW: add grouping factor level when re_form = NULL ----------------
  if (is.null(re_form) && (prep$model_type$is_lme4 || prep$model_type$is_glmmTMB || prep$model_type$is_gam)) {
    
    grp_vars <- ez_extract_grouping_vars(model)
    
    # for gam random-effects via s(group, bs="re"), grouping var is usually in RHS anyway,
    # but this still helps if it wasn't carried into new_data.
    if (length(grp_vars) > 0) {
      
      # normalize fix_values to named list
      fv <- fix_values
      if (!is.null(fv) && !is.list(fv)) fv <- as.list(fv)
      
      for (g in grp_vars) {
        
        # if already shown as fixed, skip
        if (any(grepl(paste0("^", g, " = "), fixed_pairs))) next
        
        # determine the level being used
        g_val <- NULL
        
        if (!is.null(fv) && !is.null(fv[[g]])) {
          g_val <- fv[[g]]
        } else if (g %in% names(new_data) && length(unique(new_data[[g]])) == 1) {
          g_val <- unique(new_data[[g]])[1]
        } else if (!is.null(prep$data) && g %in% names(prep$data)) {
          # fall back to the most frequent (mode) level in the training/clean data
          g_val <- ez_mode_factor(prep$data[[g]])
        }
        
        if (!is.null(g_val) && !is.na(g_val)) {
          fixed_pairs <- c(fixed_pairs, paste0(g, " = ", as.character(g_val)))
        }
      }
    }
  }
  
  ## ---- Console output -----------------------------------------------------
  message("\nPrediction conditioning summary")
  
  if (!is.null(re_note))
    message("  - Random effects: ", re_note)
  
  if (!is.null(by) &&
      by %in% names(new_data) &&
      is.numeric(new_data[[by]])) {
    
    by_vals <- sort(unique(new_data[[by]]))
    
    if (!is.null(by_breaks)) {
      message("  - by (numeric): using user-specified values: ",
              paste(signif(by_vals, 6), collapse = ", "))
    } else {
      message("  - by (numeric): using default percentiles (10th, 50th, 90th): ",
              paste(signif(by_vals, 6), collapse = ", "))
    }
  }
  
  if (length(fixed_pairs) > 0) {
    message("  - Held fixed in prediction grid: ",
            paste(fixed_pairs, collapse = "; "))
  } else {
    message("  - Held fixed: none")
  }
  
  message("  - Varied in prediction grid: ",
          paste(varying_cols, collapse = ", "),
          "\n")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

