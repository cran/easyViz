# ez-predictions.R

ez_compute_predictions <- function(model,
                                   new_data,
                                   prep,
                                   re_form,
                                   rlm_vcov = "HC0") {
  mt <- prep$model_type
  is_lm_model      <- mt$is_lm
  is_rlm_model     <- mt$is_rlm
  is_gls_model     <- mt$is_gls
  is_nls_model     <- mt$is_nls
  is_glm_model     <- mt$is_glm
  is_betareg_model <- mt$is_betareg
  is_lme4_model    <- mt$is_lme4
  is_glmmTMB_model <- mt$is_glmmTMB
  is_gam_model     <- mt$is_gam
  is_coxph_model   <- mt$is_coxph
  
  # For mgcv::gam models, emulate re.form = NA or ~0 by excluding re smooths
  exclude_terms <- character(0)
  exclude_re_smooths <- identical(re_form, NA) ||
    (inherits(re_form, "formula") &&
       trimws(paste(deparse(re_form), collapse = " ")) %in% c("~0", "~ 0"))
  if (is_gam_model && exclude_re_smooths) {
    re_smooths <- ez_gam_re_smooths(model)
    if (any(re_smooths)) {
      exclude_terms <- vapply(model$smooth[re_smooths], function(sm) sm$label, character(1))
    } else {
      exclude_terms <- character(0)
    }
    if (length(exclude_terms) > 0) {
      message("Excluding random-effect smooth terms from prediction 
(analogous to re.form = NA or ~0): ",
              paste(exclude_terms, collapse = ", "))
    }
  }
  
  # Predict according to model type
  if (is_lme4_model && is.null(re_form)) {
    pred_fit <- if (inherits(model, "glmerMod")) {
      predict(model, newdata = new_data, type = "link", re.form = NULL)
    } else {
      predict(model, newdata = new_data, re.form = NULL)
    }
    preds_link <- list(fit = pred_fit, se.fit = rep(NA_real_, length(pred_fit)))
    message("Note: CIs are not available for lme4 models when re_form = NULL.")
    link_name <- ez_get_link(model)
    
  } else if (is_gam_model) {
    pr <- predict(model, newdata = new_data, se.fit = TRUE, type = "link",
                  exclude = if (exclude_re_smooths) exclude_terms else NULL)
    preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
    link_name <- ez_get_link(model)

  } else if (is_coxph_model) {
    
    # survival::coxph
    # Link scale is the linear predictor: log(relative hazard)
    pr <- tryCatch(
      predict(model, newdata = new_data, type = "lp", se.fit = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(pr)) {
      pred_fit <- predict(model, newdata = new_data, type = "lp")
      preds_link <- list(fit = pred_fit, se.fit = rep(NA_real_, length(pred_fit)))
      message("Note: se.fit not available for this coxph prediction; returning NA SEs.")
    } else {
      # predict.coxph can return either a numeric vector or a list depending on args/version
      if (is.list(pr) && !is.null(pr$fit)) {
        preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
      } else {
        # fallback if returned just a vector
        preds_link <- list(fit = as.numeric(pr), se.fit = rep(NA_real_, length(pr)))
        message("Note: Unexpected predict() return type for coxph; returning NA SEs.")
      }
    }
    
    link_name <- "log"
    
  } else if (is_lme4_model) {
    if (inherits(model, "glmerMod")) {
      pr <- tryCatch(
        predict(model, newdata = new_data, type = "link",
                re.form = re_form, se.fit = TRUE),
        error = function(e) NULL
      )
      if (is.null(pr) || is.null(pr$se.fit)) {
        pred_fit <- predict(model, newdata = new_data,
                            type = "link", re.form = re_form)
        preds_link <- list(
          fit = pred_fit,
          se.fit = rep(NA_real_, length(pred_fit))
        )
        message("Note: se.fit not available for this glmerMod prediction; returning NA SEs.")
      } else {
        preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
      }
      link_name <- ez_get_link(model)
    } else if (inherits(model, "lmerMod")) {
      pr <- tryCatch(
        predict(model, newdata = new_data,
                re.form = re_form, se.fit = TRUE),
        error = function(e) NULL
      )
      if (is.null(pr) || is.null(pr$se.fit)) {
        pred_fit <- predict(model, newdata = new_data, re.form = re_form)
        preds_link <- list(
          fit = pred_fit,
          se.fit = rep(NA_real_, length(pred_fit))
        )
        message("Note: se.fit not available for this lmerMod prediction; returning NA SEs.")
      } else {
        preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
      }
      link_name <- ez_get_link(model)
      
    } else {
      stop("Unsupported lme4 model class: ", paste(class(model), collapse = ", "))
    }
    
  } else if (is_glmmTMB_model) {
    pr <- tryCatch(
      predict(model, newdata = new_data, se.fit = TRUE, type = "link", re.form = re_form),
      error = function(e) NULL
    )
    if (is.null(pr)) {
      pred_fit <- predict(model, newdata = new_data, type = "link", re.form = re_form)
      preds_link <- list(fit = pred_fit, se.fit = rep(NA_real_, length(pred_fit)))
      message("Note: se.fit not available for this glmmTMB prediction; returning NA SEs.")
    } else {
      preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
    }
    link_name <- ez_get_link(model)
    
  } else if (is_betareg_model) {
    # Align factor levels in new_data to the fitted model (keep your current logic)
    mf_train <- model.frame(model)
    for (v in names(new_data)) {
      if (v %in% names(mf_train) && is.factor(mf_train[[v]])) {
        new_data[[v]] <- factor(new_data[[v]], levels = levels(mf_train[[v]]))
      }
    }
    # Get link-scale predictions
    pred_link <- predict(model, newdata = new_data, type = "link")
    # Mean-part covariance matrix
    k  <- length(coef(model, model = "mean"))
    
    vc <- tryCatch({
      V <- vcov(model)
      if (is.null(dim(V)) || nrow(V) < k) stop("Invalid vcov dimension")
      V[1:k, 1:k, drop = FALSE]
    }, error = function(e) NULL)
    
    # Build the *same* fixed-effect design matrix used by the model
    tt <- delete.response(terms(model))   # keep original formula structure
    
    # If an offset() is present inside the formula, remove it from the design matrix:
    off <- attr(tt, "offset")
    if (!is.null(off) && length(off) > 0) {
      tt <- drop.terms(tt, off, keep.response = FALSE)
    }
    
    # Delta-method SEs
    se_fit <- if (is.null(vc)) {
      warning("Could not extract vcov() for betareg; returning NA SEs.")
      rep(NA_real_, nrow(new_data))
    } else {
      tryCatch({
        # Build model frame for newdata using training levels
        xlev   <- .getXlevels(tt, mf_train)
        mf_new <- model.frame(tt, data = new_data, na.action = na.pass, xlev = xlev)
        
        # Build model matrix using only relevant contrasts
        contr_use <- model$contrasts
        if (!is.null(contr_use)) {
          contr_use <- contr_use[names(contr_use) %in% names(mf_new)]
        }
        
        X <- model.matrix(tt, data = mf_new, contrasts.arg = contr_use)
        sqrt(rowSums((X %*% vc) * X))
      }, error = function(e) {
        warning("betareg SEs failed; returning NA. Reason: ", conditionMessage(e))
        rep(NA_real_, nrow(new_data))
      })
    }
    
    preds_link <- list(fit = pred_link, se.fit = se_fit)
    message("Note: CIs for betareg models use a delta-method approximation.")
    link_name <- ez_get_link(model)
    
  } else if (is_glm_model) {
    pr <- predict(model, newdata = new_data, se.fit = TRUE, type = "link")
    preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
    link_name <- ez_get_link(model)
    
  } else if (is_nls_model) {
    if (!requireNamespace("numDeriv", quietly = TRUE)) {
      stop("Package 'numDeriv' is required for computing 
approximate confidence intervals for nls models. 
Please install it.")
    }
    pred_fun <- function(par, data_row) {
      local_env <- as.list(par)
      local_env <- c(local_env, as.list(data_row))
      eval(formula(model)[[3]], envir = local_env)
    }
    params <- coef(model)
    vcov_mat <- try(vcov(model), silent = TRUE)
    if (inherits(vcov_mat, "try-error")) {
      warning("Could not extract variance-covariance matrix from nls model.")
      se_fit <- rep(NA, nrow(new_data))
      pred_fit <- apply(new_data, 1, function(row) pred_fun(params, as.list(row)))
    } else {
      pred_fit <- numeric(nrow(new_data))
      se_fit <- numeric(nrow(new_data))
      for (i in seq_len(nrow(new_data))) {
        grad_i <- numDeriv::grad(func = pred_fun, x = params, data_row = new_data[i, ])
        pred_fit[i] <- pred_fun(params, new_data[i, ])
        se_fit[i] <- sqrt(t(grad_i) %*% vcov_mat %*% grad_i)
      }
    }
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    message("Note: CIs for nls models use a delta-method approximation.")
    link_name <- ez_get_link(model)
    
  } else if (is_gls_model) {
    # Create the design matrix using the same terms used in the model
    tt <- delete.response(terms(model))
    X_new <- model.matrix(tt, new_data)
    betas <- coef(model)
    pred_fit <- as.vector(X_new %*% betas)
    vcov_mat <- vcov(model)
    se_fit <- sqrt(rowSums((X_new %*% vcov_mat) * X_new))
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    message("Note: CIs for gls models use the fixed-effect covariance matrix.")
    link_name <- ez_get_link(model)
    
  } else if (is_rlm_model) { # compute SEs using sandwich estimator
    
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required to compute robust SEs for rlm models. 
Please install it.")
    }
    
    # Ensure factor alignment
    model_frame <- model.frame(model)
    for (var in names(new_data)) {
      if (var %in% names(model_frame) && is.factor(model_frame[[var]])) {
        new_data[[var]] <- factor(new_data[[var]], levels = levels(model_frame[[var]]))
      }
    }
    
    # Create the design matrix using the same terms used in the model
    tt <- delete.response(terms(model))
    X <- model.matrix(tt, new_data)
    betas <- coef(model)
    # Compute predicted values
    pred_fit <- as.vector(X %*% betas)
    
    # Robust variance selection
    if (is.matrix(rlm_vcov)) {
      vcov_mat <- rlm_vcov
      
    } else if (is.function(rlm_vcov)) {
      vcov_mat <- rlm_vcov(model)
      
    } else if (is.character(rlm_vcov)) {
      valid_types <- c("HC0","HC","const","HC1","HC2","HC3","HC4","HC4m","HC5")
      rlm_vcov <- match.arg(rlm_vcov, valid_types)
      
      # HC and const are equivalent in vcovHC
      type_use <- if (rlm_vcov %in% c("HC","const")) "const" else rlm_vcov
      
      vcov_mat <- tryCatch(
        sandwich::vcovHC(model, type = type_use),
        error = function(e) {
          warning("Failed to compute robust SEs for rlm with type = '", rlm_vcov,
                  "'. Falling back to model$cov.")
          model$cov
        }
      )
    } else {
      stop("rlm_vcov must be a character string, a matrix, or a function.")
    }
    
    # Compute standard errors
    se_fit <- tryCatch({
      sqrt(rowSums((X %*% vcov_mat) * X))
    }, error = function(e) {
      warning("Failed with full vcov; using diagonal approximation.")
      sqrt(rowSums((X %*% diag(diag(vcov_mat))) * X))
    })
    
    message("Robust standard errors for rlm model computed using: ", rlm_vcov)
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    link_name <- ez_get_link(model)
    
  } else if (is_lm_model) {
    pr <- predict(model, newdata = new_data, se.fit = TRUE)
    preds_link <- list(fit = pr$fit, se.fit = pr$se.fit)
    link_name <- ez_get_link(model)
    
  } else {
    stop("easyViz does not support this type of model. 
Supported types include: lm, rlm, gls, nls, glm, glm.nb, 
betareg, coxph (survival), lmer/glmer, glmmTMB, and gam (mgcv).")
  }
  
  # glmmTMB correlation structure message
  if (is_glmmTMB_model && !is.null(model$call)) {
    call_entries <- model$call[c("formula", "ziformula", "dispformula")]
    call_entries <- call_entries[!vapply(call_entries, is.null, logical(1))]
    if (length(call_entries) > 0) {
      call_parts <- unlist(lapply(call_entries, deparse), use.names = FALSE)
      # List of correlation structure keywords supported by glmmTMB
      correlation_keywords <- c("exp", "mat", "gau", "gen", "ar1", "cs", "un", "toep", "usr")
      has_correlation <- any(sapply(correlation_keywords, function(k) {
        any(grepl(paste0("\\b", k, "\\s*\\("), call_parts))
      }))
      if (has_correlation) {
        message("Note: This glmmTMB model includes a residual correlation structure.
For CIs, easyViz relies on the standard errors provided by predict().")
      }
    }
  }
  
  list(
    fit = preds_link$fit,
    se.fit = preds_link$se.fit,
    link = link_name
  )
}