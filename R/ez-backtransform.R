# ez-backtransform.R

ez_link_inverse <- function(link) {
  link <- tolower(trimws(link))
  link_inverses <- list(
    logit    = plogis,
    probit   = pnorm,
    cauchit  = pcauchy,
    cloglog  = function(eta) - expm1(-exp(pmin(eta, 700))),
    loglog   = function(eta) 1 - exp(-exp(pmin(eta, 700))),
    log      = exp,
    sqrt     = function(eta) eta^2,
    inverse  = function(eta) 1/eta,
    `1/mu^2` = function(eta) 1/sqrt(eta),
    identity = identity
  )
  if (!link %in% names(link_inverses))
    stop("Unsupported link function: ", link)
  link_inverses[[link]]
}

ez_backtransform <- function(preds_link,
                             model,
                             prep,
                             pred_type,
                             backtransform_response,
                             ci_level) {
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1)
    stop("ci_level must be between 0 and 1.")
  
  link_fit <- preds_link$fit
  link_se  <- preds_link$se.fit
  
  alpha <- (1 + ci_level) / 2
  mt <- prep$model_type
  
  crit <- tryCatch({
    if (mt$is_lm || mt$is_rlm || mt$is_gls || mt$is_nls) {
      df <- tryCatch(df.residual(model), error = function(e) NA_real_)
      if (is.null(df) || is.na(df) || !is.finite(df) || df <= 0) {
        n <- tryCatch(nobs(model), error = function(e) NA_real_)
        k <- tryCatch(length(coef(model)), error = function(e) NA_real_)
        df <- if (!is.na(n) && !is.na(k) && is.finite(n - k) && (n - k) > 0) n - k else NA_real_
      }
      if (!is.na(df) && is.finite(df) && df > 0) qt(alpha, df) else qnorm(alpha)
    } else {
      qnorm(alpha)
    }
  }, error = function(e) qnorm(alpha))
  
  link_lower <- link_fit - crit * link_se
  link_upper <- link_fit + crit * link_se
  
  # gls/nls: identity scale (no family)
  if (mt$is_gls || mt$is_nls) {
    
    preds <- list(
      fit   = link_fit,
      lower = link_lower,
      upper = link_upper
    )
    
  } else if (pred_type == "response") {
    
    # Special case: survival::coxph has no family()/link.
    # type="lp" is log(hazard ratio), so response = exp(lp) = hazard ratio.
    if (isTRUE(mt$is_coxph)) {
      
      linkinv <- ez_link_inverse("log")
      preds <- list(
        fit   = linkinv(link_fit),
        lower = linkinv(link_lower),
        upper = linkinv(link_upper)
      )
      
    } else {
      
      link_function <- tryCatch({
        if (inherits(model, "lme")) {
          "identity"
        } else if (inherits(model, "betareg")) {
          if (!is.null(model$link$mean)) model$link$mean$name else model$link$mu$name
        } else if (!is.null(family(model)$link)) {
          family(model)$link
        } else if (!is.null(model$modelInfo$family$link)) {
          model$modelInfo$family$link
        } else if (!is.null(model$family$link)) {
          model$family$link
        } else {
          stop("Cannot determine the link function from the model.")
        }
      }, error = function(e) {
        stop("Unable to identify the link function: ", e$message)
      })
      
      linkinv <- ez_link_inverse(link_function)
      preds <- list(
        fit   = linkinv(link_fit),
        lower = linkinv(link_lower),
        upper = linkinv(link_upper)
      )
    }
    
  } else {
    
    preds <- list(
      fit   = link_fit,
      lower = link_lower,
      upper = link_upper
    )
  }
  
  # Optional custom response backtransformation
  if (pred_type == "response" &&
      !is.null(backtransform_response) &&
      is.function(backtransform_response)) {
    preds <- list(
      fit   = backtransform_response(preds$fit),
      lower = backtransform_response(preds$lower),
      upper = backtransform_response(preds$upper)
    )
  }
  
  preds
}