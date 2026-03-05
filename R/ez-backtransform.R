# ez-backtransform.R

ez_link_inverse <- function(link) {
  link <- tolower(trimws(link))
  eps <- .Machine$double.eps
  clamp01 <- function(x) pmin(pmax(x, 0), 1)
  
  link_inverses <- list(
    logit   = function(eta) clamp01(plogis(eta)),
    probit  = function(eta) clamp01(pnorm(eta)),
    cauchit = function(eta) clamp01(pcauchy(eta)),
    cloglog = function(eta) clamp01(-expm1(-exp(pmin(eta, 700)))),
    loglog  = function(eta) clamp01(1 - exp(-exp(pmin(eta, 700)))),
    log     = exp,
    sqrt    = function(eta) eta^2,
    inverse  = function(eta) 1 / pmax(eta, eps),
    `1/mu^2` = function(eta) 1 / sqrt(pmax(eta, eps)),
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
                             pred_transform,
                             ci_level) {
  
  pred_type <- match.arg(pred_type, c("response", "link"))
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1)
    stop("ci_level must be between 0 and 1.")
  
  link_fit <- preds_link$fit
  link_se  <- preds_link$se.fit
  
  alpha <- (1 + ci_level) / 2
  mt <- prep$model_type
  
  # Prefer link computed during prediction (single source of truth)
  link_name <- preds_link$link
  if (is.null(link_name) || is.na(link_name) || !nzchar(link_name)) {
    link_name <- tryCatch(ez_get_link(model), error = function(e) NA_character_)
  }
  link_name <- tolower(link_name)
  
  is_identity_link <- !is.na(link_name) && identical(link_name, "identity")
  has_pt <- !is.null(pred_transform) && is.function(pred_transform)
  
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
  
  # Build CI bounds safely (se.fit can be NA for some model/prediction types)
  link_lower <- rep(NA_real_, length(link_fit))
  link_upper <- rep(NA_real_, length(link_fit))
  
  ok <- is.finite(link_se)
  if (any(ok)) {
    link_lower[ok] <- link_fit[ok] - crit * link_se[ok]
    link_upper[ok] <- link_fit[ok] + crit * link_se[ok]
  }
  
  # default: return on link scale
  preds <- list(
    fit   = link_fit,
    lower = link_lower,
    upper = link_upper
  )
  
  # convert to response if requested
  if (identical(pred_type, "response")) {
    
    # coxph: lp is log(HR), so response is HR
    if (isTRUE(mt$is_coxph)) {
      linkinv <- ez_link_inverse("log")
    } else {
      if (is.na(link_name)) stop("Unable to identify the link function from the model.")
      linkinv <- ez_link_inverse(link_name)
    }
    
    preds <- list(
      fit   = linkinv(link_fit),
      lower = linkinv(link_lower),
      upper = linkinv(link_upper)
    )
  }
  
  # Optional prediction transformation:
  # Apply regardless of scale; warn to clarify what is happening.
  if (has_pt) {
    
    if (identical(pred_type, "link")) {
      warning(
          "Argument 'pred_transform' is being applied to predictions on the LINK scale,
before the model's inverse-link transformation. Ensure this behavior is intended. 
If response-scale predictions are desired, set pred_type = 'response'.",
        call. = FALSE
      )
    } else { # pred_type == "response"
      if (!is_identity_link) {
        warning(
        "Argument 'pred_transform' is being applied to predictions on the RESPONSE scale.
These predictions have already been transformed via the model's inverse-link function. 
Ensure this does not result in an unintended double-transformation.",
          call. = FALSE
        )
      }
    }
    
    preds <- list(
      fit   = pred_transform(preds$fit),
      lower = pred_transform(preds$lower),
      upper = pred_transform(preds$upper)
    )
  }
  
  preds
}