# easyViz 2.0.1
- Bug fix: the legend placement preset `legend_position = "out"` is now computed
  per panel in multi-panel base graphics layouts (e.g., `par(mfrow=...)`,
  `par(mfcol=...)`, `layout()`). Legends are consistently positioned above their relevant
  plot region rather than between panels.
- Documentation: updated the examples to use `exposure` as the offset variable
  name (replacing `offset_var`) for consistency and clarity.
  
# easyViz 2.0.0
- Major internal redesign: the package has been fully modularized to improve 
  clarity, maintainability, and extensibility. Core functionality is now 
  organized into dedicated components for data preparation, newdata 
  construction, prediction, plotting, and output assembly.
- Added support for `betareg` models: confidence intervals for the mean submodel 
  are computed using a delta-method approximation.
- Added support for survival models fitted with `survival::coxph()`.
  Conditional effects are plotted on the linear predictor (log-hazard) scale.
  Raw survival data (event times and censoring) are not plotted, and confidence
  intervals are based on standard errors of the linear predictor.
- New `by_breaks` argument: allows users to manually specify conditioning 
  values when `by` is numeric, replacing the previous fixed default behavior. 
  If not provided, numeric `by` variables  with more than 6 unique values
  default to their 10th, 50th, and 90th percentiles.
- New `rlm_vcov` argument: robust standard errors for `MASS::rlm()` models 
  can now be computed using any sandwich estimator type (e.g., `"HC3"`, `"HC5"`, 
  `"const"`, etc.), a user-supplied covariance matrix, or a custom function.
- New `ci_polygon_alpha` argument: allows users to control the transparency of 
  polygon-style confidence interval bands when `ci_type = "polygon"`. 
  Defaults to 0.5 and accepts values between 0 and 1.
- New `show_conditioning` argument: allows users to print a concise summary
  in the console describing how predictions are conditioned. When
  `show_conditioning = TRUE`, `easyViz()` reports which variables are held
  fixed and which vary in the prediction grid, whether predictions from
  mixed-effects or GAM models are conditional on random effects
  or marginal, and, for numeric `by` variables, the values at which predictions 
  are evaluated (e.g., quantiles or user-specified `by_breaks`). 
  This option does not affect plots or returned values and is intended as a diagnostic aid.
- New `plot` argument: allows users to suppress graphical output. When 
  `plot = FALSE`, `easyViz()` returns only the predictions (as an invisible 
  `easyviz.pred.df` object) without drawing a plot. Useful when storing or 
  further processing predicted values.
- Improved offset handling: offsets specified either in the model formula 
  or via `offset =` are now automatically detected. For log-link count models 
  (e.g., Poisson, negative binomial, COM-Poisson, generalized or truncated 
  count models), observed responses are automatically scaled by the offset 
  for plotting, and the prediction grid sets the offset to 1 by default, 
  ensuring predictions and raw data are displayed on a common rate scale.
- Improved handling of numeric `by` variables: numeric conditioning variables with a
  *small number of distinct values* (currently ≤ 6) are now treated as discrete groups 
  and plotted with one line per value (like a factor), instead of always using 
  quantile cross-sections. For truly continuous numeric `by` variables 
  (more than 6 unique values), `easyViz()` still uses default cross-sections
  (10th, 50th, and 90th percentiles) or the user-supplied values from `by_breaks`.
  This makes the behavior more intuitive for common cases such as 0/1 or small ordinal scales.
- Improved stability and consistency in handling numeric conditioning variables, 
  interactions, binomial data, and GAM models with random-effect smooths.
- Improved legend behavior: when a `by` variable is specified and users do not 
  explicitly set `add_legend`, `easyViz()` now automatically enables the legend. 
  This ensures that multi-line or multi-group plots always display a clear legend 
  by default, while preserving user control when `add_legend` is manually set.
- New legend placement preset `"out"` for `legend_position`: setting
  `legend_position = "out"` applies a predefined layout that places the legend
  outside the plotting region above the panel.
- Documentation and diagnostic messages significantly expanded and clarified.

# easyViz 1.2.0
- Renamed `re.form` to `re_form` for consistency with 
  other arguments. Argument `re.form` is deprecated. 
  Please use `re_form` instead.
- Added `ci_level` argument to control the confidence interval level 
  (default is 0.95). The confidence level used for the interval is 
  included in the returned prediction data frame.
- Documentation improvements.

# easyViz 1.1.0
- Added support for the new `pred_resolution` argument, which controls 
  the number of prediction points used for numeric predictors. 
  The default is `101` (matching `visreg`), but higher values can be helpful 
  for predictors with a wide range or when visualizing nonlinear relationships.
- Added support for `ci_type = NULL` to suppress confidence intervals 
  for numeric predictors.
- Added support for custom legend titles using the new 
  `legend_title` argument.
- Added support for adjusting legend title text size using the 
  new `legend_title_size` argument.
- Added support for horizontal legends using the new 
  `legend_horiz` argument.
- Added support for fine-tuned legend customization via the 
  new `legend_args` argument, which allows passing any additional 
  parameters to `legend()` (e.g., `pt.cex`, `bg`, `box.lwd`, `inset`).
- Renamed `legend_text_size` to `legend_label_size` for consistency with 
  `legend_title_size` and `legend_labels`. The argument `legend_text_size` 
  is deprecated. Please use `legend_label_size` instead.
- Added support for categorical predictors: users can now 
  customize x-axis text size, color, and font using `plot_args` 
  (e.g., `cex.axis`, `col.axis`, `font.axis`) when the predictor 
  is a factor.
- Added finite-sample adjusted t-based confidence intervals for 
  `lm()`, `rlm()`, `gls()`, and `nls()` models. 
  For models with asymptotic inference (e.g., `glm()`, `mgcv::gam()`, 
  `glmer()`, `glmmTMB()`), confidence intervals are based on 
  standard normal approximations.
- Improved visual clarity: when `by` is specified for interactions, 
  all prediction lines are now drawn on top of their associated 
  confidence intervals, ensuring they remain visible and are not 
  obscured by overlapping CI polygons.
- Improved detection of binomial-style responses.
- Improved internal handling of link functions: the logit inverse 
  now uses `plogis()` for greater numerical stability, and the cloglog 
  inverse is now more robust to extreme values.
- Added a diagnostic warning when user-supplied aesthetic vectors 
  (e.g., `point_col`) have a length mismatch with the cleaned data 
  used for predictions, helping users avoid misaligned plot 
  aesthetics when the original data contains missing values.
- Added an example demonstrating how to visualize 3-way interactions 
  using the `by` and `fix_values` arguments in a multi-panel layout.
- Documentation improvements.

