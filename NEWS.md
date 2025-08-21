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
