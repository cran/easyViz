#' Easy Visualization of Conditional Effects from Regression Models
#'
#' \code{easyViz} offers a flexible and user-friendly interface for visualizing conditional effects
#' from a broad range of regression and mixed-effects models using base R graphics.
#'
#' @param model [required] A fitted model object (e.g., \code{model = your.model}).
#'   Supported models include a wide range of regression types, including linear, robust linear, nonlinear, generalized least squares,
#'   generalized linear, survival, mixed-effects, and generalized additive (mixed) models.
#'   Compatible model-fitting functions include:
#'   \code{stats::lm}, \code{MASS::rlm}, \code{nlme::gls}, \code{stats::nls}, \code{stats::glm}, \code{MASS::glm.nb},
#'   \code{betareg::betareg}, \code{survival::coxph}, \code{lme4::lmer}, \code{lme4::glmer}, \code{lme4::glmer.nb}, \code{glmmTMB::glmmTMB}, and \code{mgcv::gam}.
#' @param data [required] The data frame used to fit the model (e.g., \code{data = your.data}).
#'   This data frame is used internally for generating predictions.
#'   \emph{All variables used in the model formula (including predictors, offset variables, grouping variables, and interaction terms) must be present in this data frame}.
#'   If the model was fitted without using a \code{data} argument (e.g., using variables from the global environment),
#'   you must ensure that \code{data} includes all required variables.
#'   Otherwise, prediction may fail or produce incorrect results.
#' @param predictor [required] The name of the target explanatory variable to be plotted (e.g., \code{predictor = "x1"}).
#' @param by The name of an interaction or additional variable for conditioning (e.g., \code{by = "x2"}).
#'   If supplied, \code{easyViz()} conditions predictions on \code{by} as follows:
#'   \itemize{
#'     \item Categorical (factor/character): a separate prediction line (or point) is plotted for each level.
#'     \item Numeric with few distinct values (default: \eqn{\le} 6 unique values):
#'           the numeric values are treated as discrete groups, and a separate prediction line is plotted for each value.
#'           This is useful for variables coded as numbers (e.g., 0/1, 1--5, small ordinal scales).
#'     \item Numeric with many distinct values (default: > 6 unique values):
#'           \code{by} is treated as continuous and predictions are shown at representative values
#'           (by default the 10th, 50th, and 90th percentiles), unless overridden by \code{by_breaks}.
#'     \item Grouping variable in random effects:
#'           if \code{by} corresponds to a variable used as a grouping term (as in, e.g., \code{(1|group)} or \code{s(group, bs="re")})
#'           and \code{re_form = NULL}, predictions are conditional on group-specific random effects.
#'   }
#'   Although \code{easyViz} does not natively support direct visualization of three-way interactions in a multi-panel plot,
#'   this can be easily achieved by combining the \code{by} and \code{fix_values} arguments.
#'   For example, if your model includes a term like \code{x1*x2*x3}, you can visualize the effect of \code{x1}
#'   across levels of \code{x2} by setting \code{predictor = "x1"}, \code{by = "x2"}, and fixing \code{x3}
#'   at a specific value using \code{fix_values = c(x3 = ...)}.
#'   Repeating this with different values of \code{x3} produces multiple plots that can be arranged to visualize the full three-way interaction.
#'   See the Examples section for a demonstration of how to apply this approach.
#' @param by_breaks Optional numeric vector specifying the values of a numeric conditioning variable
#'   to include in the plot (e.g., \code{by_breaks = c(-2, 0, 2)} or
#'   \code{by_breaks = quantile(your.data$x2, c(0.25, 0.5, 0.75))}).
#'   For numeric \code{by}, this selects the cross-sections to plot and overrides the default quantiles (0.1, 0.5, 0.9).
#'   Ignored if \code{by} is categorical.
#' @param pred_type Character string indicating the type of predictions to plot.
#'   Either \code{"response"} (default), which returns predictions on the original outcome scale
#'   by applying the inverse of the model's link function (e.g., probabilities for binary models),
#'   or \code{"link"}, which returns predictions on the linear predictor (link) scale
#'   (e.g., log-odds, log-counts, or other transformed scales depending on the model).
#'   For \emph{survival models} (\code{coxph}), \code{pred_type = "link"} returns predictions 
#'   on the linear predictor scale (log-hazard ratio), while \code{pred_type = "response"} returns hazard ratios.
#'   Survival probabilities are not produced because they require a time point and the baseline hazard.
#' @param pred_range_limit Logical. Applies only when the predictor is numeric and a categorical \code{by} variable is specified.
#'   If \code{TRUE} (default), the prediction range for each level of the \code{by} variable is limited to the range of the \code{predictor}
#'   observed within that level. This avoids extrapolating predictions beyond the available data for each subgroup.
#'   If \code{FALSE}, predictions span the entire range of the predictor across all levels of the \code{by} variable.
#'   If the \code{by} variable is numeric, \code{pred_range_limit} is automatically set to \code{FALSE},
#'   since numeric \code{by} values are treated as continuous rather than grouping factors.
#' @param pred_on_top Logical. If \code{TRUE}, prediction lines (and their confidence intervals) for numeric predictors are drawn after raw data,
#'   so they appear on top. Default is \code{FALSE}, which draws predictions underneath the data.
#'   This has no effect for categorical predictors — for those, predictions are always drawn on top of raw data.
#' @param pred_resolution Number of prediction points to use for numeric predictors.
#'   Defaults to \code{101}, consistent with \code{visreg}.
#'   The default should work well in most cases. Increasing \code{pred_resolution}
#'   may be particularly helpful when the predictor spans a wide range
#'   or when visualizing nonlinear relationships (e.g., splines or polynomials),
#'   to ensure smooth and accurate rendering of the effect.
#'   \strong{Note:} A higher value may slightly increase computation time,
#'   especially when combined with many levels of a \code{by} variable.
#' @param num_conditioning How to condition non-target numeric predictors. Either \code{"median"} (default) or \code{"mean"}.
#'   This determines how numeric variables that are not directly plotted are held constant during prediction,
#'   while varying the predictor of interest.
#'   To fix specific variables at custom values instead, use the \code{fix_values} argument.
#' @param cat_conditioning How to condition non-target categorical predictors.
#'   Either \code{"mode"} (default) or \code{"reference"}. As for \code{"num_conditioning"},
#'   conditioning means holding these variables constant while varying the predictor of interest.
#'   If multiple levels are equally frequent when \code{"mode"} is selected, the level chosen will be the first in the factor's level order
#'   (which by default is alphabetical and typically coincides with the reference level, unless explicitly re-leveled).
#'   This behavior also applies to grouping variables used as random effects when \code{re_form = NULL}.
#'   To fix categorical variables (including grouping variables) at specific levels, use \code{fix_values}.
#' @param fix_values A named vector or named list specifying fixed values for one or more variables during prediction.
#'   Supports both numeric and categorical variables.
#'   For numeric variables, specify a fixed value (e.g., \code{fix_values = c(x = 1)}).
#'   For categorical variables, provide the desired level as a character string or factor (e.g., \code{fix_values = c(group = "levelA")}. 
#'   Multiple values should be provided as a list (e.g., \code{fix_values = list(x = 1, group = "levelA")}).
#'   This overrides the default conditioning behavior specified via \code{num_conditioning} and \code{cat_conditioning}.
#'   This argument also applies to \emph{grouping variables used as random effects}: when \code{re_form = NULL},
#'   predictions are conditional on the level specified in \code{fix_values};
#'   if not specified, the level is chosen based on \code{cat_conditioning}.
#'   This argument is also useful for \emph{setting offset variables}. For count models with a log link in which the offset is specified
#'   as \code{offset(log(exposure))}, \code{easyViz} interprets the model as a rate model and, by default, fixes the exposure
#'   variable to 1 in the prediction grid. Raw response values are correspondingly scaled by the exposure so that both data
#'   points and predictions are displayed on the same unit-rate scale (e.g., detections per day; see \code{show_data_points} for more details).
#'   To obtain predictions on a different rate scale, fix the exposure at the desired value using \code{fix_values}.
#'   If the offset is specified outside the model formula, for \code{gam()} and \code{glmer()} models
#'   it is treated as \code{offset = 0} during prediction (i.e., exposure = 1 for log-link models).
#'   This matches \code{easyViz}'s default behavior, but in such cases \code{easyViz} cannot vary the exposure.
#'   Include the offset inside the formula (e.g., \code{offset(log(exposure))}) to enable full control.
#'   If the default behavior is not desired (i.e., if you do not want automatic rate standardization or fixing exposure to 1),
#'   you may still use an offset by specifying a pre-transformed exposure variable
#'   (e.g., \code{log_exposure = log(exposure)} and \code{offset(log_exposure)}).
#'   In this case, \code{easyViz} treats the offset as a generic additive term on the link scale.
#'   Model predictions are computed correctly, but the offset variable is conditioned using the
#'   default rules for numeric covariates (see \code{num_conditioning}) or values supplied via \code{fix_values}.
#'   Because the exposure structure is no longer explicit, raw data are plotted on the original
#'   response scale and no automatic rate standardization is applied.
#'   \strong{Additional uses:} \code{fix_values} is useful also for forcing predictions at specific values or ensuring
#'   consistent conditioning across models, for example when you want to visualize the effect of a predictor at
#'   a specific level of an interacting variable, without conditioning on all levels.
#'   E.g., to plot the conditional effect of a continuous predictor \code{x1}
#'   at a specific value of another variable \code{x2} (numeric or categorical), simply set
#'   \code{fix_values = c(x2 = ...)} and omit the \code{by} argument.
#'   This creates a clean single-effect plot for \code{x1} at the desired level of \code{x2},
#'   without plotting multiple lines or groups as \code{by} would.
#'   This argument can also be used to visualize three-way interactions when combined with \code{by}.
#'   See the \code{by} argument for details, and the Examples section for a demonstration of how to apply this approach.
#' @param re_form A formula specifying which random effects to include when generating predictions:
#'   \itemize{
#'     \item {\code{re_form = NULL} (default):} produces group-specific predictions, conditional on the random-effect levels present in the data.
#'           By default, \code{easyViz} fixes grouping variables at their mode (i.e., the most frequent level), so the prediction reflects the
#'           conditional estimate for that group. You can override this by explicitly fixing the grouping variable via \code{fix_values}
#'           (e.g., \code{fix_values = c(group = "levelA")}). If all levels are equally frequent and no value is specified, the first level
#'           (in factor order) is used, which is usually alphabetical unless re-leveled. If \code{by} corresponds to a grouping variable used
#'           in a random effect, predictions are visualized for all group levels (i.e., conditional predictions).
#'     \item {\code{re_form = NA} or \code{re_form = ~0}:} produces population-level (i.e., marginal) predictions by excluding random effects from the prediction step.
#'           The random effects are still part of the fitted model and influence the estimation of fixed effects and their uncertainty,
#'           but they are not included when computing predicted values. This is equivalent to assuming random effects are zero —
#'           representing an "average" group or subject.
#'   }
#'   This argument is relevant for mixed-effects models only (e.g., from \code{lme4}, \code{glmmTMB}, or \code{mgcv::gam()}).
#'   For \code{mgcv::gam()} models, random effects can be modeled using smooth terms like \code{s(group, bs = "re")}.
#'   Although \code{predict.gam()} does not support a \code{re.form} argument, \code{easyViz} emulates its behavior:
#'   \code{re_form = NULL} includes random-effect smooths, while \code{re_form = NA} or \code{~0} excludes them via the \code{exclude} argument in \code{predict.gam()}.
#'   \strong{Note:} For models fitted with \code{lme4} (e.g., \code{lmer()}, \code{glmer()}), standard errors are not available when \code{re_form = NULL}.
#' @param rlm_vcov Robust variance type for \code{MASS::rlm} models.
#'   May be one of the sandwich types:
#'   \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"},
#'   \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}, \code{"const"}, \code{"HC"}.
#'   Alternatively, users may provide a covariance matrix (used directly), or
#'   a function \code{f(model)} returning a covariance matrix. Default is \code{"HC0"}.
#' @param backtransform_response A custom function to back-transform predictions for transformed response variables
#'   (e.g., \code{exp} for log-transformed responses, or \code{function(x) x^2} for square root-transformed responses).
#'   \strong{Note:} For log-transformed responses, recall that in lognormal models the mean on the original scale is
#'   not simply \code{exp(x)} due to Jensen's inequality. If you want the expected value of a lognormal response, use
#'   a function such as \code{function(x) exp(x + sigma2/2)}, where \code{sigma2} is the residual variance on the log scale
#'   (e.g., \code{sigma2 <- sigma(your.model)^2}).
#'   \strong{Note:} If you wish to model a transformed response, it is recommended to apply the transformation
#'   directly in the model formula (e.g., \code{log(y) ~ ...}), rather than modifying the response variable in the data set.
#'   This ensures that observed data points are correctly plotted on the original (back-transformed) scale.
#'   Otherwise, raw data and predicted values may not align properly in the plot.
#' @param xlim x-axis limits for the plot (e.g., \code{xlim = c(0, 10)}). Defaults to automatic scaling based on the data range.
#'   Applies to both numeric and categorical predictors.
#'   For categorical variables, x-axis positions are treated as integer values (e.g., 1, 2, ..., k),
#'   and adjusting \code{xlim} (e.g., \code{xlim = c(0.5, k + 0.5)}) can control spacing and margins around the plotted levels.
#' @param ylim y-axis limits for the plot (e.g., \code{ylim = c(10, 20)}). Defaults to automatic scaling based on the data and prediction range.
#' @param xlab x-axis labels (e.g., \code{xlab = "x"}). Defaults to \code{"predictor"}.
#' @param ylab y-axis labels (e.g., \code{ylab = "y"}). Defaults to \code{"response"}.
#' @param cat_labels Custom labels for levels of a categorical predictor (e.g., \code{cat_labels = c("Level A", "Level B", "Level C")}).
#' @param font_family Font family for the plot. E.g., \code{"sans"} (default), \code{"serif"}, \code{"mono"}.
#' @param las Text orientation for axis labels (default: \code{1}).
#' @param bty Box type around the plot. E.g., \code{"o"} (default), \code{"n"}, \code{"L"}.
#' @param plot_args A named list of additional graphical parameters passed to base R's \code{plot()} function.
#'   These arguments allow users to override default appearance settings in a flexible way.
#'   Common options include axis label size, color, label text, tick mark spacing, and coordinate scaling.
#'   \strong{Note:} Only arguments recognized by \code{plot.default()} are supported.
#'   Parameters that must be set via \code{par()} (such as \code{mar}, \code{oma}, \code{mfrow}, \code{mgp})
#'   are \emph{not} applied through \code{plot_args}. If you wish to adjust those settings,
#'   set them directly using \code{par()} before calling \code{easyViz()}.
#'   Many valid parameters are documented in both \code{?plot.default} and \code{?par}.
#'   In \code{plot_args}, they are passed to \code{plot()}, not to \code{par()}.
#'   Common \code{plot()} parameters you may override:
#'   \itemize{
#'     \item {Label/Text size and style:} \code{cex.lab}, \code{cex.axis}, \code{cex.main}, \code{font.lab}, \code{font.axis}, \code{font.main}.
#'     \item {Colors:} \code{col.lab}, \code{col.axis}, \code{col.main}, \code{col.sub}, \code{col}, \code{bg}, \code{fg}.
#'     \item {Label/Text content:} \code{xlab}, \code{ylab}, \code{main}, \code{sub}.
#'     \item {Box and axis rendering:} \code{bty}, \code{axes}, \code{frame.plot}, \code{ann}.
#'     \item {Coordinate settings and tick spacing:} \code{xlim}, \code{ylim}, \code{xaxs}, \code{yaxs}, \code{xaxp}, \code{yaxp}, \code{asp}, \code{xlog}, \code{ylog}.
#'   }
#'   For a full list of supported parameters, see \code{?plot.default} and \code{?par}.
#'   Example usage: \cr
#'   \code{plot_args = list(main = "Title", cex.lab = 1.2, col.axis = "gray40", xaxp = c(0, 10, 5))}.
#' @param show_data_points Logical. Whether to display raw data points (default: \code{TRUE}).
#'   For \emph{binomial models} where the response is expressed as \code{cbind(successes, failures)}
#'   or as a proportion \code{successes / trials}, the raw data points shown on the y-axis are
#'   plotted as proportions: \code{successes / (successes + failures)} or
#'   \code{successes / trials}, respectively.
#'   For \emph{count models with a log link} that include an offset specified as
#'   \code{offset(log(exposure))}, \code{easyViz} interprets the model as a rate model.
#'   Raw response values are rescaled as \code{(count / exposure) * exposure_ref} and, by default,
#'   \code{exposure_ref = 1}, so points are displayed on the unit-rate scale (e.g., detections per day).
#'   The prediction grid uses the same reference exposure value, ensuring that points and predictions
#'   are on the same scale. To use a different rate scale, set the exposure reference value via
#'   \code{fix_values} (e.g., \code{fix_values = c(exposure = 7)} for detections per 7 days).
#'   If this default behavior is not desired, the offset can instead be specified using a
#'   pre-transformed exposure variable (e.g., \code{log_exposure = log(exposure)} and
#'   \code{offset(log_exposure)} in the model formula). In this case, \code{easyViz} does not
#'   apply automatic rate standardization and treats the offset as a generic additive term
#'   on the link scale (see \code{fix_values} for details).
#'   For \emph{survival models} (\code{coxph}), raw data points are not displayed, because
#'   survival outcomes involve event times and censoring and are not directly
#'   comparable to the plotted linear predictor (or hazard ratio) scale.
#' @param binary_data_type For binary responses, how to display raw data points in the plot.
#'   Either \code{"plain"} (default), which plots each individual 0/1 observation as-is,
#'   or \code{"binned"}, which groups observations into intervals (bins) of the predictor and plots
#'   the proportion of 0s and 1s within each bin. This makes it easier to visualize trends in binary outcomes,
#'   especially when many points overlap.
#' @param bins Number of bins for displaying binary response raw data when \code{binary_data_type = "binned"} (default: \code{10}).
#' @param jitter_data_points Logical. If \code{TRUE}, raw data points are jittered horizontally
#'   to reduce overplotting. Applies to both categorical and numeric predictors.
#'   Default is \code{FALSE}. For categorical predictors, jittering helps distinguish overlapping points.
#'   For numeric predictors, it can be useful when many data points share the same x-value (e.g., integers or rounding).
#' @param point_col Point color for raw data (default: \code{rgb(0, 0, 0, alpha = 0.4)}).
#'   Can be specified as a color name (e.g., \code{"gray"}), an integer (e.g., \code{1}), 
#'   or an RGB (e.g., \code{rgb(0, 0, 0, alpha = 0.4)}) or hex string (e.g., \code{"#808080"}).
#'   When the focal predictor is numeric, raw data points are plotted at the observation level.
#'   This typically matters when visualizing interactions involving a grouping variable
#'   (via \code{by}). In this case, \code{point_col} must be either a single value
#'   (applied to all points) or a vector of length equal to the number of observations
#'   in the data supplied to \code{easyViz()} (e.g., generated via
#'   \code{ifelse(group == "levelA", "blue", "red")} or similar logic; see the Example section).
#'   When the focal predictor is categorical and points are plotted for different
#'   levels of a grouping variable (via \code{by}), \code{point_col} can be a vector of colors,
#'   with one color per group (e.g., \code{point_col = c("blue", "red")}; see the Example section).\cr
#'   \strong{Tip:} For large data sets with many overlapping data points,
#'   it is recommended to use semi-transparent colors to reduce overplotting.
#'   You can achieve this by setting a low alpha value (e.g., \code{rgb(1,0,0, alpha = 0.1}),
#'   or by using \code{adjustcolor()} with the argument \code{alpha.f}
#'   (e.g., \code{adjustcolor("red", alpha.f = 0.1)}).
#'   In such cases, consider setting \code{pred_on_top = TRUE} to ensure that prediction lines
#'   and confidence intervals remain clearly visible above the dense cloud of raw data points.
#' @param point_pch Point shape for raw data (default: \code{16}).
#'   Dynamic: accepts multiple values when points are plotted for different values/levels of a variable.
#'   The same grouping logic described for \code{point_col} applies.
#' @param point_cex Point size for raw data (default: \code{0.75}).
#'   Dynamic: accepts multiple values when points are plotted for different values/levels of a variable.
#'   The same grouping logic described for \code{point_col} applies.
#' @param pred_line_col Color of the predicted line for numeric predictors (default: \code{"black"}).
#'   Can be specified as a color name, number or RGB/hex string.
#'   Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param pred_line_lty Type of the predicted line for numeric predictors (default: \code{1}).
#'   Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param pred_line_lwd Width of the predicted line for numeric predictors (default: \code{2}).
#'   Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param ci_level Confidence level for the intervals (between 0 and 1). Defaults to \code{0.95}.
#'   For example, \code{ci_level = 0.85} plots 85 percent confidence intervals.
#' @param ci_type Type of confidence intervals for numeric predictors.
#'   Either \code{"polygon"} (default) to draw shaded confidence bands, \code{"lines"} to draw lines,
#'   or \code{NULL} to suppress confidence intervals for numeric predictors.
#'   \strong{Note:} \code{ci_type = NULL} does \emph{not} suppress confidence bars for categorical predictors;
#'   these are always shown unless manually suppressed via custom logic (e.g., by setting \code{ci_bar_lwd = 0}).
#' @param ci_polygon_col Color for confidence interval polygon (default: \code{"gray"}).
#'   Requires \code{ci_type = "polygon"}. Can be specified as a color name, number or RGB/hex string.
#'   Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")})
#'   when CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_polygon_alpha Numeric value between 0 and 1 controlling the transparency
#'   of confidence interval bands when \code{ci_type = "polygon"}.
#'   Default is \code{0.5}. Higher values make the band more opaque; lower values
#'   make it more transparent.
#' @param ci_line_col Color for confidence interval lines (default: \code{"black"}).
#'   Requires \code{ci_type = "lines"}. Can be specified as a color name, number or RGB/hex string.
#'   Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")})
#'   when CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_line_lty Type for confidence interval lines (default: \code{1}).
#'   Requires \code{ci_type = "lines"}. Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)})
#'   when CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_line_lwd Width for confidence interval lines (default: \code{1}).
#'   Requires \code{ci_type = "lines"}. Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)})
#'   when CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param pred_point_col Color for predicted point values of categorical predictors (default: \code{"black"}).
#'   Can be specified as a color name, number or RGB/hex string.
#'   When \code{by} is specified (interaction plots), \code{pred_point_col} may be a vector with
#'   one color per group (i.e., per level/value of \code{by}); the same group color is then used
#'   for predicted points across all levels of the focal predictor.
#' @param pred_point_pch Shape for predicted point values of categorical predictors (default: \code{16}).
#'   Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when points are plotted for an interaction (i.e., when \code{by} is specified).
#'   The same grouping logic described for \code{pred_point_col} applies.
#' @param pred_point_cex Size for predicted point values of categorical predictors (default: \code{1}).
#'   Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when points are plotted for an interaction (i.e., when \code{by} is specified).
#'   The same grouping logic described for \code{pred_point_col} applies.
#' @param ci_bar_col Color for confidence interval bars (default: \code{"black"}).
#'   Applies only when the predictor is categorical. Can be a single color (applied to all CI bars) or a vector of colors.
#'   When \code{by} is used, CI bars are drawn by looping first over the levels of the focal predictor and then over the levels 
#'   of the grouping variable. Colors are assigned following this order, so they may need to be repeated to match predictor levels within each group. 
#'   For example, with 2 levels of \code{x} and 2 groups for \code{by}, four CI bars are drawn, and a length-4 vector can be used to assign colors to each bar 
#'   (e.g., \code{c("blue","blue","red","red")}; see the Examples section).
#' @param ci_bar_lty Type for confidence interval bars (default: \code{1}). Applies only when the predictor is categorical.
#'   Follows the same assignment logic as \code{ci_bar_col}.
#' @param ci_bar_lwd Width for confidence interval bars (default: \code{1}). Applies only when the predictor is categorical.
#'   Follows the same assignment logic as \code{ci_bar_col}.
#'   To suppress confidence interval bars, set \code{ci_bar_lwd = 0} (line width of zero).
#' @param ci_bar_caps Size of the caps on confidence interval bars (default: \code{0.1}).
#'   Applies only when the predictor is categorical.
#'   Follows the same assignment logic as \code{ci_bar_col}.
#'   Increase for more visible caps, set to 0 to remove caps and draw plain vertical bars.
#' @param add_legend Logical. Whether to draw a legend for the \code{by} variable.
#'   By default, a legend is drawn automatically (i.e., \code{add_legend = TRUE}) when \code{by} is supplied and omitted otherwise.
#'   Set \code{add_legend = FALSE} to suppress the legend even when \code{by} is present.
#' @param legend_position Legend position. Either a named position string
#'   (\code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"},
#'   \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, \code{"bottomright"}),
#'   the special keyword \code{"out"}, or a numeric vector \code{c(x, y)} specifying
#'   exact coordinates for manual placement.
#'   When a \code{by} variable is specified, a legend is drawn automatically with default
#'   position \code{"out"}, i.e., a horizontal legend is drawn above the plotting region.
#'   All other values follow standard base R legend positioning rules.
#'   Advanced manual placement outside the plot region is possible by temporarily increasing margins with
#'   \code{par(mar = ...)} and/or allowing drawing outside the plot region with
#'   \code{par(xpd = TRUE)}, then adjusting the legend position using \code{inset} or
#'   explicit coordinates. For example, to place a legend to the right of the axes,
#'   you may need to increase the right margin (e.g., \code{par(mar = c(5, 4, 4, 8))})
#'   and set \code{par(xpd = TRUE)} before calling \code{easyViz()}.
#'   You can then fine-tune placement using \code{inset} via \code{legend_args}
#'   when \code{legend_position} is a keyword (i.e., \code{"topright"}, \code{"right"}, \code{"bottomright"}).
#'   If \code{legend_position} is given as explicit coordinates \code{c(x, y)},
#'   \code{inset} is not used because the legend is positioned directly at \code{(x, y)}.
#'   See the Examples section for demonstrations.
#' @param legend_horiz Logical. If \code{TRUE}, the legend is drawn horizontally (side-by-side).
#'   If \code{FALSE}, the legend is drawn vertically (stacked).
#'   \strong{Note:} when \code{legend_position = "out"}, \code{easyViz} may automatically draw
#'   the legend horizontally (and adjust legend settings) to improve readability, unless overridden
#'   by user-supplied \code{legend_args}.
#' @param legend_title Optional character string controlling the legend title.
#'   If \code{legend_title} is \emph{not specified}, and a \code{by} variable is present,
#'   \code{easyViz()} automatically uses the name of the \code{by} variable as the legend title,
#'   and legend labels correspond to the levels of \code{by} (e.g., \code{"A"}, \code{"B"}, \code{"C"}).
#'   If \code{legend_title} is explicitly set to \code{NULL}, no legend title is drawn,
#'   and legend labels revert to the verbose form \code{"by = level"} (e.g., \code{"group = A"}).
#'   If \code{legend_title} is a character string, it is used as the legend title,
#'   and legend labels correspond to the levels of \code{by}.
#'   In all cases, legend labels can be manually overridden using \code{legend_labels}.
#' @param legend_labels Custom labels for the legend (e.g., \code{legend_labels = c("Level A", "Level B", "Level C")}).
#' @param legend_title_size Numeric. Text size for the legend title (default: \code{1}).
#' @param legend_label_size Numeric. Text size for the legend labels (default: \code{0.9}).
#' @param legend_args A named list of additional arguments passed to base R's \code{legend()} function.
#'   These allow fine-tuned control over the appearance and placement of the legend and override the high-level options
#'   provided by \code{legend_position}, \code{legend_title} and other \code{legend_*} arguments.
#'   For example, you can adjust the legend's box style, border color, spacing, point size, or background color.
#'   Common options include:
#'   \itemize{
#'     \item {Point and line appearance:} \code{pch}, \code{col}, \code{pt.cex}, \code{pt.lwd}, \code{lty}, \code{lwd}.
#'     \item {Layout and spacing:} \code{ncol}, \code{x.intersp}, \code{y.intersp}, \code{inset}, \code{xjust}, \code{yjust}.
#'     \item {Text style and color:} \code{cex}, \code{text.col}, \code{font}, \code{adj}.
#'     \item {Box and background:} \code{bty}, \code{box.lwd}, \code{box.col}, \code{bg}.
#'     \item {Title control:} \code{title}, \code{title.col}, \code{title.cex}, \code{title.adj}.
#'   }
#'   For a full list of supported parameters, see \code{?legend}.
#'   Example usage: \cr
#'   \code{legend_args = list(bty = "o", box.col = "black", pt.cex = 1.5)}.
#'   \strong{Tip:} Legends can be pushed outside the plotting region by combining
#'   \code{par(xpd = TRUE)} and wider margins (e.g., via \code{par(mar = ...)}),
#'   and by supplying appropriate coordinates or negative \code{inset} values through \code{legend_args}.
#' @param show_conditioning Logical. If \code{TRUE}, \code{easyViz} prints a
#'   concise summary in the R console describing how predictions are conditioned.
#'   The message reports:
#'   \itemize{
#'     \item Whether predictions from mixed-effects or GAM models are
#'           conditional on random effects (\code{re_form = NULL}) or represent
#'           marginal / population-level predictions (\code{re_form = NA} or \code{~0}).
#'     \item For numeric \code{by} variables, the values (e.g., quantiles or
#'           user-specified \code{by_breaks}) at which predictions are evaluated.
#'     \item Which variables are held fixed during prediction (and their values).
#'     \item Which variables vary across the prediction grid (typically the
#'           focal \code{predictor} and, if specified, the \code{by} variable).
#'   }
#'   This option does not affect the plot or returned values and is intended as
#'   a diagnostic aid to improve transparency and reproducibility.
#'   Default is \code{FALSE}.
#' @param plot Logical. If \code{TRUE} (default), \code{easyViz} produces a plot.
#'   If \code{FALSE}, no plot is drawn and the function only returns the
#'   predicted values (as an invisible \code{easyviz.pred.df} object). This is useful
#'   when you want to extract or store the predictions (e.g., in a data frame)
#'   without generating any graphical output.
#' 
#' @return A base R plot visualizing the conditional effect of a predictor on the
#'   response variable. Additionally, a data frame is invisibly returned containing
#'   the predictor values, conditioning variables, predicted values (\code{fit}),
#'   and lower and upper confidence limits. The confidence interval columns are
#'   labeled according to the specified level (e.g., \code{95lcl} and \code{95ucl}
#'   for \code{ci_level = 0.95}).
#'   To extract prediction data for further use (e.g., custom plotting or tabulation), assign the output to an object:
#'   \code{pred.df <- easyViz(...)}. You can then inspect it using \code{head(pred.df)} or save it with \code{write.csv(pred.df, ...)}.
#'
#' @details
#'   This function provides an easy-to-use yet highly flexible tool for visualizing conditional effects
#'   from a wide range of regression models, including mixed-effects and generalized additive (mixed) models.
#'   Compatible model types include \code{lm}, \code{rlm}, \code{glm}, \code{glm.nb}, \code{betareg}, and \code{mgcv::gam};
#'   nonlinear models via \code{nls}; generalized least squares via \code{gls}; survival models via \code{survival::coxph}.
#'   Mixed-effects models with random intercepts and/or slopes can be fitted using \code{lmer}, \code{glmer}, \code{glmer.nb},
#'   \code{glmmTMB}, or \code{mgcv::gam} (via smooth terms).
#'   The function handles nonlinear relationships (e.g., splines, polynomials), two-way interactions,
#'   and supports visualization of three-way interactions via conditional plots.
#'   Plots are rendered using base R graphics with extensive customization options available through the \code{plot_args} and
#'   \code{legend_args} argument. Users can pass any valid graphical parameters accepted by \code{plot}, \code{par} or \code{legend}
#'   enabling full control over axis/legend labels, font styles, colors, margins, and more.
#'
#'   \strong{Tip:} To customize plot appearance, look for argument names by prefix.
#'   Arguments starting with \code{point_} control the appearance of raw data.
#'   Arguments starting with \code{pred_} control the appearance of predicted values (lines or points).
#'   Arguments starting with \code{ci_} adjust the display of confidence intervals (polygons, lines or bars).
#'   Arguments starting with \code{legend_} control the appearance of the legend.
#'   This naming convention simplifies styling: just type the prefix (\code{point}, \code{pred}, \code{ci}, or \code{legend})
#'   to discover relevant arguments.
#'
#'   The arguments \code{model}, \code{data}, and \code{predictor} are required.
#'   The function will return an error if any of them is missing or invalid.
#'
#' @examples
#' #------------------------------------------
#' # Load required packages
#' #------------------------------------------
#'
#' library(MASS)
#' library(nlme)
#' library(betareg)
#' library(survival)
#' library(lme4)
#' library(glmmTMB)
#' library(mgcv)
#'
#' #------------------------------------------
#' # Simulate dataset
#' #------------------------------------------
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- runif(n, 0, 5)
#' x4 <- factor(sample(letters[1:3], n, replace = TRUE))
#' group_levels <- paste0("G", 1:10)
#' group <- factor(sample(group_levels, n, replace = TRUE))
#'
#' # Generate random intercepts for each group
#' group_effects <- rnorm(length(group_levels), mean = 0, sd = 2)  # non-zero variance
#' names(group_effects) <- group_levels
#' group_intercept <- group_effects[as.character(group)]
#'
#' # Non-linear continuous response
#' true_y <- 5 * sin(x3) + 3 * x1 + group_intercept + model.matrix(~x4)[, -1] %*% c(2, -2)
#' noise <- rnorm(n, sd = 3)
#' y <- as.vector(true_y + noise)
#'
#' # Binary response with group effect added to logit
#' logit_p <- 2 * x1 - 1 + group_intercept
#' p <- 1 / (1 + exp(-logit_p))
#' binary_y <- rbinom(n, size = 1, prob = p)
#'
#' # Binomial response: number of successes and failures
#' y3 <- sample(10:30, n, replace = TRUE)
#' logit_p_prop <- -1.5 * scale(x1)
#' p_prop <- 1 / (1 + exp(-logit_p_prop))
#' y1 <- rbinom(n, size = y3, prob = p_prop) # successes
#' y2 <- y3 - y1  # failures
#'
#' # Count response with group effect in log(mu)
#' mu_count <- exp(1 + 0.8 * x2 - 0.5 * (x4 == "b") + group_intercept)
#' size <- 1.2
#' count_y <- rnbinom(n, size = size, mu = mu_count)
#' # Offset variable
#' exposure <- runif(n, 1, 10)
#'
#' # Assemble dataset
#' sim.data <- data.frame(x1, x2, x3, x4, group, y, binary_y, y1, y2, y3, count_y, exposure)
#'
#' #------------------------------------------
#' # 1. Linear model (lm)
#' #------------------------------------------
#' mod.lm <- lm(y ~ x1 + x4,
#'              data = sim.data)
#' easyViz(model = mod.lm, data = sim.data, predictor = "x1",
#'         by = "x4",
#'         pred_range_limit = FALSE,
#'         pred_on_top = TRUE,
#'         ylim = c(-12,18),
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         point_col = ifelse(sim.data$x4=="a", "red",
#'                            ifelse(sim.data$x4=="b", "orange",
#'                                   "yellow")),
#'         point_cex = 0.5,
#'         pred_line_col = c("red", "orange", "yellow"),
#'         pred_line_lty = 1,
#'         ci_polygon_col = c(rgb(1,0,0,0.5),
#'                            rgb(1,0.5,0,0.5),
#'                            rgb(1,1,0,0.5)))
#'
#' mod.lm2 <- lm(sqrt(x3) ~ x1 * x4,
#'               data = sim.data)
#' easyViz(model = mod.lm2, data = sim.data, predictor = "x1",
#'         by="x4",
#'         backtransform_response = function(x) x^2,
#'         ylim = c(0,8),
#'         show_data_points = FALSE,
#'         show_conditioning = TRUE)
#'
#' mod.lm3 <- lm(y ~ poly(x3, 3),
#'               data = sim.data)
#' easyViz(model = mod.lm3, data = sim.data, predictor = "x3",
#'         pred_on_top = TRUE,
#'         font_family = "mono",
#'         point_col = rgb(1,0,0,0.3),
#'         point_pch = "+",
#'         ci_level = 0.85,
#'         ci_type = "lines",
#'         ci_line_lty = 2)
#'
#' # Extract prediction data
#' pred.df <- easyViz(model = mod.lm, data = sim.data, predictor = "x1",
#'                    by = "x4", ci_level = 0.85, plot = FALSE)
#' head(pred.df)
#'
#' #------------------------------------------
#' # 2. Robust linear model (rlm)
#' #------------------------------------------
#' mod.rlm <- rlm(y ~ x1 + x4,
#'                data = sim.data)
#' old_xpd_mar <- par(xpd = TRUE, mar = c(5.1, 4.1, 4.1, 5.1))
#' easyViz(model = mod.rlm, data = sim.data, predictor = "x1",
#'         by = "x4",
#'         pred_on_top = TRUE,
#'         bty = "n",
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         point_col = ifelse(sim.data$x4=="a", "red",
#'                            ifelse(sim.data$x4=="b", "orange",
#'                                   "yellow")),
#'         point_cex = 0.5,
#'         pred_line_col = c("red", "orange", "yellow"),
#'         pred_line_lty = 1,
#'         ci_polygon_col = c(rgb(1,0,0),
#'                            rgb(1,0.5,0),
#'                            rgb(1,1,0)),
#'         ci_polygon_alpha = 0.4,                    
#'         legend_position = c(2.25,13),
#'         legend_title = "Predictor x4",
#'         legend_title_size = 0.9,
#'         legend_args = list(legend = c("a", "b", "c",
#'                                       "a", "b", "c"),
#'                            col = c("red", "orange", "yellow",
#'                                    "red", "orange", "yellow"),
#'                            lty = c(1, 1, 1, NA, NA, NA), 
#'                            lwd = c(2, 2, 2, NA, NA, NA), 
#'                            pch = c(NA, NA, NA, 16, 16, 15), 
#'                            cex = 0.75,
#'                            bty = "n"))
#' par(old_xpd_mar)
#'
#' #------------------------------------------
#' # 3. Generalized least squares (gls)
#' #------------------------------------------
#' mod.gls <- gls(y ~ x1 + x2 + x4,
#'                correlation = corAR1(form = ~1|group),
#'                data = sim.data)
#' easyViz(model = mod.gls, data = sim.data, predictor = "x4",
#'         jitter_data_points = TRUE,
#'         bty = "n",
#'         xlab = "Predictor x4",
#'         ylab = "Response y",
#'         point_col = rgb(0,0,1,0.2),
#'         pred_point_col = "blue",
#'         cat_labels = c("group A", "group B", "group C"))
#'
#' sim.data$x5 <- sample(c(rep("CatA", 50), rep("CatB", 50)))
#' mod.gls2 <- gls(y ~ x1 + x2 + x4 * x5,
#'                 correlation = corAR1(form = ~1|group),
#'                 data = sim.data)
#' old_xpd_mar <- par(xpd = TRUE, mar = c(5.1, 4.1, 4.1, 5.1))
#' easyViz(model = mod.gls2, data = sim.data, predictor = "x4",
#'         by = "x5",
#'         jitter_data_points = TRUE,
#'         bty = "n",
#'         ylim = c(-15,15),
#'         xlab = "Predictor x4",
#'         ylab = "Response y",
#'         cat_labels = c("group A", "group B", "group C"),
#'         point_col = c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)),
#'         pred_point_col = c("blue", "red"),
#'         ci_bar_col = c("blue", "blue", "blue", "red", "red", "red"),
#'         ci_bar_caps = 0,
#'         legend_position = "topright",
#'         legend_args = list(title = "Predictor x5",
#'                            title.cex = 1,
#'                            legend = c("A", "B"),
#'                            pt.cex = 1.25,
#'                            horiz = TRUE,
#'                            inset = c(-0.2, 0)))
#' par(old_xpd_mar)
#'
#' #------------------------------------------
#' # 4. Nonlinear least squares (nls)
#' #------------------------------------------
#' mod.nls <- nls(y ~ a * sin(b * x3) + c,
#'                data = sim.data,
#'                start = list(a = 5, b = 1, c = 0))
#' summary(mod.nls)
#' easyViz(model = mod.nls, data = sim.data, predictor = "x3",
#'         pred_on_top = TRUE,
#'         font_family = "serif",
#'         bty = "n",
#'         xlab = "Predictor x3",
#'         ylab = "Response y",
#'         point_col = rgb(0,1,0,0.7),
#'         point_pch = 1,
#'         ci_type = "lines",
#'         ci_line_col = "black",
#'         ci_line_lty = 2)
#' text(x = 2.5, y = 11,
#'      labels = expression(Y %~% 5.31584 %*% sin(1.08158 %*% X[3]) + 0.51338),
#'      cex = 0.7)
#'
#' #------------------------------------------
#' # 5. Generalized linear model (glm)
#' #------------------------------------------
#' mod.glm <- glm(binary_y ~ x1 + x4 + offset(log(exposure)),
#'                family = binomial(link="cloglog"),
#'                data = sim.data)
#' easyViz(model = mod.glm, data = sim.data, predictor = "x1",
#'         fix_values = list(x4="b", exposure=1),
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         binary_data_type = "binned",
#'         point_col = "black",
#'         ci_polygon_col = "red",
#'         ci_polygon_alpha = 1)
#'
#' easyViz(model = mod.glm, data = sim.data, predictor = "x4",
#'         bty = "n",
#'         xlab = "Predictor x4",
#'         ylab = "Response y",
#'         binary_data_type = "plain",
#'         jitter_data_points = TRUE,
#'         point_col = "black",
#'         point_pch = "|",
#'         point_cex = 0.5)
#'
#' mod.glm2 <- glm(y1/y3 ~ x1 + x4, weights = y3,
#'                 family = binomial(link="logit"),
#'                 data = sim.data)
#' easyViz(model = mod.glm2, data = sim.data, predictor = "x1",
#'         pred_on_top = TRUE,
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         point_col = "black",
#'         ci_polygon_col = "red")
#'
#' #------------------------------------------
#' # 6. Negative binomial GLM (glm.nb)
#' #------------------------------------------
#' mod.glm.nb <- glm.nb(count_y ~ x2 + offset(log(exposure)),
#'                      data = sim.data)
#' easyViz(model = mod.glm.nb, data = sim.data, predictor = "x2",
#'         font_family = "mono",
#'         bty = "L",
#'         plot_args = list(main = "NB model"),
#'         xlab = "Predictor x2",
#'         ylab = "Response y",
#'         ci_polygon_col = "blue")
#'
#' #------------------------------------------
#' # 7. Beta regression (betareg)
#' #------------------------------------------
#' sim.data$prop <- y1/y3
#' mod.betareg <- betareg(prop ~ x1 * x4, offset = log(exposure),
#'                        data = sim.data, link= "cloglog")
#' easyViz(model = mod.betareg, data = sim.data, predictor = "x1",
#'         fix_values = c(exposure = 6),
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         ci_polygon_col = "forestgreen",
#'         show_conditioning = TRUE)
#'
#' #------------------------------------------
#' # 8. Survival model (coxph)
#' #------------------------------------------
#' mod.surviv <- coxph(Surv(y3, binary_y) ~ poly(x1,2) + x4, data=sim.data)
#' easyViz(model = mod.surviv, data = sim.data, predictor = "x1",
#'         pred_type = "link",
#'         xlab = "Predictor x1",
#'         ci_polygon_col = "orange2",
#'         ci_polygon_alpha = 1,
#'         show_conditioning = TRUE)
#'         
#' #------------------------------------------
#' # 9. Linear mixed model (lmer)
#' #------------------------------------------
#' mod.lmer <- lmer(y ~ x1 + x4 + (1 | group),
#'                  data = sim.data)
#' easyViz(model = mod.lmer, data = sim.data, predictor = "x1",
#'         by="group",
#'         re_form = NULL,
#'         bty = "n",
#'         plot_args = list(xaxp = c(round(min(sim.data$x1),1),
#'                                   round(max(sim.data$x1),1), 5)),
#'         ylim = c(-15, 15),
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         pred_line_col = "green",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 1,
#'         add_legend = FALSE)
#' old_new <- par(new = TRUE)
#' easyViz(model = mod.lmer, data = sim.data, predictor = "x1",
#'         re_form = NA,
#'         bty = "n",
#'         plot_args = list(xaxp = c(round(min(sim.data$x1),1),
#'                                   round(max(sim.data$x1),1), 5)),
#'         show_data_points = FALSE,
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         ylim = c(-15, 15),
#'         pred_line_col = "red",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 2,
#'         ci_type = NULL)
#' par(old_new)
#'
#' #------------------------------------------
#' # 10. Generalized linear mixed model (glmer)
#' #------------------------------------------
#' mod.glmer <- glmer(binary_y ~ x1 + x4 + (1 | group),
#'                    family = binomial,
#'                    data = sim.data)
#' easyViz(model = mod.glmer, data = sim.data, predictor = "x1",
#'         by = "group",
#'         re_form = NULL,
#'         cat_conditioning = "reference",
#'         font_family = "serif",
#'         xlab = "Predictor x1",
#'         ylab = "Response y",
#'         binary_data_type = "binned",
#'         pred_range_limit = FALSE,
#'         pred_line_col = "blue",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 1,
#'         add_legend = FALSE)
#'
#' #------------------------------------------
#' # 11. GLMM with negative binomial (glmer.nb)
#' #------------------------------------------
#' mod.glmer.nb <- glmer.nb(count_y ~ x2 + x4 + (1 | group),
#'                          data = sim.data)
#' easyViz(model = mod.glmer.nb, data = sim.data, predictor = "x2",
#'         re_form = NA,
#'         bty = "n",
#'         xlab = "Predictor x2",
#'         ylab = "Response y",
#'         ylim = c(0, 120),
#'         point_pch = 1)
#'
#' #------------------------------------------
#' # 12. GLMM (glmmTMB)
#' #------------------------------------------
#' mod.glmmTMB <- glmmTMB(count_y ~ x2 + x4 + (1 | group),
#'                        ziformula = ~ x2,
#'                        family = nbinom2,
#'                        data = sim.data)
#' easyViz(model = mod.glmmTMB, data = sim.data, predictor = "x2",
#'         re_form = NA,
#'         bty = "n",
#'         xlab = "Predictor x2",
#'         ylab = "Response y",
#'         ylim = c(0, 120),
#'         point_pch = 1,
#'         ci_type = NULL)
#'
#' #------------------------------------------
#' # 13. GAM (mgcv::gam) with random smooth
#' #------------------------------------------
#' mod.gam <- gam(y ~ s(x3) + s(group, bs = "re"),
#'                data = sim.data)
#' easyViz(model = mod.gam, data = sim.data, predictor = "x3",
#'         re_form = NA,
#'         las = 0,
#'         plot_args = list(xlab = "", ylab = "", axes = FALSE),
#'         bty = "n",
#'         xlab = "Predictor x3",
#'         ylab = "Response y",
#'         point_col = "black",
#'         point_pch = 1,
#'         ci_level = 0.99,
#'         ci_polygon_alpha = 0.25,
#'         ci_polygon_col = "red")
#' old_new <- par(new = TRUE)
#' easyViz(model = mod.gam, data = sim.data, predictor = "x3",
#'         re_form = NA,
#'         las = 0,
#'         plot_args = list(xlab = "", ylab = "", axes = FALSE),
#'         bty = "n",
#'         xlab = "Predictor x3",
#'         ylab = "Response y",
#'         point_col = "black",
#'         point_pch = 1,
#'         ci_level = 0.95,
#'         ci_polygon_alpha = 0.5,
#'         ci_polygon_col = "red")
#' par(old_new)
#' old_new <- par(new = TRUE)
#' easyViz(model = mod.gam, data = sim.data, predictor = "x3",
#'         re_form = NA,
#'         las = 0,
#'         bty = "n",
#'         xlab = "Predictor x3",
#'         ylab = "Response y",
#'         point_col = "black",
#'         point_pch = 1,
#'         ci_polygon_alpha = 1,
#'         ci_level = 0.8,
#'         ci_polygon_col = "red")
#' par(old_new)
#' rect(3.5,9,4,9.5, col=adjustcolor("red", alpha.f = 0.25), border=FALSE)
#' rect(3.5,7.5,4,8, col=adjustcolor("red", alpha.f = 0.5), border=FALSE)
#' rect(3.5,6,4,6.5, col=adjustcolor("red", alpha.f = 1), border=FALSE)
#' text(c(4.4, 4.4, 4.4), c(9.25, 7.75, 6.25), c("99% CI", "95% CI", "80% CI"), cex=0.75)
#'
#' #------------------------------------------
#' # 14. Plotting 3-way interaction
#' #------------------------------------------
#' mod.lm.int <- lm(y ~ x1 * x2 * x3,
#'                  data = sim.data)
#'
#' # Check conditional values to use for plotting
#' quantile(x2, c(0.1,0.5, 0.9))
#' quantile(x3, c(0.1,0.5, 0.9))
#'
#' # (optional) Generate a customizable function to add a strip label at the top
#' add_strip_label <- function(label, bg = "grey90", cex = 1, font = 2, height_mult = 2.5) {
#'   usr <- par("usr")
#'   x_left <- usr[1]
#'   x_right <- usr[2]
#'   y_top <- usr[4]
#'   # Estimate strip height using text height
#'   h <- strheight(label, cex = cex) * height_mult
#'   # Strip coordinates (extending above the plotting region)
#'   y_bottom <- y_top + 0.2 * h
#'   y_top_box <- y_bottom + h
#'   # Draw the full-width strip
#'   rect(x_left, y_bottom, x_right, y_top_box, col = bg, border = "black", xpd = NA)
#'   # Add centered text
#'   text(x = mean(c(x_left, x_right)),
#'        y = mean(c(y_bottom, y_top_box)),
#'        labels = label, cex = cex, font = font, xpd = NA)
#' }
#'
#' # par settings for multi-panel plot
#' old_mfrow <- par(mfrow = c(1, 3))
#' old_oma <- par(oma = c(4, 4, 2, 1))
#' old_mar <- par(mar = c(0, 0, 2, 0))
#'
#' # Panel 1
#' easyViz(model = mod.lm.int, data = sim.data, predictor = "x1",
#'         by = "x2",
#'         fix_values = c(x3 = 0.5750978),
#'         plot_args = list(xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4),
#'         legend_position = "topleft",
#'         legend_title = NULL,
#'         legend_labels = c("x2 = -1.3", "x2 = -0.2", "x2 = 1.5"))
#' add_strip_label("x3 = 0.6")
#' mtext("Response y", side = 2, outer = TRUE, line = 2.5)
#'
#' # Panel 2
#' easyViz(model = mod.lm.int, data = sim.data, predictor = "x1",
#'         by = "x2",
#'         fix_values = c(x3 = 2.3095046),
#'         plot_args = list(yaxt = "n", xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4),
#'         add_legend = FALSE)
#' add_strip_label("x3 = 2.3")
#'
#' # Panel 3
#' easyViz(model = mod.lm.int, data = sim.data, predictor = "x1",
#'         by = "x2",
#'         fix_values = c(x3 = 4.4509078),
#'         plot_args = list(yaxt = "n", xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4),
#'         add_legend = FALSE)
#' add_strip_label("x3 = 4.5")
#' mtext("Predictor x1", side = 1, outer = TRUE, line = 2.5)
#'
#' # Restore original settings
#' par(old_mfrow)
#' par(old_oma)
#' par(old_mar)
#'
#' #-------------END OF EXAMPLES--------------
#'
#' @importFrom stats formula family glm gaussian plogis pnorm pcauchy predict quantile
#' model.frame median complete.cases coef model.matrix vcov getCall terms df.residual
#' nobs qt qnorm reformulate na.pass .getXlevels delete.response drop.terms
#' @importFrom utils modifyList head tail
#' @importFrom graphics par plot polygon lines points axis legend arrows
#' @importFrom grDevices col2rgb rgb adjustcolor
#'
#' @export
easyViz <- function(model,
                    data,
                    predictor,
                    by = NULL,
                    by_breaks = NULL,
                    pred_type = "response",
                    pred_range_limit = TRUE,
                    pred_on_top = FALSE,
                    pred_resolution = 101,
                    num_conditioning = "median",
                    cat_conditioning = "mode",
                    fix_values = NULL,
                    re_form = NULL,
                    rlm_vcov = "HC0",
                    backtransform_response = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    cat_labels = NULL,
                    font_family = "sans",
                    las = 1,
                    bty = "o",
                    plot_args = list(),
                    show_data_points = TRUE,
                    binary_data_type = "plain",
                    bins = 10,
                    jitter_data_points = FALSE,
                    point_col = rgb(0,0,0,alpha = 0.4),
                    point_pch = 16,
                    point_cex = 0.75,
                    pred_line_col = "black",
                    pred_line_lty = c(1,2,3,4,5,6),
                    pred_line_lwd = 2,
                    ci_level = 0.95,
                    ci_type = "polygon",
                    ci_polygon_col = c("gray", "black", "lightgray", "darkgray","gray", "black"),
                    ci_polygon_alpha = 0.5,
                    ci_line_col = "black",
                    ci_line_lty = c(1,2,3,4,5,6),
                    ci_line_lwd = 1,
                    pred_point_col = c("black", "gray", "darkgray", "lightgray","black", "gray"),
                    pred_point_pch = 16,
                    pred_point_cex = 1,
                    ci_bar_col = "black",
                    ci_bar_lty = 1,
                    ci_bar_lwd = 1,
                    ci_bar_caps = 0.1,
                    add_legend = NULL,
                    legend_position = "out",
                    legend_horiz = FALSE,
                    legend_title = NULL,
                    legend_labels = NULL,
                    legend_title_size = 1,
                    legend_label_size = 0.9,
                    legend_args = list(),
                    show_conditioning = FALSE,
                    plot = TRUE) {

  # Check required arguments
  if (missing(model)) stop("Argument 'model' is required.", call. = FALSE)
  if (missing(data)) stop("Argument 'data' is required.", call. = FALSE)
  if (missing(predictor)) stop("Argument 'predictor' is required.", call. = FALSE)
 
  mc <- match.call(expand.dots = FALSE)
  pred_range_limit_user <- "pred_range_limit" %in% names(mc)
  legend_title_user     <- "legend_title"     %in% names(mc)
  legend_labels_user    <- "legend_labels"    %in% names(mc)
  ylab_user             <- "ylab"             %in% names(mc)
  
  # add_legend: NULL = auto, TRUE/FALSE = user override
  if (is.null(add_legend)) add_legend <- !is.null(by)

  # 1. Prepare data & meta-information
  prep <- ez_prepare_data(
    model = model,
    data = data,
    predictor = predictor,
    by = by,
    by_breaks = by_breaks,
    num_conditioning = num_conditioning,
    cat_conditioning = cat_conditioning,
    fix_values = fix_values,
    show_data_points = show_data_points,
    point_col = point_col,
    point_pch = point_pch,
    point_cex = point_cex
  )

  # --- Default ylab for coxph (depends on pred_type) ---
  if (isTRUE(prep$model_type$is_coxph) && !ylab_user) {
    ylab <- if (identical(pred_type, "link")) "Log-hazard ratio" else "Hazard ratio"
  }
  
  # 2. Build newdata prediction grid
  nd_out <- ez_build_newdata(
    prep = prep,
    pred_resolution = pred_resolution,
    pred_range_limit = pred_range_limit,
    pred_range_limit_user = pred_range_limit_user,
    by_breaks = by_breaks
  )
  new_data <- nd_out$new_data
  
  
  # --- Legend defaults (must run AFTER ez_build_newdata()) ---
  if (!is.null(by) && isTRUE(add_legend)) {
    
    # default title only if user did NOT specify legend_title
    if (!legend_title_user) legend_title <- by
    
    # default labels only if user did NOT specify legend_labels
    if (!legend_labels_user) {
      base_labs <- as.character(nd_out$by_levels)
      
      # hard safety: if something went wrong, don't call legend()
      if (length(base_labs) == 0) {
        add_legend <- FALSE
      } else if (is.null(legend_title)) {
        legend_labels <- paste0(by, " = ", base_labs)
      } else {
        legend_labels <- base_labs
      }
    }
  }
  
  # 3. Compute link-scale predictions
  preds_link <- ez_compute_predictions(
    model = model,
    new_data = new_data,
    prep = prep,
    re_form = re_form,
    rlm_vcov = rlm_vcov
  )

  if (isTRUE(show_conditioning)) {
    ez_message_conditioning(
      model      = model,
      prep       = prep,
      new_data   = new_data,
      re_form    = re_form,
      by_breaks  = by_breaks,
      fix_values = fix_values
    )
  }
  
  # 4. Back-transform to response scale if requested
  preds <- ez_backtransform(
    preds_link = preds_link,
    model = model,
    prep = prep,
    pred_type = pred_type,
    backtransform_response = backtransform_response,
    ci_level = ci_level
  )

  # 5. Plot
  plot_obj <- NULL
  if (isTRUE(plot)) {
    plot_obj <- ez_plot_dispatch(
    prep = prep,
    new_data = new_data,
    preds = preds,
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    font_family = font_family,
    las = las,
    bty = bty,
    plot_args = plot_args,
    pred_on_top = pred_on_top,
    show_data_points = show_data_points,
    binary_data_type = binary_data_type,
    bins = bins,
    jitter_data_points = jitter_data_points,
    point_col = point_col,
    point_pch = point_pch,
    point_cex = point_cex,
    pred_line_col = pred_line_col,
    pred_line_lty = pred_line_lty,
    pred_line_lwd = pred_line_lwd,
    ci_type = ci_type,
    ci_polygon_col = ci_polygon_col,
    ci_polygon_alpha = ci_polygon_alpha,
    ci_line_col = ci_line_col,
    ci_line_lty = ci_line_lty,
    ci_line_lwd = ci_line_lwd,
    pred_point_col = pred_point_col,
    pred_point_pch = pred_point_pch,
    pred_point_cex = pred_point_cex,
    ci_bar_col = ci_bar_col,
    ci_bar_lty = ci_bar_lty,
    ci_bar_lwd = ci_bar_lwd,
    ci_bar_caps = ci_bar_caps,
    cat_labels = cat_labels,
    add_legend = add_legend,
    legend_position = legend_position,
    legend_horiz = legend_horiz,
    legend_title = legend_title,
    legend_labels = legend_labels,
    legend_title_size = legend_title_size,
    legend_label_size = legend_label_size,
    legend_args = legend_args
  )
  }
  # 6. Output
  pred.df <- ez_build_output(
    model = model,
    new_data = new_data,
    preds = preds,
    ci_level = ci_level,
    re_form = re_form
  )

  invisible(pred.df)
}
