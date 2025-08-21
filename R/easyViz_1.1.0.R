#' Easy Visualization of Conditional Effects from Regression Models
#'
#' \code{easyViz} offers a flexible and user-friendly interface for visualizing conditional effects 
#' from a broad range of regression and mixed-effects models using base R graphics.
#'
#' @param model [required] A fitted model object (e.g., \code{model = your.model}).
#' Supported models include a wide range of regression types, including linear, robust linear, nonlinear, generalized least squares,
#' generalized linear, mixed-effects, and generalized additive (mixed) models.
#' Compatible model-fitting functions include:
#' \code{stats::lm}, \code{MASS::rlm}, \code{stats::nls}, \code{nlme::gls}, \code{stats::glm}, \code{MASS::glm.nb},
#' \code{lme4::lmer}, \code{lme4::glmer}, \code{lme4::glmer.nb}, \code{glmmTMB::glmmTMB}, and \code{mgcv::gam}.
#' @param data [required] The data frame used to fit the model (e.g., \code{data = your.data}).
#' This data frame is used internally for generating predictions.
#' \strong{All variables used in the model formula (including predictors, offsets, grouping variables, and interaction terms) must be present in this data frame}.
#' If the model was fitted without using a \code{data} argument (e.g., using variables from the global environment),
#' you must ensure that \code{data} includes all required variables.
#' Otherwise, prediction may fail or produce incorrect results.
#' @param predictor [required] The name of the target explanatory variable to be plotted (e.g., \code{predictor = "x1"}).
#' @param by The name of an interaction or additional variable for conditioning (e.g., \code{by = "x2"}). 
#' If the variable is:
#' \itemize{
#'   \item {continuous}, predictions are shown for cross-sections at the 10th, 50th, and 90th percentiles.
#'   \item {categorical}, a separate prediction line or point will be plotted for each level.
#'   \item {used as a grouping variable} in a random effect term (e.g., \code{(1|group)} or \code{s(group, bs = "re")})
#'         and \code{re.form = NULL}, predictions are conditional on each group's estimated random effect.
#' }
#' Although \code{easyViz} does not natively support direct visualization of three-way interactions in a multi-panel plot,
#' this can be easily achieved by combining the \code{by} and \code{fix_values} arguments.
#' For example, if your model includes a term like \code{x1*x2*x3}, you can visualize the effect of \code{x1}
#' across levels of \code{x2} by setting \code{predictor = "x1"}, \code{by = "x2"}, and fixing \code{x3}
#' at a specific value using \code{fix_values = c(x3 = ...)}.
#' Repeating this with different values of \code{x3} produces multiple plots that can be arranged to visualize the full three-way interaction.
#' See the Examples section for a demonstration of how to apply this approach.
#' @param pred_type Character string indicating the type of predictions to plot. 
#' Either \code{"response"} (default), which returns predictions on the original outcome scale 
#' by applying the inverse of the model's link function (e.g., probabilities for binary models), 
#' or \code{"link"}, which returns predictions on the linear predictor (link) scale 
#' (e.g., log-odds, log-counts, or other transformed scales depending on the model).
#' @param pred_range_limit Logical. Applies only when the predictor is numeric and a categorical \code{by} variable is specified. 
#' If \code{TRUE} (default), the prediction range for each level of the \code{by} variable is limited to the range of the \code{predictor} 
#' observed within that level. This avoids extrapolating predictions beyond the available data for each subgroup. 
#' If \code{FALSE}, predictions span the entire range of the predictor across all levels of the \code{by} variable.
#' If the \code{by} variable is numeric, \code{pred_range_limit} is automatically set to \code{FALSE}, 
#' since numeric \code{by} values are treated as continuous rather than grouping factors.
#' @param pred_on_top Logical. If \code{TRUE}, prediction lines (and their confidence intervals) for numeric predictors are drawn after raw data, 
#' so they appear on top. Default is \code{FALSE}, which draws predictions underneath the data. 
#' This has no effect for categorical predictors — for those, predictions are always drawn on top of raw data.
#' @param pred_resolution Number of prediction points to use for numeric predictors.
#' Defaults to \code{101}, consistent with \code{visreg}.
#' The default should work well in most cases. Increasing \code{pred_resolution} 
#' may be particularly helpful when the predictor spans a wide range 
#' or when visualizing nonlinear relationships (e.g., splines or polynomials), 
#' to ensure smooth and accurate rendering of the effect.
#' \strong{Note:} A higher value may slightly increase computation time, 
#' especially when combined with many levels of a \code{by} variable.
#' @param num_conditioning How to condition non-target numeric predictors. Either \code{"median"} (default) or \code{"mean"}. 
#' This determines how numeric variables that are not directly plotted are held constant during prediction, 
#' while varying the predictor of interest — a common approach when visualizing effects in multivariable models. 
#' To fix specific variables at custom values instead, use the \code{fix_values} argument.
#' @param cat_conditioning How to condition non-target categorical predictors. 
#' Either \code{"mode"} (default) or \code{"reference"}. As for \code{"num_conditioning"}, 
#' conditioning means holding these variables constant while varying the predictor of interest.
#' If multiple levels are equally frequent when \code{"mode"} is selected, the level chosen will be the first in the factor's level order 
#' (which by default is alphabetical and typically coincides with the reference level, unless explicitly re-leveled).
#' This behavior also applies to grouping variables used as random effects when \code{re.form = NULL}.
#' To fix categorical variables (including grouping variables) at specific levels, use \code{fix_values}.
#' @param fix_values A named vector or named list specifying fixed values for one or more variables during prediction.
#' Supports both numeric and categorical variables.
#' For numeric variables, specify a fixed value (e.g., \code{fix_values = c(x = 1)}).
#' For categorical variables (factors), provide the desired level as a character string or factor 
#' (e.g., \code{fix_values = c(group = "levelA")} or \code{fix_values = list(group = levels(data$group)[1])}).
#' This overrides the default conditioning behavior specified via \code{num_conditioning} and \code{cat_conditioning}.
#' \strong{Note:} This argument also applies to grouping variables used as random effects: when \code{re.form = NULL}, 
#' predictions are conditional on the level specified in \code{fix_values}; 
#' if not specified, the level is chosen based on \code{cat_conditioning}.
#' This argument is useful for setting offsets, forcing predictions at specific values, or ensuring consistent conditioning across models.
#' For example, it is particularly useful when you want to visualize the effect of a predictor
#' at a specific level of an interacting variable, without conditioning on all levels.
#' E.g., to plot the conditional effect of a continuous predictor \code{x1} 
#' at a specific value of another variable \code{x2} (numeric or categorical), simply set
#' \code{fix_values = c(x2 = ...)} and omit the \code{by} argument. 
#' This creates a clean single-effect plot for \code{x1} at the desired level of \code{x2},
#' without plotting multiple lines or groups as \code{by} would.
#' This argument can also be used to visualize three-way interactions when combined with \code{by}.
#' See the \code{by} argument for details, and the Examples section for a demonstration of how to apply this approach.
#' @param re.form A formula specifying which random effects to include when generating predictions.
#' This argument is relevant for mixed-effects models only (e.g., from \code{lme4}, \code{glmmTMB}, or \code{mgcv::gam()}).
#' \itemize{
#'   \item {\code{re.form = NULL} (default):} produces group-specific predictions, conditional on the random-effect levels present in the data.
#'         By default, \code{easyViz} fixes grouping variables at their mode (i.e., the most frequent level), so the prediction reflects the 
#'         conditional estimate for that group. You can override this by explicitly fixing the grouping variable via \code{fix_values} 
#'         (e.g., \code{fix_values = c(group = "levelA")}). If all levels are equally frequent and no value is specified, the first level
#'         (in factor order) is used, which is usually alphabetical unless re-leveled. If \code{by} corresponds to a grouping variable used 
#'         in a random effect, predictions are visualized for all group levels (i.e., conditional predictions).
#'   \item {\code{re.form = NA} or \code{re.form = ~0}:} produces population-level (i.e., marginal) predictions by excluding random effects from the prediction step.
#'         The random effects are still part of the fitted model and influence the estimation of fixed effects and their uncertainty,
#'         but they are not included when computing predicted values. This is equivalent to assuming random effects are zero —
#'         representing an "average" group or subject.
#' }
#' For \code{mgcv::gam()} models, random effects are typically modeled using smooth terms like \code{s(group, bs = "re")}.
#' Although \code{predict.gam()} does not support a \code{re.form} argument, \code{easyViz} emulates its behavior:
#' \code{re.form = NULL} includes random-effect smooths, while \code{re.form = NA} or \code{~0} excludes them via the \code{exclude} argument in \code{predict.gam()}.
#' \strong{Note:} For models fitted with \code{lme4} (e.g., \code{lmer()}, \code{glmer()}), standard errors are not available when \code{re.form = NULL}.
#' @param backtransform_response A custom function to back-transform predictions for transformed response variables 
#' (e.g., \code{exp} for log-transformed responses, or \code{function(x) x^2} for square root-transformed responses).
#' \strong{Note:} If you wish to model a transformed response, it is recommended to apply the transformation 
#' directly in the model formula (e.g., \code{log(y)}), rather than modifying the response variable in the data set. 
#' This ensures that observed data points are correctly plotted on the original (back-transformed) scale. 
#' Otherwise, raw data and predicted values may not align properly in the plot.
#' @param xlim x-axis limits for the plot (e.g., \code{xlim = c(0, 10)}). Defaults to automatic scaling based on the data range.
#' Applies to both numeric and categorical predictors. 
#' For categorical variables, x-axis positions are treated as integer values (e.g., 1, 2, ..., k), 
#' and adjusting \code{xlim} (e.g., \code{xlim = c(0.5, k + 0.5)}) can control spacing and margins around the plotted levels. 
#' @param ylim y-axis limits for the plot (e.g., \code{ylim = c(10, 20)}). Defaults to automatic scaling based on the data and prediction range.
#' @param xlab x-axis labels (e.g., \code{xlab = "x"}). Defaults to \code{"predictor"}.
#' @param ylab y-axis labels (e.g., \code{ylab = "y"}). Defaults to \code{"response"}.
#' @param cat_labels Custom labels for levels of a categorical predictor (e.g., \code{cat_labels = c("Level A", "Level B", "Level C")}).
#' @param font_family Font family for the plot. E.g., \code{"sans"} (default), \code{"serif"}, \code{"mono"}.
#' @param las Text orientation for axis labels (default: \code{1}).
#' @param bty Box type around the plot. E.g., \code{"o"} (default), \code{"n"}, \code{"L"}.
#' @param plot_args A named list of additional graphical parameters passed to base R's \code{plot()} function.
#' These arguments allow users to override default appearance settings in a flexible way.
#' Common options include axis label size, color, label text, tick mark spacing, and coordinate scaling.
#' \strong{Note:} Only arguments recognized by \code{plot.default()} are supported.
#' Parameters that must be set via \code{par()} (such as \code{mar}, \code{oma}, \code{mfrow}, \code{mgp}) 
#' are \emph{not} applied through \code{plot_args}. If you wish to adjust those settings, 
#' set them directly using \code{par()} before calling \code{easyViz()}.
#' Many valid parameters are documented in both \code{?plot.default} and \code{?par}.
#' In \code{plot_args}, they are passed to \code{plot()}, not to \code{par()}.
#' Common \code{plot()} parameters you may override:
#' \itemize{
#'   \item {Label/Text size and style:} \code{cex.lab}, \code{cex.axis}, \code{cex.main}, \code{font.lab}, \code{font.axis}, \code{font.main}
#'   \item {Colors:} \code{col.lab}, \code{col.axis}, \code{col.main}, \code{col.sub}, \code{col}, \code{bg}, \code{fg}
#'   \item {Label/Text content:} \code{xlab}, \code{ylab}, \code{main}, \code{sub}
#'   \item {Box and axis rendering:} \code{bty}, \code{axes}, \code{frame.plot}, \code{ann}
#'   \item {Coordinate settings and tick spacing:} \code{xlim}, \code{ylim}, \code{xaxs}, \code{yaxs}, \code{xaxp}, \code{yaxp}, \code{asp}, \code{xlog}, \code{ylog}
#' }
#' For a full list of supported parameters, see \code{?plot.default} and \code{?par}.
#' Example usage: \cr
#' \code{plot_args = list(main = "Title", cex.lab = 1.2, col.axis = "gray40", xaxp = c(0, 10, 5))}.
#' @param show_data_points Logical. Whether to display raw data points (default: \code{TRUE}).
#' For binomial models where the response is expressed in the formula as \code{cbind(successes, failures)} or as \code{successes / trials},
#' the raw data points plotted on the y-axis are based on the calculated proportions:
#' \code{successes / (successes + failures)} or \code{successes / trials}, respectively.
#' These proportions are computed internally from the original data and temporarily added to the data set for visualization purposes.
#' @param binary_data_type For binary responses, how to display raw data points in the plot. 
#' Either \code{"plain"} (default), which plots each individual 0/1 observation as-is, 
#' or \code{"binned"}, which groups observations into intervals (bins) of the predictor and plots 
#' the proportion of 0s and 1s within each bin. This makes it easier to visualize trends in binary outcomes, 
#' especially when many points overlap.
#' @param bins Number of bins for displaying binary response raw data when \code{binary_data_type = "binned"} (default: \code{10}).
#' @param jitter_data_points Logical. If \code{TRUE}, raw data points are jittered horizontally 
#' to reduce overplotting. Applies to both categorical and numeric predictors. 
#' Default is \code{FALSE}. For categorical predictors, jittering helps distinguish overlapping points. 
#' For numeric predictors, it can be useful when many data points share the same x-value (e.g., integers or rounding).
#' @param point_col Point color for raw data (default: \code{rgb(0,0,0, alpha = 0.4)}). 
#' Can be specified as a color name (e.g., \code{"gray"}), 
#' an integer (e.g., \code{1}), or an RGB (e.g., \code{rgb(0,0,0,alpha=0.4)}) or hex string (e.g., \code{"#808080"}). 
#' Dynamic: accepts multiple values when points are plotted for different values/levels of a variable. \cr
#' \strong{Tip:} For large data sets with many overlapping data points, 
#' it is recommended to use semi-transparent colors to reduce overplotting. 
#' You can achieve this by setting a low alpha value (e.g., \code{rgb(1,0,0, alpha = 0.1}), 
#' or by using \code{adjustcolor()} with the argument \code{alpha.f} 
#' (e.g., \code{adjustcolor("red", alpha.f = 0.1)}). 
#' In such cases, consider setting \code{pred_on_top = TRUE} to ensure that prediction lines 
#' and confidence intervals remain clearly visible above the dense cloud of raw points.
#' @param point_pch Point shape for raw data (default: \code{16}). 
#' Dynamic: accepts multiple values when points are plotted for different values/levels of a variable.
#' @param point_cex Point size for raw data (default: \code{0.75}). 
#' Dynamic: accepts multiple values when points are plotted for different values/levels of a variable.
#' @param pred_line_col Color of the predicted line for numerical predictors (default: \code{"black"}). 
#' Can be specified as a color name, number or RGB/hex string. 
#' Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param pred_line_lty Type of the predicted line for numerical predictors (default: \code{1}). 
#' Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param pred_line_lwd Width of the predicted line for numerical predictors (default: \code{2}). 
#' Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when multiple lines are plotted (i.e., when \code{by} is specified).
#' @param ci_type Type of 95 percent confidence intervals for numeric predictors. 
#' Either \code{"polygon"} (default) to draw shaded confidence bands, \code{"lines"} to draw lines, 
#' or \code{NULL} to suppress confidence intervals for numeric predictors. 
#' \strong{Note:} \code{ci_type = NULL} does \emph{not} suppress confidence bars for categorical predictors; 
#' these are always shown unless manually suppressed via custom logic (e.g., by setting \code{ci_bar_lwd = 0}).
#' @param ci_polygon_col Color for 95 percent confidence interval polygon (default: \code{"gray"}). 
#' Requires \code{ci_type = "polygon"}. Can be specified as a color name, number or RGB/hex string. 
#' Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) 
#' when 95 percent CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_line_col Color for 95 percent confidence interval lines (default: \code{"black"}). 
#' Requires \code{ci_type = "lines"}. Can be specified as a color name, number or RGB/hex string. 
#' Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) 
#' when 95 percent CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_line_lty Type for 95 percent confidence interval lines (default: \code{1}). 
#' Requires \code{ci_type = "lines"}. Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) 
#' when 95 percent CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param ci_line_lwd Width for 95 percent confidence interval lines (default: \code{1}). 
#' Requires \code{ci_type = "lines"}. Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) 
#' when 95 percent CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' @param pred_point_col Color for predicted point values of categorical predictors (default: \code{"black"}). 
#' Can be specified as a color name, number or RGB/hex string. 
#' Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) when points are plotted for an interaction (i.e., when \code{by} is specified).
#' @param pred_point_pch Shape for predicted point values of categorical predictors (default: \code{16}). 
#' Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when points are plotted for an interaction (i.e., when \code{by} is specified).
#' @param pred_point_cex Size for predicted point values of categorical predictors (default: \code{1}). 
#' Dynamic: accepts multiple values (e.g., \code{c(1, 2, 3)}) when points are plotted for an interaction (i.e., when \code{by} is specified).
#' @param ci_bar_col Color for 95 percent confidence interval bars (default: \code{"black"}). 
#' Applies only when the predictor is categorical. Can be specified as a color name, number, or RGB/hex string. 
#' @param ci_bar_lty Type for 95 percent confidence interval bars (default: \code{1}). Applies only when the predictor is categorical.
#' @param ci_bar_lwd Width for 95 percent confidence interval bars (default: \code{1}). Applies only when the predictor is categorical.
#' To suppress confidence interval bars, set \code{ci_bar_lwd = 0} (line width of zero).
#' @param ci_bar_caps Size of the caps on 95 percent confidence interval bars (default: \code{0.1}).
#' Increase for more visible caps, set to 0 to remove caps and draw plain vertical bars.
#' @param add_legend Logical. Whether to add a legend for \code{by} variable levels (default: \code{FALSE}).
#' @param legend_position Legend position. Either a named position string (\code{"top"}, \code{"bottom"}, \code{"left"}, 
#' \code{"right"}, \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, \code{"bottomright"}), or a numeric vector 
#' \code{c(x, y)} specifying exact coordinates for manual placement.
#' In base \code{plot()} in R, there is no direct way to place the legend truly outside the plot area (e.g., to the right 
#' of the axis and tick marks). A workaround, especially effective when \code{bty = "n"}, is to expand the \code{xlim} 
#' or \code{ylim} range, optionally adjust tick spacing using \code{plot_args = list(xaxp = ...)} or \code{yaxp = ...}, 
#' and set \code{legend_position = c(x, y)} to manually locate the legend within the expanded plotting coordinates. 
#' When placing the legend in an expanded region, note that axis labels may become misaligned; for best results, suppress them 
#' with \code{xlab = ""} or \code{ylab = ""} and add them manually using \code{text()} after the plot is drawn.
#' See the Examples section for a demonstration of this approach.
#' Alternatively, you can enable drawing outside the plot region by setting \code{par(xpd = TRUE)} and increasing the 
#' plot margins using \code{par(mar = ...)}. In this case, the legend must be added manually \emph{after} the call to \code{easyViz()}, 
#' as it will fall outside the plot area managed by the function.
#' @param legend_title Optional character string. If specified, this will appear as the title of the legend. 
#' In this case, the legend labels will correspond to the levels of the \code{by} variable (e.g., "A", "B", "C"). 
#' If left as \code{NULL} (default), the legend labels will follow standard behavior (e.g., \code{"by = level"}). 
#' In any case, they can be manually specified via \code{legend_labels}.
#' @param legend_labels Custom labels for the legend (e.g., \code{legend_labels = c("Level A", "Level B", "Level C")}).
#' @param legend_title_size Numeric. Text size for the legend title (default: \code{1}).
#' @param legend_label_size Numeric. Text size for the legend labels (default: \code{0.9}).
#' @param legend_horiz Logical. If \code{TRUE}, the legend is drawn horizontally (side-by-side) 
#' instead of vertically (stacked). Defaults to \code{FALSE}. Useful for placing the legend 
#' above or below the plot in a compact layout.
#' @param legend_args A named list of additional arguments passed to base R's \code{legend()} function. 
#' These allow fine-tuned control over the appearance and placement of the legend and override the high-level options 
#' provided by \code{legend_position}, \code{legend_title} and other \code{legend_*} arguments.
#' For example, you can adjust the legend's box style, border color, spacing, point size, or background color.
#' Common options include:
#' \itemize{
#'   \item {Point and line appearance:} \code{pch}, \code{col}, \code{pt.cex}, \code{pt.lwd}, \code{lty}, \code{lwd}
#'   \item {Layout and spacing:} \code{ncol}, \code{x.intersp}, \code{y.intersp}, \code{inset}, \code{xjust}, \code{yjust}
#'   \item {Text style and color:} \code{cex}, \code{text.col}, \code{font}, \code{adj}
#'   \item {Box and background:} \code{bty}, \code{box.lwd}, \code{box.col}, \code{bg}
#'   \item {Title control:} \code{title}, \code{title.col}, \code{title.cex}, \code{title.adj}
#' }
#' For a full list of supported parameters, see \code{?legend}.
#' Example usage: \cr
#' \code{legend_args = list(bty = "o", box.col = "black", pt.cex = 1.5)}.
#' \strong{Tip:} If you're adding a legend outside the plot region using \code{par(xpd = TRUE)}, 
#' you must call \code{legend()} manually \emph{after} \code{easyViz()} to place it correctly.
#' Use \code{legend_args} only for legends drawn \emph{inside} the plot area.
#' 
#' @return A base R plot visualizing the conditional effect of a predictor on the response variable.
#' Additionally, a data frame is invisibly returned containing the predictor values, conditioning variables, 
#' predicted values (\code{fit}), and their 95 percent confidence intervals (\code{lower}, \code{upper}).
#' To extract prediction data for further use (e.g., custom plotting or tabulation), assign the output to an object:
#' \code{pred_df <- easyViz(...)}. You can then inspect it using \code{head(pred_df)} or save it with \code{write.csv(pred_df, ...)}.
#' 
#' @details
#' This function provides an easy-to-use yet highly flexible tool for visualizing conditional effects 
#' from a wide range of regression models, including mixed-effects and generalized additive (mixed) models. 
#' Compatible model types include \code{lm}, \code{rlm}, \code{glm}, \code{glm.nb}, and \code{mgcv::gam}; 
#' nonlinear models via \code{nls}; and generalized least squares via \code{gls}. 
#' Mixed-effects models with random intercepts and/or slopes can be fitted using \code{lmer}, \code{glmer}, \code{glmer.nb}, 
#' \code{glmmTMB}, or \code{mgcv::gam} (via smooth terms). 
#' The function handles nonlinear relationships (e.g., splines, polynomials), two-way interactions, 
#' and supports visualization of three-way interactions via conditional plots.
#' Plots are rendered using base R graphics with extensive customization options available through the \code{plot_args} and 
#' \code{legend_args} argument. Users can pass any valid graphical parameters accepted by \code{plot}, \code{par} or \code{legend}
#' enabling full control over axis/legend labels, font styles, colors, margins, and more.
#' 
#' \strong{Tip:} To customize plot appearance, look for argument names by prefix:  
#' Arguments starting with \code{point_} control the appearance of raw data.  
#' Arguments starting with \code{pred_} control the appearance of predicted values (lines or points).  
#' Arguments beginning with \code{ci_} adjust the display of confidence intervals (polygons, lines or bars).  
#' Arguments beginning with \code{legend_} control the appearance of the legend.  
#' This naming convention simplifies styling: just type the prefix (\code{point}, \code{pred}, \code{ci}, or \code{legend}) 
#' to discover relevant arguments.
#' 
#' The arguments \code{model}, \code{data}, and \code{predictor} are required.
#' The function will return an error if any of them is missing or invalid.
#' 
#' @examples
#' #------------------------------------------
#' # Load required packages
#' #------------------------------------------
#' 
#' library(nlme)
#' library(MASS)
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
#' offset_var <- log(runif(n, 1, 10))
#' 
#' # Assemble dataset
#' sim_data <- data.frame(x1, x2, x3, x4, group, y, binary_y, y1, y2, y3, count_y, offset_var)
#' 
#' #------------------------------------------
#' # 1. Linear model (lm)
#' #------------------------------------------
#' mod_lm <- lm(y ~ x1 + x4, 
#'              data = sim_data)
#' easyViz(model = mod_lm, data = sim_data, predictor = "x1", 
#'         by = "x4",
#'         pred_range_limit = FALSE,
#'         pred_on_top = TRUE,
#'         bty = "n",
#'         ylim = c(-12,18),
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         point_col = ifelse(sim_data$x4=="a", "red", 
#'                            ifelse(sim_data$x4=="b", "orange", 
#'                                   "yellow")),
#'         point_cex = 0.5,
#'         pred_line_col = c("red", "orange", "yellow"),
#'         pred_line_lty = 1,
#'         ci_polygon_col = c(rgb(1,0,0,0.5), 
#'                            rgb(1,0.5,0,0.5), 
#'                            rgb(1,1,0,0.5)),
#'         add_legend = TRUE, 
#'         legend_position = "top",
#'         legend_title = "Predictor x4",
#'         legend_labels = c("a", "b", "c"),
#'         legend_horiz = TRUE,
#'         legend_args = list(pch = 16))
#' 
#' mod_lm2 <- lm(sqrt(x3) ~ x1 * x4, 
#'               data = sim_data)
#' easyViz(model = mod_lm2, data = sim_data, predictor = "x1", 
#'         by="x4",
#'         backtransform_response = function(x) x^2,
#'         ylim = c(0,8),
#'         show_data_points = FALSE,
#'         add_legend = TRUE)
#' 
#' mod_lm3 <- lm(y ~ poly(x3, 3), 
#'               data = sim_data)
#' easyViz(model = mod_lm3, data = sim_data, predictor = "x3", 
#'         pred_on_top = TRUE,
#'         font_family = "mono",
#'         point_col = rgb(1,0,0,0.3),
#'         point_pch = "+",
#'         ci_type = "lines",
#'         ci_line_lty = 2)
#' 
#' # Extract prediction data
#' pred_df <- easyViz(model = mod_lm, data = sim_data, predictor = "x1", by = "x4")
#' head(pred_df)
#' 
#' #------------------------------------------
#' # 2. Robust linear model (rlm)
#' #------------------------------------------
#' mod_rlm <- rlm(y ~ x1 + x4, 
#'                data = sim_data)
#' easyViz(model = mod_rlm, data = sim_data, predictor = "x1", 
#'         by = "x4",
#'         pred_on_top = TRUE,
#'         bty = "n",
#'         xlim = c(-2.2,3.5), # extend x-axis limits
#'         xlab = "", # temporarily remove x-axis label
#'         ylab = "Response y",
#'         plot_args = list(xaxp=c(-2, 2, 4)), # set tick marks
#'         point_col = ifelse(sim_data$x4=="a", "red", 
#'                            ifelse(sim_data$x4=="b", "orange", 
#'                                   "yellow")),
#'         point_cex = 0.5,
#'         pred_line_col = c("red", "orange", "yellow"),
#'         pred_line_lty = 1,
#'         ci_polygon_col = c(rgb(1,0,0,0.5), 
#'                            rgb(1,0.5,0,0.5), 
#'                            rgb(1,1,0,0.5)),
#'         add_legend = TRUE, 
#'         legend_position = c(2.25,13),
#'         legend_title = "Predictor x4",
#'         legend_title_size = 0.9,
#'         legend_labels = c("a", "b", "c"),
#'         legend_horiz = FALSE,
#'         legend_args = list(pch = 16))
#' # Then manually add centered x-axis label
#' text(x = 0, y = -18.2, labels = "Predictor x1", xpd = NA)
#' 
#' #------------------------------------------
#' # 3. Generalized least squares (gls)
#' #------------------------------------------
#' mod_gls <- gls(y ~ x1 + x2 + x4, 
#'                correlation = corAR1(form = ~1|group), 
#'                data = sim_data)
#' easyViz(model = mod_gls, data = sim_data, predictor = "x4",
#'         jitter_data_points = TRUE,
#'         bty = "n",
#'         xlab = "Predictor x4", 
#'         ylab = "Response y",
#'         point_col = rgb(0,0,1,0.2),
#'         pred_point_col = "blue",
#'         cat_labels = c("group A", "group B", "group C"))
#' 
#' sim_data$x5 <- sample(c(rep("CatA", 50), rep("CatB", 50)))
#' mod_gls2 <- gls(y ~ x1 + x2 + x4 * x5, 
#'                 correlation = corAR1(form = ~1|group), 
#'                 data = sim_data)
#' easyViz(model = mod_gls2, data = sim_data, predictor = "x4",
#'         by = "x5",
#'         jitter_data_points = TRUE,
#'         bty = "n",
#'         ylim = c(-15,15),
#'         xlim=c(0.75,4), # extend x-axis limits
#'         xlab = "", # temporarily remove x-axis label
#'         ylab = "Response y",
#'         cat_labels = c("group A", "group B", "group C"),
#'         point_col = c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)),
#'         pred_point_col = c("blue", "red"),
#'         ci_bar_caps = 0,
#'         add_legend = TRUE,
#'         legend_position = "topright",
#'         legend_args = list(title = "Predictor x5",
#'                            title.cex = 1, 
#'                            legend = c("A", "B"),
#'                            pt.cex = 1.5,
#'                            horiz = TRUE))
#' # Then manually add centered x-axis label
#' text(x = 2, y = -23.2, labels = "Predictor x4", xpd = NA)
#' 
#' #------------------------------------------
#' # 4. Nonlinear least squares (nls)
#' #------------------------------------------
#' mod_nls <- nls(y ~ a * sin(b * x3) + c,
#'                data = sim_data,
#'                start = list(a = 5, b = 1, c = 0))
#' summary(mod_nls)
#' easyViz(model = mod_nls, data = sim_data, predictor = "x3",
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
#' mod_glm <- glm(binary_y ~ x1 + x4 + offset(log(offset_var)), 
#'                family = binomial(link="cloglog"),
#'                data = sim_data)
#' easyViz(model = mod_glm, data = sim_data, predictor = "x1",
#'         fix_values = list(x4="b", offset_var=1),
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         binary_data_type = "binned",
#'         point_col = "black",
#'         ci_polygon_col = "red")
#' 
#' easyViz(model = mod_glm, data = sim_data, predictor = "x4",
#'         bty = "n",
#'         xlab = "Predictor x4", 
#'         ylab = "Response y",
#'         binary_data_type = "plain",
#'         jitter_data_points = TRUE,
#'         point_col = "black",
#'         point_pch = "|",
#'         point_cex = 0.5)
#' 
#' mod_glm2 <- glm(y1/y3 ~ x1 + x4, weights = y3, 
#'                 family = binomial(link="logit"), 
#'                 data = sim_data)
#' easyViz(model = mod_glm2, data = sim_data, predictor = "x1",
#'         pred_on_top = TRUE,
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         point_col = "black",
#'         ci_polygon_col = "red")
#' 
#' #------------------------------------------
#' # 6. Negative binomial GLM (glm.nb)
#' #------------------------------------------
#' mod_glm_nb <- glm.nb(count_y ~ x2, 
#'                      data = sim_data)
#' easyViz(model = mod_glm_nb, data = sim_data, predictor = "x2",
#'         font_family = "mono",
#'         bty = "L",
#'         plot_args = list(main = "NB model"),
#'         xlab = "Predictor x2", 
#'         ylab = "Response y",
#'         ci_polygon_col = "blue")
#' 
#' #------------------------------------------
#' # 7. Linear mixed-effects model (lmer)
#' #------------------------------------------
#' mod_lmer <- lmer(y ~ x1 + x4 + (1 | group), 
#'                  data = sim_data)
#' easyViz(model = mod_lmer, data = sim_data, predictor = "x1", 
#'         by="group",
#'         re.form = NULL,
#'         bty = "n",
#'         plot_args = list(xaxp = c(round(min(sim_data$x1),1),
#'                                   round(max(sim_data$x1),1), 5)),
#'         ylim = c(-15, 15),
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         pred_line_col = "green",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 1)
#' oldpar <- par(new = TRUE)
#' easyViz(model = mod_lmer, data = sim_data, predictor = "x1",
#'         re.form = NA,
#'         bty = "n",
#'         plot_args = list(xaxp = c(round(min(sim_data$x1),1),
#'                                   round(max(sim_data$x1),1), 5)),
#'         show_data_points = FALSE,
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         ylim = c(-15, 15),
#'         pred_line_col = "red",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 2,
#'         ci_type = NULL)
#' par(oldpar)
#' 
#' #------------------------------------------
#' # 8. Generalized linear mixed model (glmer)
#' #------------------------------------------
#' mod_glmer <- glmer(binary_y ~ x1 + x4 + (1 | group), 
#'                    family = binomial,
#'                    data = sim_data)
#' easyViz(model = mod_glmer, data = sim_data, predictor = "x1", 
#'         by = "group",
#'         re.form = NULL,
#'         cat_conditioning = "reference",
#'         font_family = "serif",
#'         xlab = "Predictor x1", 
#'         ylab = "Response y",
#'         binary_data_type = "binned",
#'         pred_range_limit = FALSE,
#'         pred_line_col = "blue",
#'         pred_line_lty = 1,
#'         pred_line_lwd = 1)
#' 
#' #------------------------------------------
#' # 9. GLMM with negative binomial (glmer.nb)
#' #------------------------------------------
#' mod_glmer_nb <- glmer.nb(count_y ~ x2 + x4 + (1 | group), 
#'                          data = sim_data)
#' easyViz(model = mod_glmer_nb, data = sim_data, predictor = "x2",
#'         re.form = NA,
#'         bty = "n",
#'         xlab = "Predictor x2", 
#'         ylab = "Response y",
#'         ylim = c(0, 120),
#'         point_pch = 1)
#' 
#' #------------------------------------------
#' # 10. GLMM using glmmTMB
#' #------------------------------------------
#' mod_glmmTMB <- glmmTMB(count_y ~ x2 + x4 + (1 | group),
#'                        ziformula = ~ x2, 
#'                        family = nbinom2,
#'                        data = sim_data)
#' easyViz(model = mod_glmmTMB, data = sim_data, predictor = "x2",
#'         re.form = NA,
#'         bty = "n",
#'         xlab = "Predictor x2", 
#'         ylab = "Response y",
#'         ylim = c(0, 120),
#'         point_pch = 1,
#'         ci_type = NULL)
#' 
#' #------------------------------------------
#' # 11. GAM with random smooth for group
#' #------------------------------------------
#' mod_gam <- gam(y ~ s(x3) + s(group, bs = "re"),
#'                data = sim_data)
#' easyViz(model = mod_gam, data = sim_data, predictor = "x3",
#'         re.form = NA,
#'         las = 0,
#'         bty = "n",
#'         xlab = "Predictor x3", 
#'         ylab = "Response y",
#'         point_col = "black",
#'         point_pch = 1,
#'         ci_polygon_col = rgb(1,0,0,0.5))
#'         
#' #------------------------------------------
#' # 12. Plotting 3-way interaction
#' #------------------------------------------
#' mod_lm_int <- lm(y ~ x1*x2*x3, 
#'                  data = sim_data)
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
#' easyViz(model = mod_lm_int, data = sim_data, predictor = "x1", 
#'         by = "x2",
#'         fix_values = c(x3 = 0.5750978),
#'         plot_args = list(xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4),
#'         add_legend = TRUE,
#'         legend_position = "topleft",
#'         legend_labels = c("x2 = -1.3", "x2 = -0.2", "x2 = 1.5"))
#' add_strip_label("x3 = 0.6")
#' mtext("Response y", side = 2, outer = TRUE, line = 2.5)
#' 
#' # Panel 2
#' easyViz(model = mod_lm_int, data = sim_data, predictor = "x1", 
#'         by = "x2",
#'         fix_values = c(x3 = 2.3095046),
#'         plot_args = list(yaxt = "n", xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4))
#' add_strip_label("x3 = 2.3")
#' 
#' # Panel 3
#' easyViz(model = mod_lm_int, data = sim_data, predictor = "x1", 
#'         by = "x2",
#'         fix_values = c(x3 = 4.4509078),
#'         plot_args = list(yaxt = "n", xlab = "", ylab = ""),
#'         show_data_points = FALSE,
#'         pred_line_col = c(2, 3, 4),
#'         ci_polygon_col = c(2, 3, 4))
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
#' @importFrom stats formula family glm gaussian plogis pnorm pcauchy predict quantile model.frame median complete.cases coef model.matrix vcov getCall terms df.residual nobs qt qnorm
#' @importFrom utils modifyList head tail
#' @importFrom graphics par plot polygon lines points axis legend arrows
#' @importFrom grDevices col2rgb rgb adjustcolor
#' 
#' @export
easyViz <- function(model,
                    data,
                    predictor,
                    by = NULL,
                    pred_type = "response",
                    pred_range_limit = TRUE,
                    pred_on_top = FALSE,
                    pred_resolution = 101,
                    num_conditioning = "median",
                    cat_conditioning = "mode",
                    fix_values = NULL,
                    re.form = NULL,
                    backtransform_response = NULL,
                    xlim = NULL, 
                    ylim = NULL,
                    xlab = NULL, 
                    ylab = NULL,
                    cat_labels = NULL,
                    font_family = "",
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
                    pred_line_lty = c(1,2,3,4), 
                    pred_line_lwd = 2,
                    ci_type = "polygon",
                    ci_polygon_col = c("gray", "black", "lightgray", "darkgray"),
                    ci_line_col = "black", 
                    ci_line_lty = c(1,2,3,4), 
                    ci_line_lwd = 1,
                    pred_point_col = c("black", "gray", "darkgray", "lightgray"), 
                    pred_point_pch = 16, 
                    pred_point_cex = 1,
                    ci_bar_col = "black", 
                    ci_bar_lty = 1, 
                    ci_bar_lwd = 1,
                    ci_bar_caps = 0.1,
                    add_legend = FALSE,
                    legend_position = "top",
                    legend_title = NULL,
                    legend_labels = NULL,
                    legend_title_size = 1,
                    legend_label_size = 0.9,
                    legend_horiz = FALSE,
                    legend_args = list())

{
  #-1---INPUT VALIDATION------------------------------------------
  
  # Check required arguments
  if (missing(model)) stop("Argument 'model' is required.")
  if (missing(data)) stop("Argument 'data' is required.")
  if (missing(predictor)) stop("Argument 'predictor' is required.")
  
  #-2---RESPONSE EXTRACTION---------------------------------------
  
  # First define helper to unwrap a cleaned-up expression object 
  # (e.g., y, cbind(a, b), successes / trials) so we can analyze its structure...
  unwrap_to_meaningful_expr <- function(expr) {
    while (is.call(expr)) {
      head <- as.character(expr[[1]])
      if (head %in% c("(", "c")) {
        expr <- expr[[2]]
      } else {
        break
      }
    }
    return(expr)
  }
  
  # ...then define helper to extract a usable variable name from the object,
  # if it’s not one of the special forms (binomial proportion data)
  extract_response_name <- function(expr) {
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
  
  lhs_expr <- formula(model)[[2]] # Get the left-hand-side of the model formula
  unwrapped <- unwrap_to_meaningful_expr(lhs_expr) # Remove outermost wrappers to simplify further analysis 
                                                   # (e.g., obtain y1/y2 from c(y1/y2) or (y1/y2))
  # Check for use of $ in the response variable
  warn_on_env_prefix <- function(expr) {
    if (!is.call(expr)) return(FALSE)
    head <- as.character(expr[[1]])
    if (head %in% c("$")) return(TRUE)
    any(vapply(as.list(expr), warn_on_env_prefix, logical(1)))
  }
  if (warn_on_env_prefix(lhs_expr)) {
    message("Heads up! Using `$` in model formulas can produce 
unexpected results. It's safer to specify the model 
using the `data` argument instead.")
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
      message("Detected cbind(successes, failures) response. 
Plotting success proportions.")
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
        message("Detected successes / trials response. 
Plotting success proportions.") 
      }
      
    } else { # fallback for any other call, e.g., log(y), sqrt(y), y^2, df$y, etc.
      response <- extract_response_name(unwrapped)
    }
    
  } else { # fallback for non-calls, e.g., y
    response <- extract_response_name(unwrapped)
  }
  
  # Convert binary response to numeric (0/1) if it's a factor
  if (is.factor(data[[response]]) && length(levels(data[[response]])) == 2) {
    data[[response]] <- as.numeric(data[[response]]) - 1
  }
  
  #-3---EXPLANATORY VARIABLE EXTRACTION AND DATA CLEANING---------
  
  # Identify variables that should be complete (i.e., no NA)
  extract_clean_formula_vars <- function(expr) {
    raw_vars <- all.vars(expr)
    
    # Remove environment references like df in df$y
    remove_env_refs <- function(e) {
      if (!is.call(e)) return(character(0))
      head <- as.character(e[[1]])
      if (head %in% c("$")) {
        return(as.character(e[[2]]))  # "df" in df$y
      }
      unlist(lapply(as.list(e)[-1], remove_env_refs))
    }
    
    env_refs <- remove_env_refs(expr)
    setdiff(raw_vars, env_refs)
  }
  
  formula_vars <- extract_clean_formula_vars(formula(model))
  
  # Capture offset variables passed via offset= argument (outside formula)
  offset_vars <- if ("offset" %in% names(getCall(model))) all.vars(getCall(model)$offset) else NULL
  
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
  
  # Subset data to rows with no NA in those variables
  original_data <- data # to be used later when validating user-supplied aesthetic for data points
  data <- data[complete.cases(data[, model_vars, drop = FALSE]), ]
  # Save number of rows in cleaned data for aesthetic checks
  n_data_rows <- nrow(data)
  
  # Validate user-supplied aesthetic lengths (if vector input)
  # Only meaningful for point aesthetics.
  # The warning is triggered when the user supplies a vector that matches the number of rows in the original (unfiltered) data
  # but not in the cleaned data used for plotting — a sign that the aesthetic was constructed before NA filtering
  # and may now be misaligned with the actual data being plotted.
  # Typical example is the use of 'ifelse(your.data$x=="level1"...)' to change point aesthetics.
  check_length <- function(arg, name) {
    if (!is.null(arg) && length(arg) > 1 &&
        length(arg) != n_data_rows && # check if aesthetic lengths = rows in the cleaned data frame 
        length(arg) == nrow(original_data)) { # check if aesthetic lengths = rows in the original data frame 
      warning(sprintf(
        "Length mismatch: '%s' has length %d, but the cleaned 
data frame used for predictions has %d rows. This may
cause incorrect aesthetic mapping in the plot. 
This usually happens when '%s' is constructed from the 
original data frame, which still includes rows with 
missing values. To fix this, remove rows with NA only 
in the variables used in the model, like this:

clean.data <- your.data[complete.cases(your.data[, c(\"%s\")]), ]

Then construct '%s' using clean.data instead of the 
original data.",
        name, length(arg), n_data_rows, name,
        paste(model_vars, collapse = "\", \""),
        name
      ), call. = FALSE)
    }
  }
  # Check point aesthetics
  check_length(point_col, "point_col")
  check_length(point_cex, "point_cex")
  check_length(point_pch, "point_pch")
  
  # Fix_values: warn if variable is not in formula or offset
  if (!is.null(fix_values)) {
    for (var_name in names(fix_values)) {
      if (!(var_name %in% c(formula_vars, offset_vars))) {
        message(sprintf(
          "Heads up! Variable '%s' in fix_values 
is not used in the model formula or offset. 
Fixing it will have no effect on predictions.
Consider removing it from your easyViz() call.",
          var_name
        ))
      }
    }
  }
  
  # Check for contradiction: predictor also listed in fix_values
  if (!is.null(fix_values) && predictor %in% names(fix_values)) {
    stop("The variable specified in 'predictor' is also included 
in 'fix_values'. This is contradictory. 
Remove it from 'fix_values' to allow it to vary.")
  }
  
  # by: warn if not used in the model
  if (!is.null(by) && !(by %in% c(formula_vars, offset_vars))) {
    message(sprintf(
      "Heads up! The 'by' variable '%s' 
is not used in the model formula or offset. 
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

  #-4---CONDITIONING VALUES FOR NON-TARGET VARIABLES--------------
  
  # Identify numeric and categorical predictors
  is_numeric <- is.numeric(data[[predictor]])
  
  # Conditioning numeric and categorical variables
  numeric_vars <- sapply(data, is.numeric)
  numeric_vars[predictor] <- FALSE # Exclude predictor
  
  if (num_conditioning == "mean") {
    num_values <- sapply(data[, numeric_vars], mean, na.rm = TRUE)
  } else if (num_conditioning == "median") {
    num_values <- sapply(data[, numeric_vars], median, na.rm = TRUE)
  } else {
    stop("Invalid value for num_conditioning. 
Choose 'mean' or 'median'.")
  }
  
  categorical_vars <- sapply(data, is.factor)
  categorical_vars[predictor] <- FALSE  # Exclude predictor
  
  if (cat_conditioning == "mode") {
    get_mode <- function(x) {
      x_no_na <- x[!is.na(x)]
      if (length(x_no_na) == 0) return(NA_character_)
      names(sort(table(x_no_na), decreasing = TRUE))[1]
    }
    
    cat_values <- sapply(data[, categorical_vars, drop = FALSE], get_mode)
    
    # Check for NAs and stop early with a helpful message
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
    
    if (anyNA(cat_values)) {
      bad_vars <- names(cat_values)[is.na(cat_values)]
      stop("Could not determine reference level for the following factor(s): ",
           paste(bad_vars, collapse = ", "))
    }
  } else {
    stop("Invalid value for cat_conditioning. 
Choose 'mode' or 'reference'.")
  }
  
  
  #-5---NEW DATA GENERATION FOR PREDICTION------------------------
  
  # Define new_data for prediction
  new_data <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(new_data) <- colnames(data)
  
  # Handle case where `by` is not NULL (e.g., interactions)
  if (!is.null(by)) {
    by_levels <- levels(data[[by]])
    n_levels <- length(by_levels)
    
    # If both predictor and `by` are numeric, ensure numeric treatment throughout
    if (is.numeric(data[[predictor]]) && is.numeric(data[[by]])) {
      by_levels <- quantile(data[[by]], probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
      by_levels <- as.numeric(by_levels)
    }
    n_levels <- length(by_levels)
    
    # Ensure dynamic properties can accommodate multiple levels
    # Dynamically adjust line and polygon features based on the number of levels in `by`
    n_levels <- length(by_levels)
    
    # Repeat user-provided aesthetics or provide defaults dynamically
    pred_line_lty <- if (length(pred_line_lty) < n_levels) rep(pred_line_lty, length.out = n_levels) else pred_line_lty
    pred_line_col <- if (length(pred_line_col) < n_levels) rep(pred_line_col, length.out = n_levels) else pred_line_col
    pred_line_lwd <- if (length(pred_line_lwd) < n_levels) rep(pred_line_lwd, length.out = n_levels) else pred_line_lwd
    
    ci_polygon_col <- if (length(ci_polygon_col) < n_levels) rep(ci_polygon_col, length.out = n_levels) else ci_polygon_col
    ci_line_col <- if (length(ci_line_col) < n_levels) rep(ci_line_col, length.out = n_levels) else ci_line_col
    ci_line_lty <- if (length(ci_line_lty) < n_levels) rep(ci_line_lty, length.out = n_levels) else ci_line_lty
    ci_line_lwd <- if (length(ci_line_lwd) < n_levels) rep(ci_line_lwd, length.out = n_levels) else ci_line_lwd
    
    # override pred_range_limit if `by` is numeric
    explicit_user_pred_range_limit <- !missing(pred_range_limit)
    
    if (!is.null(by) && is.numeric(data[[by]])) {
      if (explicit_user_pred_range_limit && pred_range_limit) {
        message("Note: `pred_range_limit` ignored with numeric 'by'; 
using full predictor range.")
      }
      pred_range_limit <- FALSE
    }
    # If `by` is not NULL, vary the predictor within each level of `by`...
    # First build a data frame (`new_data`) that contains all combinations of the predictor and `by` levels
    if (is_numeric) { # numeric predictor
      # Restrict prediction range to observed range within each `by` level (if pred_range_limit = TRUE)
      if (pred_range_limit) {
        new_data_list <- lapply(by_levels, function(by_level) {
          subset_data <- data[data[[by]] == by_level, ]
          range_x <- range(subset_data[[predictor]], na.rm = TRUE)
          seq_values <- seq(from = range_x[1], to = range_x[2], length.out = pred_resolution)
          df <- data.frame(
            predictor = seq_values,
            by = rep(by_level, length(seq_values))
          )
          names(df) <- c(predictor, by)
          return(df)
        })
        new_data <- do.call(rbind, new_data_list)
      } else {
        # Use full prediction range (across all data)
        seq_values <- seq(min(data[[predictor]], na.rm = TRUE), 
                          max(data[[predictor]], na.rm = TRUE), 
                          length.out = pred_resolution)
        new_data <- expand.grid(seq_values, by_levels)
        colnames(new_data) <- c(predictor, by)
      }
    } else { # categorical predictor
      levels_values <- levels(data[[predictor]])
      new_data <- expand.grid(levels_values, by_levels)
      colnames(new_data) <- c(predictor, by)
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
    if (is_numeric) { # numeric predictor
      seq_values <- seq(min(data[[predictor]], na.rm = TRUE), max(data[[predictor]], na.rm = TRUE), length.out = pred_resolution)
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(seq_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- seq_values
    } else { # categorical predictor
      levels_values <- levels(data[[predictor]])
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(levels_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- levels_values
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
    fix_values <- as.list(fix_values)  # Convert to list to handle both named vector and named list inputs
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
        stop(paste("Variable", var_name, "not found in the data."))
      }
    }
  }
  
  
  #-6---MODEL TYPE DETECTION AND OFFSET WARNINGS------------------
  
  # Identify model classes
  is_lm_model <- inherits(model, "lm") &&
    !inherits(model, c("rlm", "glm", "gam"))
  is_rlm_model <- inherits(model, "rlm") 
  is_nls_model <- inherits(model, "nls")
  is_gls_model <- inherits(model, "gls")
  is_lme4_model <- inherits(model, "glmerMod") || inherits(model, "lmerMod")
  is_glmmTMB_model <- inherits(model, "glmmTMB")
  is_gam_model <- inherits(model, "gam")
  
  # Warn if offset was passed outside of the formula (and we're using gam/lmer/glmer)
  model_call <- try(getCall(model), silent = TRUE)
  if (!inherits(model_call, "try-error") && "offset" %in% names(model_call)) {
    offset_expr <- model_call$offset
    offset_vars <- all.vars(offset_expr)
    
    # Check if model type is one that ignores offsets outside the formula
    if (is_lme4_model || is_gam_model) {
      message(sprintf("Heads up! Model includes offset variable(s) 
specified outside of the formula: %s. 
These may be ignored during prediction for 
models like lmer()/glmer() or gam().
Consider including the offset inside the formula 
as offset(%s).", paste(offset_vars, collapse = ", "), deparse(offset_expr)
      ))
    }
  }
  
  
  #-7---PREDICTIONS-----------------------------------------------
  
  # Default: no term excluded
  exclude_terms <- NULL
  # Determine if re.form indicates exclusion of random-effect smooths in mgcv::gam
  exclude_re_smooths <- (
    isTRUE(all.equal(re.form, NA)) ||
      (inherits(re.form, "formula") && deparse(re.form) == "~0")
  )
  # For mgcv::gam models, emulate re.form = NA by excluding re smooths
  if (is_gam_model && exclude_re_smooths) {
    re_smooths <- vapply(model$smooth, function(sm) {
      inherits(sm, "random.effect") || (inherits(sm, "re.smooth") || (!is.null(sm$xt) && sm$xt$bs == "re"))
    }, logical(1))
    exclude_terms <- vapply(model$smooth[re_smooths], function(sm) sm$label, character(1))
    if (length(exclude_terms) > 0) {
      message("Excluding random-effect smooth terms from prediction 
(analogous to re.form = NA or ~0): ",
              paste(exclude_terms, collapse = ", "))
    }
  }
  
  # Predict with or without re.form depending on model class
  if (is_lme4_model && is.null(re.form)) {
    # lme4 + subject-specific predictions: no SEs
    pred_fit <- predict(model, newdata = new_data, type = "link", re.form = re.form)
    preds_link <- list(fit = pred_fit, se.fit = rep(NA, length(pred_fit)))
    message("Note: Standard errors are not available for 
lme4 models when re.form = NULL.")
    
  } else if (is_lme4_model || is_glmmTMB_model) {
    # Models that support re.form and se.fit
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link", re.form = re.form)
    
  } else if (is_gam_model) {
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link",
                          exclude = if (exclude_re_smooths) exclude_terms else NULL)
    
  } else if (is_rlm_model) { # compute SEs using sandwich estimator
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required to compute 
robust SEs for rlm models. Please install it.")
    }
    
    # Align factor levels in new_data to match the model's training data
    model_frame <- model.frame(model)
    for (var in names(new_data)) {
      if (var %in% names(model_frame)) {
        if (is.factor(model_frame[[var]])) {
          new_data[[var]] <- factor(new_data[[var]], levels = levels(model_frame[[var]]))
        }
      }
    }
    
    # Create the design matrix using the same terms used in the model
    X <- model.matrix(terms(model), new_data)
    betas <- coef(model)
    
    # Compute predicted values
    pred_fit <- as.vector(X %*% betas)
    
    # Robust variance-covariance matrix
    vcov_robust <- sandwich::vcovHC(model, type = "HC0")
    
    # Option A: full matrix (default)
    se_fit <- tryCatch({
      sqrt(rowSums((X %*% vcov_robust) * X))
    }, error = function(e) {
      warning("Failed to compute SEs with full vcovHC matrix; 
falling back to diagonal approximation.")
      sqrt(rowSums((X %*% diag(diag(vcov_robust))) * X))
    })
    
    # Diagnostic message
    message("Robust standard errors for rlm model 
computed using sandwich estimator (HC0).")
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
  } else if (is_gls_model) {
    X_new <- model.matrix(terms(model), new_data)
    betas <- coef(model)
    pred_fit <- as.vector(X_new %*% betas)
    
    vcov_mat <- vcov(model)
    se_fit <- sqrt(rowSums((X_new %*% vcov_mat) * X_new))
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    message("Note: CIs reflect fixed-effect uncertainty only; 
residual correlation structure is ignored, 
and intervals may be underestimated.")
    
  } else if (is_nls_model) { # compute SEs using the delta method 
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
    
    # Fallback if vcov fails
    if (inherits(vcov_mat, "try-error")) {
      warning("Could not extract variance-covariance matrix from nls model.")
      se_fit <- rep(NA, nrow(new_data))
    } else {
      # Compute predicted values and delta-method SEs
      pred_fit <- numeric(nrow(new_data))
      se_fit <- numeric(nrow(new_data))
      
      for (i in seq_len(nrow(new_data))) {
        grad_i <- numDeriv::grad(func = pred_fun, x = params, data_row = new_data[i, ])
        pred_fit[i] <- pred_fun(params, new_data[i, ])
        se_fit[i] <- sqrt(t(grad_i) %*% vcov_mat %*% grad_i)
      }
    }
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    message("Note: CIs for nls models are approximate 
and computed via the delta method.")
    
  } else if (is_lm_model) {
    # lm does not support type = "link" — use response
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "response")
    
  } else if (inherits(model, "glm")) {
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link")
    
  } else {
    stop("easyViz does not support this type of model. 
Supported types include: lm, glm, glm.nb, nls, 
gls, lmer/glmer, glmmTMB and gam (mgcv).")
  }
  
  # Detect correlated residual structure in glmmTMB
  if (is_glmmTMB_model && !is.null(model$call)) {
    call_entries <- model$call[c("formula", "ziformula", "dispformula")]
    call_entries <- call_entries[!vapply(call_entries, is.null, logical(1))]  # Remove NULLs
    if (length(call_entries) > 0) {
      call_parts <- unlist(lapply(call_entries, deparse), use.names = FALSE)
      
      # List of correlation structure keywords supported by glmmTMB
      correlation_keywords <- c("exp", "mat", "gau", "gen", "ar1", "cs", "un", "toep", "usr")
      
      has_correlation <- any(sapply(correlation_keywords, function(k) {
        any(grepl(paste0("\\b", k, "\\s*\\("), call_parts))
      }))
      
      if (has_correlation) {
        message("Note: CIs reflect fixed-effect uncertainty only; 
residual correlation structure is ignored, 
and intervals may be underestimated.")
      }
    }
  }
  
  
  #-8---BACKTRANSFORMING TO RESPONSE SCALE------------------------
  
  # Calculate link-scale confidence intervals
  link_fit <- preds_link$fit
  link_se <- preds_link$se.fit
  # Calculate t-based CIs only for models where inference is not asymptotic
  # and finite-sample correction is appropriate
  #	Uses fallback logic (nobs - k) only for those models — ensuring robustness
  # Defaults to 1.96 for everything else — including GLMs, GAMs, and mixed models
  crit <- tryCatch({
    if (inherits(model, c("lm", "rlm", "gls", "nls")) &&
        !inherits(model, c("glm", "gam", "glmerMod", "lmerMod", "glmmTMB"))) {
      
      df <- df.residual(model)
      
      # Fallback: use nobs - k
      if (is.null(df) || is.na(df) || !is.finite(df) || df <= 0) {
        n <- tryCatch(nobs(model), error = function(e) NA)
        k <- tryCatch(length(coef(model)), error = function(e) NA)
        df <- if (!is.na(n) && !is.na(k) && is.finite(n - k) && (n - k) > 0) {
          n - k
        } else {
          NA
        }
      }
      
      if (!is.na(df) && is.finite(df) && df > 0) {
        qt(0.975, df)
      } else {
        qnorm(0.975)
      }
      
    } else {
      qnorm(0.975)
    }
  }, error = function(e) qnorm(0.975))
  link_lower <- link_fit - crit * link_se
  link_upper <- link_fit + crit * link_se
  
  # Transform predictions back to the response scale if needed
  # Define a mapping of common link functions to their inverses
  get_linkinv <- function(link) {
    link_inverses <- list(
      logit    = plogis,                                      # Logit
      probit   = pnorm,                                       # Probit
      cauchit  = pcauchy,                                     # Cauchit
      cloglog  = function(eta) -expm1(-exp(pmin(eta, 700))),  # Complementary log-log
      log      = exp,                                         # Log
      sqrt     = function(eta) eta^2,                         # Square root
      inverse  = function(eta) 1/eta,                         # Inverse
      identity = identity,                                    # Identity
      `1/mu^2` = function(eta) 1 / sqrt(eta)                  # 1/mu^2
    )
    if (!link %in% names(link_inverses))
      stop("Unsupported link function: ", link)
    link_inverses[[link]]
  }
  
  # Transform predictions back to the response scale if needed
  if (is_gls_model || is_nls_model) {
    preds <- list(
      fit = link_fit,
      lower = link_lower,
      upper = link_upper
    )
  } else if (pred_type == "response") {
    # Attempt to determine the link function
    link_function <- tryCatch({
      if (inherits(model, "lme")) {
        "identity"
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
    
    # Get the inverse of the link function
    linkinv <- get_linkinv(link_function)
    
    # Transform predictions
    preds <- list(
      fit = linkinv(link_fit),
      lower = linkinv(link_lower),
      upper = linkinv(link_upper)
    )
  } else {
    preds <- list(
      fit = link_fit,
      lower = link_lower,
      upper = link_upper
    )
  }
  # Apply backtransform_response irrespective of pred_type
  if (!is.null(backtransform_response) && is.function(backtransform_response)) {
    preds <- list(
      fit = backtransform_response(preds$fit),
      lower = backtransform_response(preds$lower),
      upper = backtransform_response(preds$upper)
    )
  }
  
  
  #-9---PLOT SETUP------------------------------------------------
  
  # Validate color arguments
  validate_color <- function(color) {
    tryCatch({
      grDevices::col2rgb(color)  # works for numeric, hex, and names
      TRUE
    }, error = function(e) FALSE)
  }
  colors <- c(pred_line_col, point_col, ci_polygon_col, ci_line_col, pred_point_col, ci_bar_col)
  sapply(colors, function(col) if (!validate_color(col)) stop(paste("Invalid color:", col)))
  
  # For categorical predictors, convert them to numeric for plotting
  if (!is_numeric) {
    # Ensure the levels of the predictor are explicitly set
    if (!is.null(levels(data[[predictor]]))) {
      new_data[[predictor]] <- factor(new_data[[predictor]], levels = levels(data[[predictor]]))
    } else {
      # If levels are not explicitly set, default to the order in the data
      new_data[[predictor]] <- factor(new_data[[predictor]], levels = unique(data[[predictor]]))
    }
    new_data[[predictor]] <- as.numeric(new_data[[predictor]])
  }
  
  # Define plot limits if not provided
  if (is.null(xlim)) {
    if (!is_numeric) {
      num_levels <- length(levels(factor(data[[predictor]])))
      xlim <- c(0.75, num_levels + 0.25)
    } else {
      xlim <- range(new_data[[predictor]], na.rm = TRUE)
    }
  }
  if (is.null(ylim)) {
    ylim <- range(c(data[[response]], preds$lower, preds$upper), na.rm = TRUE)
  }
  
  ylab <- ifelse(is.null(ylab), response, ylab)
  xlab <- ifelse(is.null(xlab), predictor, xlab)
  
  # Set up the plot
  old <- par(family = font_family)
  on.exit(par(old))
  plot_defaults <- list(
    x = 1,
    type = "n",
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    las = las,
    bty = bty,
    xaxt = if (!is_numeric) "n" else "s")
  
  # Warn if plot_args includes global par() settings (which won't work here)
  forbidden_plot_args <- c("mar", "oma", "mfrow", "mfcol", "mgp", "xpd", "plt", "mai", "omi")
  if (any(names(plot_args) %in% forbidden_plot_args)) {
    warning("Some entries in 'plot_args' affect global settings 
and will be ignored: ",
            paste(intersect(names(plot_args), forbidden_plot_args), collapse = ", "), call. = FALSE)
  }
  
  do.call(plot, modifyList(plot_defaults, plot_args))
  
  #-10---PLOT SETUP: NUMERIC PREDICTOR---------------------------
  
  # Plot for numeric predictors (with or without `by`)
  if (is_numeric) {
    draw_prediction_layers <- function() {
      if (!is.null(by)) { # with `by`
        auto_legend_labels <- vector("character", length(by_levels))
        for (i in seq_along(by_levels)) {
          subset_data <- new_data[new_data[[by]] == by_levels[i], ]
          subset_preds <- preds$fit[new_data[[by]] == by_levels[i]]
          subset_lower <- preds$lower[new_data[[by]] == by_levels[i]]
          subset_upper <- preds$upper[new_data[[by]] == by_levels[i]]
          
          if (!is.null(ci_type)) {
            if (ci_type == "polygon") {
            polygon_x <- c(subset_data[[predictor]], rev(subset_data[[predictor]]))
            polygon_y <- c(subset_upper, rev(subset_lower))
            polygon(polygon_x, polygon_y,
                    col = adjustcolor(ci_polygon_col[i], alpha.f = 0.5),
                    border = NA)
            } else if (ci_type == "lines") {
            lines(subset_data[[predictor]], subset_upper,
                  col = ci_line_col[i], lty = ci_line_lty[i], lwd = ci_line_lwd[i])
            lines(subset_data[[predictor]], subset_lower,
                  col = ci_line_col[i], lty = ci_line_lty[i], lwd = ci_line_lwd[i])
            } else {
              stop("Invalid ci_type. Choose 'polygon', 'lines', or NULL.")
            }
          }
        }
        for (i in seq_along(by_levels)) {
          subset_data <- new_data[new_data[[by]] == by_levels[i], ]
          subset_preds <- preds$fit[new_data[[by]] == by_levels[i]]
          
          lines(subset_data[[predictor]], subset_preds,
                col = pred_line_col[i],
                lwd = pred_line_lwd[i],
                lty = pred_line_lty[i])
          auto_legend_labels[i] <- paste(by, "=", by_levels[i])
        }
        if (add_legend) {
          # Determine legend labels
          if (!is.null(legend_labels)) {
            final_legend_labels <- legend_labels
          } else if (!is.null(legend_title)) {
            final_legend_labels <- by_levels
          } else {
            final_legend_labels <- auto_legend_labels
          }
          
          # Assemble legend arguments
          legend_args_final <- list(
            legend = final_legend_labels,
            col = pred_line_col,
            lwd = pred_line_lwd,
            lty = pred_line_lty,
            bty = "n",
            cex = legend_label_size,
            horiz = legend_horiz
          )
          
          # Add title if specified
          if (!is.null(legend_title)) {
            legend_args_final$title <- legend_title
            legend_args_final$title.cex <- legend_title_size
          }
          
          # Position: named or numeric
          if (is.character(legend_position)) {
            legend_args_final$x <- legend_position
            do.call(legend, modifyList(legend_args_final, legend_args))
          } else if (is.numeric(legend_position) && length(legend_position) == 2) {
            legend_args_final$x <- legend_position[1]
            legend_args_final$y <- legend_position[2]
            do.call(legend, modifyList(legend_args_final, legend_args))
          } else {
            warning("Invalid legend_position. Must be a string or numeric vector of length 2.")
          }
        }
      } else { # without `by`
        if (!is.null(ci_type)) {
          if (ci_type == "polygon") {
          polygon_x <- c(new_data[[predictor]], rev(new_data[[predictor]]))
          polygon_y <- c(preds$upper, rev(preds$lower))
          polygon(polygon_x, polygon_y,
                  col = adjustcolor(ci_polygon_col[1], alpha.f = 0.5),
                  border = NA)
          } else if (ci_type == "lines") {
            lines(new_data[[predictor]], preds$upper,
                  col = ci_line_col[1], lty = ci_line_lty[1], lwd = ci_line_lwd[1])
            lines(new_data[[predictor]], preds$lower,
                  col = ci_line_col[1], lty = ci_line_lty[1], lwd = ci_line_lwd[1])
          } else {
            stop("Invalid ci_type. Choose 'polygon', 'lines', or NULL.")
          }
        }
        
        # Prediction line last
        lines(new_data[[predictor]], preds$fit,
              col = pred_line_col[1],
              lwd = pred_line_lwd[1],
              lty = pred_line_lty[1])
      }
    }
    
    # If predictions should go under raw data, first plot predictions
    if (!pred_on_top) draw_prediction_layers()
    
    # Then plot raw data
    if (show_data_points) {
      if (length(unique(data[[response]])) == 2) { # Binary response
        if (binary_data_type == "binned") {
          breaks <- seq(min(data[[predictor]], na.rm = TRUE), max(data[[predictor]], na.rm = TRUE), length.out = bins + 1)
          data$bin <- cut(data[[predictor]], breaks = breaks, include.lowest = TRUE)
          bin_edges <- breaks
          bin_centers <- (head(bin_edges, -1) + tail(bin_edges, -1)) / 2
          bin_levels <- levels(data$bin)
          
          for (y_val in 0:1) {
            for (i in seq_along(bin_levels)) {
              bin_label <- bin_levels[i]
              bin_center <- bin_centers[i]
              subset_bin <- data[data$bin == bin_label & data[[response]] == y_val, ]
              if (nrow(subset_bin) > 0) {
                cex_val <- sqrt(nrow(subset_bin)) / max(sqrt(table(data$bin)), na.rm = TRUE) * point_cex * 2
                points(bin_center, y_val, pch = point_pch, col = point_col, cex = cex_val)
              }
            }
          }
        } else {
          points(data[[predictor]], data[[response]], pch = point_pch, col = point_col, cex = point_cex)
        }
      } else { # Continuous response
        x_vals <- if (jitter_data_points) jitter(data[[predictor]]) else data[[predictor]]
        points(x_vals, data[[response]], pch = point_pch, col = point_col, cex = point_cex)
      }
    }
    
    # Otherwise, if predictions should go on top of raw data, plot predictions after data
    if (pred_on_top) draw_prediction_layers()
    
  } 
  
  #-11---PLOT SETUP: CATEGORICAL PREDICTOR-----------------------
  
  else {
    # Plot for categorical predictors
    if (!is_numeric) {
      # Apply custom labels to categorical predictor
      if (!is.null(cat_labels)) {
        if (length(cat_labels) != length(levels(data[[predictor]]))) {
          stop("Length of cat_labels must match the number of levels 
in the categorical predictor.")
        }
        levels(data[[predictor]]) <- cat_labels
        levels(new_data[[predictor]]) <- cat_labels
      }
      
      # Determine x-axis positions
      x_positions <- as.numeric(new_data[[predictor]])
      
      if (!is.null(by)) {
        # Case: with `by`
        unique_by_levels <- levels(factor(data[[by]])) # Ensure all levels of `by` are included
        offset_values <- seq(-0.1, 0.1, length.out = length(unique_by_levels)) # Adjust spacing as needed
        
        # Dynamically set aesthetics for interaction levels
        pred_point_col <- if (length(pred_point_col) < length(unique_by_levels))
          rep(pred_point_col, length.out = length(unique_by_levels)) else pred_point_col
        pred_point_pch <- if (length(pred_point_pch) < length(unique_by_levels))
          rep(pred_point_pch, length.out = length(unique_by_levels)) else pred_point_pch
        pred_point_cex <- if (length(pred_point_cex) < length(unique_by_levels))
          rep(pred_point_cex, length.out = length(unique_by_levels)) else pred_point_cex
        
        # Offset x-positions and assign aesthetics for interaction levels
        for (i in seq_along(unique_by_levels)) {
          new_data$x_with_offset[new_data[[by]] == unique_by_levels[i]] <-
            x_positions[new_data[[by]] == unique_by_levels[i]] + offset_values[i]
          new_data$point_col[new_data[[by]] == unique_by_levels[i]] <- pred_point_col[i]
          new_data$point_pch[new_data[[by]] == unique_by_levels[i]] <- pred_point_pch[i]
          new_data$point_cex[new_data[[by]] == unique_by_levels[i]] <- pred_point_cex[i]
        }
        x_positions <- new_data$x_with_offset
        
        # Plot raw data points divided by `predictor` and `by` levels if show_data_points is TRUE
        if (show_data_points) {
          for (pred_level in levels(data[[predictor]])) {
            # Convert pred_level to its numeric position (factor index)
            base_x_position <- as.numeric(factor(pred_level, levels = levels(data[[predictor]])))
            
            for (i in seq_along(unique_by_levels)) {
              # Compute offset position for the current `by` level
              raw_x_position <- base_x_position + offset_values[i]
              
              # Subset raw data for the current predictor and "by" level
              subset_data <- data[data[[predictor]] == pred_level & data[[by]] == unique_by_levels[i], ]
              
              # Determine the color for the current "by" level
              current_color <- if (length(point_col) >= i) point_col[i] else point_col[1]
              current_pch <- if (length(point_pch) >= i) point_pch[i] else point_pch[1]
              current_cex <- if (length(point_cex) >= i) point_cex[i] else point_cex[1]
              
              # Plot raw data points for this group
              if (nrow(subset_data) > 0) {
                # Apply jitter only if requested and response is binary (0/1)
                jittered_x <- if (jitter_data_points) {
                  jitter(rep(raw_x_position, nrow(subset_data)))
                } else {
                  rep(raw_x_position, nrow(subset_data))
                }
                points(jittered_x,
                       subset_data[[response]],
                       pch = current_pch,
                       col = current_color,
                       cex = current_cex)
              } else {
                # Placeholder points (jitter not needed here)
                points(rep(raw_x_position, 1),
                       mean(ylim),  # place it in the center just to show axis level
                       pch = current_pch,
                       col = adjustcolor(current_color, alpha.f = 0.2),
                       cex = current_cex)
              }
            }
          }
        }
        arrows(x_positions, preds$lower,
               x_positions, preds$upper,
               angle = 90, code = 3, length = ci_bar_caps, 
               col = ci_bar_col, lty = ci_bar_lty, lwd = ci_bar_lwd)
        # Plot predicted points for interaction levels
        points(x_positions, preds$fit,
               pch = new_data$point_pch, col = new_data$point_col, cex = new_data$point_cex)
        
      } else {
        # Case: without `by`
        # Plot raw data points with jitter (if enabled)
        if (show_data_points) {
          x_vals <- as.numeric(data[[predictor]])
          if (jitter_data_points) {
            x_vals <- jitter(x_vals)
          }
          points(x_vals, data[[response]], pch = point_pch, col = point_col, cex = point_cex)
        }
        arrows(x_positions, preds$lower,
               x_positions, preds$upper,
               angle = 90, code = 3, length = ci_bar_caps, 
               col = ci_bar_col, lty = ci_bar_lty, lwd = ci_bar_lwd)
        # Plot predicted points directly
        points(x_positions, preds$fit,
               pch = pred_point_pch[1],
               col = pred_point_col[1],
               cex = pred_point_cex[1])
      }
      
      # Add custom x-axis for categorical predictors
      if (!is_numeric) {
        axis_args <- list(
          side = 1,
          at = 1:length(levels(data[[predictor]])),
          labels = levels(data[[predictor]]),
          las = las
        )
        
        # Pull relevant axis customization from plot_args
        axis_mods <- plot_args[names(plot_args) %in% c("cex.axis", "col.axis", "font.axis")]
        axis_args <- modifyList(axis_args, axis_mods)
        
        do.call(axis, axis_args)
      }
      
      # Add legend if `add_legend` is TRUE
      if (!is.null(by) && add_legend) {
        # Determine legend labels
        if (!is.null(legend_labels)) {
          final_legend_labels <- legend_labels
        } else if (!is.null(legend_title)) {
          final_legend_labels <- unique_by_levels
        } else {
          final_legend_labels <- paste(by, "=", unique_by_levels)
        }
        
        # Build legend arguments
        legend_args_final <- list(
          legend = final_legend_labels,
          col = pred_point_col,
          pch = pred_point_pch,
          bty = "n",
          cex = legend_label_size,
          horiz = legend_horiz
        )
        
        if (!is.null(legend_title)) {
          legend_args_final$title <- legend_title
          legend_args_final$title.cex <- legend_title_size  # <-- NEW LINE
        }
        
        # Position: named or numeric
        if (is.character(legend_position)) {
          legend_args_final$x <- legend_position
          do.call(legend, modifyList(legend_args_final, legend_args))
        } else if (is.numeric(legend_position) && length(legend_position) == 2) {
          legend_args_final$x <- legend_position[1]
          legend_args_final$y <- legend_position[2]
          do.call(legend, modifyList(legend_args_final, legend_args))
        } else {
          warning("Invalid legend_position. Must be a string or numeric vector of length 2.")
        }
      }
    }
  }
  
  
  #-12---OUTPUT--------------------------------------------------
  
  # Build a data frame with predicted values and 95 CIs (preds_df) cleanly with relevant predictors only
  # Extract variables from formula
  used_terms <- all.vars(formula(model))
  # Remove all components of response
  lhs_expr <- formula(model)[[2]]
  response_vars <- all.vars(lhs_expr)
  # Assemble final variable list
  vars_to_include <- setdiff(used_terms, response_vars)
  
  # Build pred_df
  pred_df <- cbind(
    new_data[, intersect(vars_to_include, names(new_data)), drop = FALSE],
    fit = preds$fit,
    lower = preds$lower,
    upper = preds$upper
  )
  
  # Attach warning attribute if random effects were excluded
  if (isTRUE(all.equal(re.form, NA)) || (inherits(re.form, "formula") && deparse(re.form) == "~0")) {
    attr(pred_df, "re.form_warning") <- "When re.form = NA or ~0, grouping variables 
(random effects) are ignored in predictions."
  }
  
  # Assign custom class to trigger print method
  class(pred_df) <- c("easyviz_pred_df", class(pred_df))
  # Return silently
  invisible(pred_df)
}

#' @export
print.easyviz_pred_df <- function(x, ...) {
  if (!is.null(attr(x, "re.form_warning"))) {
    warning(attr(x, "re.form_warning"), call. = FALSE)
  }
  NextMethod()  # fall back to default print.data.frame
}

