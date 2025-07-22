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
#' If a continuous variable is used, cross-sections are taken at the 10th, 50th, and 90th quantiles.
#' If a categorical variable is used, a separate line or point will be plotted for each level.
#' This can also be used to visualize group-level random effects all at once:
#' namely, when \code{by} corresponds to a grouping variable used in a random effect term (e.g., if \code{by = "group"} when 
#' the random term is specified as \code{(1|group)} or \code{s(group, bs="re")})
#' and \code{re.form = NULL}, predictions are conditional on each group's estimated random effect.
#' Although \code{easyViz} does not natively support direct visualization of three-way interactions in a multi-panel plot,
#' this can be easily achieved by combining the \code{by} and \code{fix_values} arguments.
#' For example, if your model includes a term like \code{x1*x2*x3}, you can visualize the effect of \code{x1}
#' across levels of \code{x2} by setting \code{predictor = "x1"}, \code{by = "x2"}, and fixing \code{x3}
#' at a specific value using \code{fix_values = c(x3 = ...)}.
#' Repeating this with different values of \code{x3} produces multiple plots that can be arranged to visualize the full three-way interaction.
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
#' See the \code{by} argument description for details and an example of how to apply this approach.
#' @param backtransform_response A custom function to back-transform predictions for transformed response variables 
#' (e.g., \code{exp} for log-transformed responses, or \code{function(x) x^2} for square root-transformed responses).
#' \strong{Note:} If you wish to model a transformed response, it is recommended to apply the transformation 
#' directly in the model formula (e.g., \code{log(y)}), rather than modifying the response variable in the data set. 
#' This ensures that observed data points are correctly plotted on the original (back-transformed) scale. 
#' Otherwise, raw data and predicted values may not align properly in the plot.
#' @param re.form A formula specifying which random effects to include when generating predictions.
#' This argument is relevant for mixed-effects models only (e.g., from \code{lme4}, \code{glmmTMB}, or \code{mgcv::gam}).
#' Use \code{re.form = NULL} (default) to include group-specific predictions,
#' conditional on the random-effect levels present in the data.
#' By default, \code{easyViz} fixes grouping variables at their mode (i.e., the most frequent level), 
#' so that, when \code{re.form = NULL}, the prediction reflects the conditional estimate for that group level.
#' However, you can explicitly fix the level of the grouping variable using the \code{fix_values} argument —
#' this allows you to visualize group-specific predictions for a specific level of the random term (e.g., \code{fix_values = c(group = "levelA")}).
#' If all levels are equally frequent and no value is specified via \code{fix_values}, the first level (in factor order) is used,
#' which typically follows alphabetical order unless manually re-leveled.
#' Use \code{re.form = NA} or \code{re.form = ~0} to obtain population-level predictions based only on fixed effects —
#' this means that random effects are part of the model fit but are excluded from the prediction, resulting in population-level (i.e., marginal) 
#' predictions based solely on fixed effects. This is equivalent to assuming the random effects are zero — i.e., an ‘average’ group or subject.
#' For \code{mgcv::gam()} models, random effects are typically modeled using smooth terms such as \code{s(group, bs = "re")}.
#' Although \code{predict.gam()} does not support a \code{re.form} argument, \code{easyViz} emulates its behavior:
#' \code{re.form = NULL} includes random-effect smooths in the prediction, while \code{re.form = NA} or \code{re.form = ~0} excludes them 
#' by internally using the \code{exclude} argument in \code{predict.gam()}.
#' For all types of mixed models, when \code{re.form = NULL} and \code{by} corresponds to a grouping variable used in a random effect term,
#' group-specific (i.e., conditional) predictions are visualized for all levels of the grouping variable.
#' \strong{Note:} For models fitted with \code{lme4} (e.g., \code{lmer()}, \code{glmer()}), standard errors are not available when \code{re.form = NULL}.
#' @param xlim x-axis limits for the plot (e.g., \code{xlim = c(0, 10)}). Defaults to automatic scaling based on the data range.
#' @param ylim y axis limits for the plot (e.g., \code{ylim = c(10, 20)}). Defaults to automatic scaling based on the data and prediction range.
#' @param xlab x axis labels (e.g., \code{xlab = "x"}). Defaults to \code{"predictor"}.
#' @param ylab y axis labels (e.g., \code{ylab = "y"}). Defaults to \code{"response"}.
#' @param font_family Font family for the plot. E.g., \code{"sans"} (default), \code{"serif"}, \code{"mono"}.
#' @param las Text orientation for axis labels (default: \code{1}).
#' @param bty Box type around the plot. E.g., \code{"o"} (default), \code{"n"}, \code{"L"}.
#' @param plot_args A named list of additional graphical parameters passed to base R's \code{plot()} function.
#' These arguments allow users to override default appearance settings in a flexible way.
#' Common options include axis label size, color, margin settings, font family, and tick mark style.
#' Common \code{plot()} parameters you may override:
#' \itemize{
#'   \item {Label/Text size and style:} \code{cex.lab}, \code{cex.axis}, \code{cex.main}, \code{font.lab}, \code{font.axis}, \code{font.main}
#'   \item {Colors:} \code{col.lab}, \code{col.axis}, \code{col.main}, \code{col.sub}, \code{col}, \code{bg}, \code{fg}
#'   \item {Label/Text content:} \code{xlab}, \code{ylab}, \code{main}, \code{sub}
#'   \item {Margins and layout:} \code{mar}, \code{oma}, \code{mgp}, \code{tcl}, \code{las}, \code{adj}
#'   \item {Box and axis rendering:} \code{bty}, \code{axes}, \code{frame.plot}, \code{ann}
#'   \item {Coordinate settings:} \code{xlim}, \code{ylim}, \code{xaxs}, \code{yaxs}, \code{asp}, \code{xlog}, \code{ylog}
#' }
#' This is a flexible alternative to manually setting individual plot parameters in the function signature.
#' For a full list of supported parameters, see \code{?par} and \code{?plot.default}.
#' Example usage: \cr
#' \code{plot_args = list(main = "Title", cex.lab = 1.2, col.axis = "gray40", mar = c(5, 4, 4, 2), las = 1)}.
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
#' @param bins Number of bins for displaying binary response raw data (default: \code{10}).
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
#' @param ci_type Type of 95 percent confidence intervals for numerical predictors. \code{"polygon"} (default) or \code{"lines"}.
#' @param ci_polygon_col Color for 95 percent confidence interval polygon (default: \code{"gray"}). 
#' Requires \code{ci_type = "polygon"}. Can be specified as a color name, number or RGB/hex string. 
#' Dynamic: accepts multiple values (e.g., \code{c("red", "green", "blue")}) 
#' when 95 percent CIs are plotted for multiple lines (i.e., when \code{by} is specified).
#' \strong{Tip:} To hide confidence bands entirely, set this to \code{rgb(0,0,0,0)}.
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
#' @param ci_bar_caps Size of the caps on 95 percent confidence interval bars (default: \code{0.1}).
#' Increase for more visible caps, set to 0 to remove caps and draw plain vertical bars.
#' @param cat_labels Custom labels for levels of a categorical predictor (e.g., \code{cat_labels = c("Level A", "Level B", "Level C")}).
#' @param add_legend Logical. Whether to add a legend for \code{by} variable levels (default: \code{FALSE}).
#' @param legend_position Legend position. Either a named position string (\code{"top"}, \code{"bottom"}, \code{"left"}, 
#' \code{"right"}, \code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, \code{"bottomright"}) or a numeric vector \code{c(x, y)} 
#' specifying exact coordinates for the legend placement.
#' @param legend_text_size Legend text size (default: \code{0.9}).
#' @param legend_labels Custom labels for the legend (e.g., \code{legend_labels = c("Label 1", "Label 2", "Label 3")}).
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
#' Plots are rendered using base R graphics with extensive customization options available through the \code{plot_args} argument.
#' Users can pass any valid graphical parameters accepted by \code{plot} or \code{par},
#' enabling full control over axis labels, font styles, colors, margins, tick orientation, and more.
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
#'         legend_position = "topleft",
#'         legend_labels = c("a", "b", "c"))
#' 
#' # Extract prediction data
#' pred_df <- easyViz(model = mod_lm, data = sim_data, predictor = "x1", by = "x4")
#' head(pred_df)
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
#' #------------------------------------------
#' # 2. Robust linear model (rlm)
#' #------------------------------------------
#' mod_rlm <- rlm(y ~ x1 + x4, 
#'                data = sim_data)
#' easyViz(model = mod_rlm, data = sim_data, predictor = "x1", 
#'         by = "x4",
#'         pred_on_top = TRUE,
#'         bty = "n",
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
#'         legend_position = c(1.25,-1),
#'         legend_labels = c("a", "b", "c"))
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
#'         ylim = c(-15,20),
#'         xlab = "Predictor x4", 
#'         ylab = "Response y",
#'         point_col = c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)),
#'         pred_point_col = c("blue", "red"),
#'         ci_bar_caps = 0,
#'         cat_labels = c("group A", "group B", "group C"),
#'         add_legend = TRUE,
#'         legend_position = "topright",
#'         legend_labels = c("Category A", "Category B"))
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
#'         ci_polygon = rgb(0,0,0,0))
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
#'         point_pch = 1)
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
#' @importFrom stats formula family glm gaussian pcauchy predict quantile model.frame pnorm median complete.cases coef model.matrix vcov getCall terms
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
                    num_conditioning = "median",
                    cat_conditioning = "mode",
                    fix_values = NULL,
                    backtransform_response = NULL,
                    re.form = NULL,
                    xlim = NULL, 
                    ylim = NULL,
                    xlab = NULL, 
                    ylab = NULL,
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
                    cat_labels = NULL,
                    add_legend = FALSE,
                    legend_position = "top",
                    legend_text_size = 0.9,
                    legend_labels = NULL)

{
  if (missing(model)) stop("Argument 'model' is required.")
  if (missing(data)) stop("Argument 'data' is required.")
  if (missing(predictor)) stop("Argument 'predictor' is required.")
  
  # Define helper to allow fitting response in different forms (e.g., y, df$y...) 
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
  
  # Extract response from combination of counts (binomial data)
  unwrap_to_meaningful_expr <- function(expr) {
    if (!inherits(expr, "call")) return(expr)
    head <- as.character(expr[[1]])
    if (head %in% c("(", "c") && length(expr) == 2) {
      return(unwrap_to_meaningful_expr(expr[[2]]))
    }
    return(expr)
  }
  
  lhs_expr <- formula(model)[[2]]
  unwrapped <- unwrap_to_meaningful_expr(lhs_expr)
  
  # Check for use of $ in the response variable
  warn_on_env_prefix <- function(expr) {
    if (!is.call(expr)) return(FALSE)
    head <- as.character(expr[[1]])
    if (head %in% c("$")) return(TRUE)
    any(vapply(as.list(expr), warn_on_env_prefix, logical(1)))
  }
  
  if (warn_on_env_prefix(lhs_expr)) {
    message("Careful! Using `$` in model formulas can produce 
unexpected results. It's safer to specify the model using
the `data` argument instead.")
  }
  
  if (inherits(unwrapped, "call")) {
    fun_name <- as.character(unwrapped[[1]])
    
    if (fun_name == "cbind" && length(unwrapped) == 3) {
      successes_vals <- try(eval(unwrapped[[2]], envir = data), silent = TRUE)
      failures_vals  <- try(eval(unwrapped[[3]], envir = data), silent = TRUE)
      if (inherits(successes_vals, "try-error") || inherits(failures_vals, "try-error")) {
        stop("Could not evaluate cbind(success, failure) response.")
      }
      prop_vals <- successes_vals / (successes_vals + failures_vals)
      response_name <- "Predicted proportion"
      while (response_name %in% names(data)) response_name <- paste0(response_name, "_")
      data[[response_name]] <- prop_vals
      response <- response_name
      message("Detected cbind(success, failure) response. 
Plotting success proportions.")
      
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
      message("Detected successes / trials response. 
Plotting success proportions.")
      
    } else {
      response <- extract_response_name(unwrapped)
    }
    
  } else {
    response <- extract_response_name(unwrapped)
  }
  
  # Identify variables that should be complete (i.e., no NA)
  extract_clean_formula_vars <- function(expr) {
    raw_vars <- all.vars(expr)
    
    # Remove env references like df in df$y
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
  data <- data[complete.cases(data[, model_vars, drop = FALSE]), ]
  
  # Fix_values: warn if variable is not in formula or offset
  if (!is.null(fix_values)) {
    for (var_name in names(fix_values)) {
      if (!(var_name %in% c(formula_vars, offset_vars))) {
        warning(sprintf(
          "Variable '%s' in fix_values is not used in the model 
formula or offset. Fixing it will have no effect on predictions.",
          var_name
        ))
      }
    }
  }
  
  # by: warn if not used in the model
  if (!is.null(by) && !(by %in% c(formula_vars, offset_vars))) {
    warning(sprintf(
      "The 'by' variable '%s' is not used in the model 
formula or offset. Grouping by it will not affect predictions.",
      by
    ))
  }
  
  # Convert all character columns to factors
  char_cols <- sapply(data, is.character)
  data[char_cols] <- lapply(data[char_cols], as.factor)
  
  # Check if the predictor and response are in the dataframe
  if (!(predictor %in% colnames(data))) stop(paste("Predictor", predictor, "not found in data"))
  if (!(response %in% colnames(data))) stop(paste("Response", response, "not found in data"))
  
  # Check valid colors
  validate_color <- function(color) {
    if (is.character(color)) {
      tryCatch({
        grDevices::col2rgb(color)
        return(TRUE)
      }, error = function(e) {
        return(FALSE)
      })
    }
    return(FALSE)
  }
  
  # Validate color arguments
  colors <- c(pred_line_col, point_col, ci_polygon_col, ci_line_col, pred_point_col, ci_bar_col)
  sapply(colors, function(col) if (!validate_color(col)) stop(paste("Invalid color:", col)))
  
  # Check if by is a valid column in the dataframe
  if (!is.null(by) && !(by %in% colnames(data))) stop(paste("Column", by, "not found in data"))
  
  # Convert binary response to numeric (0/1) if it's a factor
  if (is.factor(data[[response]]) && length(levels(data[[response]])) == 2) {
    data[[response]] <- as.numeric(data[[response]]) - 1
  }
  
  # Identify numeric and categorical variables
  is_numeric <- is.numeric(data[[predictor]])
  numeric_vars <- sapply(data, is.numeric)
  
  # Automatically activate categorical x-axis only if predictor is a factor
  add_cat_x_axis <- !is_numeric
  
  if (num_conditioning == "mean") {
    num_values <- sapply(data[, numeric_vars], mean, na.rm = TRUE)
  } else if (num_conditioning == "median") {
    num_values <- sapply(data[, numeric_vars], median, na.rm = TRUE)
  } else {
    stop("Invalid value for num_conditioning. 
Choose 'mean' or 'median'.")
  }
  
  categorical_vars <- sapply(data, is.factor)
  
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
  
  # Define new_data for prediction
  new_data <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(new_data) <- colnames(data)
  
  # Handle case where `by` is not NULL (interaction)
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
    if (!is.null(by) && is.numeric(data[[by]]) && pred_range_limit) {
      message("Note: `pred_range_limit = TRUE` is ignored because 'by' is numeric.\n",
              "Setting `pred_range_limit = FALSE` and using 
the full range of the predictor.")
      pred_range_limit <- FALSE
    }
    
    if (is_numeric) {
      if (pred_range_limit) {
        new_data_list <- lapply(by_levels, function(by_level) {
          subset_data <- data[data[[by]] == by_level, ]
          range_x <- range(subset_data[[predictor]], na.rm = TRUE)
          seq_values <- seq(from = range_x[1], to = range_x[2], length.out = 100)
          df <- data.frame(
            predictor = seq_values,
            by = rep(by_level, length(seq_values))
          )
          names(df) <- c(predictor, by)
          return(df)
        })
        new_data <- do.call(rbind, new_data_list)
      } else {
        # full range
        seq_values <- seq(min(data[[predictor]], na.rm = TRUE), 
                          max(data[[predictor]], na.rm = TRUE), 
                          length.out = 100)
        new_data <- expand.grid(seq_values, by_levels)
        colnames(new_data) <- c(predictor, by)
      }
    } else {
      levels_values <- levels(data[[predictor]])
      new_data <- expand.grid(levels_values, by_levels)
      colnames(new_data) <- c(predictor, by)
    }
    
    # Fill other variables with num_values or cat_values
    for (col_name in colnames(data)) {
      if (col_name != predictor && col_name != by) {
        if (is.numeric(data[[col_name]])) {
          new_data[[col_name]] <- num_values[col_name]
        } else if (is.factor(data[[col_name]])) {
          new_data[[col_name]] <- factor(cat_values[col_name], levels = levels(data[[col_name]]))
        }
      }
    }
    
  } else {
    # If `by` is NULL, just vary the predictor and keep others fixed at num_values or cat_values
    if (is_numeric) {
      seq_values <- seq(min(data[[predictor]], na.rm = TRUE), max(data[[predictor]], na.rm = TRUE), length.out = 100)
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(seq_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- seq_values
    } else {
      levels_values <- levels(data[[predictor]])
      new_data <- data.frame(matrix(ncol = ncol(data), nrow = length(levels_values)))
      colnames(new_data) <- colnames(data)
      new_data[[predictor]] <- levels_values
    }
    
    # Fill other variables with num_values or cat_values
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
or fix_values = list(x = 1, group = 'A'")
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
  
  # Identify model classes
  is_lm_model <- inherits(model, "lm") && !inherits(model, "glm")
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
      warning(sprintf("Model includes offset variable(s) specified outside 
of the formula: %s. These may be ignored during prediction 
for models like lmer()/glmer() or gam().
Consider including the offset inside the formula 
as offset(%s).", paste(offset_vars, collapse = ", "), deparse(offset_expr)
      ))
    }
  }
  
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
    warning("Standard errors are not available for lme4 models 
when re.form = NULL.")
    
  } else if (is_lme4_model || is_glmmTMB_model) {
    # Models that support re.form and se.fit
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link", re.form = re.form)
    
  } else if (is_gam_model) {
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link",
                          exclude = if (exclude_re_smooths) exclude_terms else NULL)
    
  } else if (is_rlm_model) {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required to compute robust SEs for rlm models. Please install it.")
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
    message("Robust standard errors for rlm model computed 
using sandwich estimator (HC0).")
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
  } else if (is_lm_model) {
    # lm does not support type = "link" — use response
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "response")
    
  } else if (is_gls_model) {
    X_new <- model.matrix(formula(model), new_data)
    betas <- coef(model)
    pred_fit <- as.vector(X_new %*% betas)
    
    vcov_mat <- vcov(model)
    se_fit <- sqrt(rowSums((X_new %*% vcov_mat) * X_new))
    
    preds_link <- list(fit = pred_fit, se.fit = se_fit)
    warning("CIs reflect fixed-effect uncertainty only; residual correlation 
structure is ignored, and intervals may be underestimated.")
    
  } else if (is_nls_model) {
    if (!requireNamespace("numDeriv", quietly = TRUE)) {
      stop("Package 'numDeriv' is required for computing approximate 
confidence intervals for nls models. Please install it.")
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
    message("Note: CIs for nls models are approximate and computed 
via the delta method.")
    
  } else if (inherits(model, "glm")) {
    preds_link <- predict(model, newdata = new_data, se.fit = TRUE, type = "link")
    
  } else {
    stop("easyViz does not support this type of model. 
Supported types include: lm, glm, glm.nb, nls, gls, 
lmer/glmer, glmmTMB and gam (mgcv).")
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
        warning("CIs reflect fixed-effect uncertainty only; residual correlation 
structure is ignored, and intervals may be underestimated.")
      }
    }
  }
  
  # Calculate link-scale confidence intervals
  link_fit <- preds_link$fit
  link_se <- preds_link$se.fit
  link_lower <- link_fit - 1.96 * link_se
  link_upper <- link_fit + 1.96 * link_se
  
  # Transform predictions back to the response scale if needed
  # Define a mapping of common link functions to their inverses
  get_linkinv <- function(link) {
    link_inverses <- list(
      logit = function(x) 1 / (1 + exp(-x)),  # Logit
      probit = pnorm,                         # Probit
      cauchit = pcauchy,                      # Cauchit
      cloglog = function(x) 1 - exp(-exp(x)), # Complementary log-log
      log = exp,                              # Log
      sqrt = function(x) x^2,                 # Square root
      inverse = function(x) 1/x,              # Inverse
      identity = identity,                    # Identity
      `1/mu^2` = function(x) 1 / sqrt(x)      # 1/mu^2
    )
    if (link %in% names(link_inverses)) {
      return(link_inverses[[link]])
    } else {
      stop(paste("Unsupported link function:", link))
    }
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
    xaxt = if (add_cat_x_axis) "n" else "s")
  do.call(plot, modifyList(plot_defaults, plot_args))
  
  # Plot for numeric predictors (with or without `by`)
  if (is_numeric) {
    draw_prediction_layers <- function() {
      if (!is.null(by)) {
        auto_legend_labels <- vector("character", length(by_levels))
        for (i in seq_along(by_levels)) {
          subset_data <- new_data[new_data[[by]] == by_levels[i], ]
          subset_preds <- preds$fit[new_data[[by]] == by_levels[i]]
          subset_lower <- preds$lower[new_data[[by]] == by_levels[i]]
          subset_upper <- preds$upper[new_data[[by]] == by_levels[i]]
          
          if (ci_type == "polygon") {
            polygon_x <- c(subset_data[[predictor]], rev(subset_data[[predictor]]))
            polygon_y <- c(subset_upper, rev(subset_lower))
            polygon(polygon_x, polygon_y, col = adjustcolor(ci_polygon_col[i], alpha.f = 0.5), border = NA)
          } else if (ci_type == "lines") {
            lines(subset_data[[predictor]], subset_upper,
                  col = ci_line_col[i], lty = ci_line_lty[i], lwd = ci_line_lwd[i])
            lines(subset_data[[predictor]], subset_lower,
                  col = ci_line_col[i], lty = ci_line_lty[i], lwd = ci_line_lwd[i])
          }
          lines(subset_data[[predictor]], subset_preds, col = pred_line_col[i],
                lwd = pred_line_lwd[i], lty = pred_line_lty[i])
          auto_legend_labels[i] <- paste(by, "=", by_levels[i])
        }
        if (add_legend) {
          final_legend_labels <- if (!is.null(legend_labels)) legend_labels else auto_legend_labels
          if (is.character(legend_position)) {
            if (is.character(legend_position) && length(legend_position) == 1 && legend_position == "outside") {
              legend("right", inset = c(-0.5, 0), xpd = TRUE, bty = "n",
                     legend = final_legend_labels,
                     col = pred_line_col, lwd = pred_line_lwd, lty = pred_line_lty,
                     cex = legend_text_size)
            } else {
              legend(legend_position, legend = final_legend_labels,
                     col = pred_line_col, lwd = pred_line_lwd, lty = pred_line_lty,
                     bty = "n", cex = legend_text_size)
            }
          } else if (is.numeric(legend_position) && length(legend_position) == 2) {
            legend(x = legend_position[1], y = legend_position[2],
                   legend = final_legend_labels,
                   col = pred_line_col, lwd = pred_line_lwd, lty = pred_line_lty,
                   bty = "n", cex = legend_text_size)
          } else {
            warning("Invalid legend_position. Must be a string or numeric vector of length 2.")
          }
        }
      } else {
        if (ci_type == "polygon") {
          polygon_x <- c(new_data[[predictor]], rev(new_data[[predictor]]))
          polygon_y <- c(preds$upper, rev(preds$lower))
          polygon(polygon_x, polygon_y, col = adjustcolor(ci_polygon_col[1], alpha.f = 0.5), border = NA)
        } else if (ci_type == "lines") {
          lines(new_data[[predictor]], preds$upper, col = ci_line_col, lty = ci_line_lty, lwd = ci_line_lwd)
          lines(new_data[[predictor]], preds$lower, col = ci_line_col, lty = ci_line_lty, lwd = ci_line_lwd)
        }
        lines(new_data[[predictor]], preds$fit, col = pred_line_col[1], lwd = pred_line_lwd[1], lty = pred_line_lty[1])
      }
    }
    
    # 1. If predictions should go under raw data
    if (!pred_on_top) draw_prediction_layers()
    
    # 2. Plot raw data
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
    
    # 3. If predictions should go on top of raw data
    if (pred_on_top) draw_prediction_layers()
  } else {
    # Plot for categorical predictors
    if (!is_numeric) {
      # Apply custom labels to categorical predictor
      if (!is.null(cat_labels)) {
        if (length(cat_labels) != length(levels(data[[predictor]]))) {
          stop("Length of cat_labels must match the number of levels in the 
categorical predictor.")
        }
        levels(data[[predictor]]) <- cat_labels
        levels(new_data[[predictor]]) <- cat_labels
      }
      
      # Determine x-axis positions
      x_positions <- as.numeric(new_data[[predictor]])
      
      if (!is.null(by)) {
        # Case: With interactions
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
        # Case: Without interactions
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
      
      # Add custom x-axis for categorical predictors, if enabled
      if (add_cat_x_axis) {
        axis(1, at = 1:length(levels(data[[predictor]])), labels = levels(data[[predictor]]))
      }
      
      # Add legend if `add_legend` is TRUE
      if (!is.null(by) && add_legend) {
        final_legend_labels <- if (!is.null(legend_labels)) legend_labels else paste(by, "=", unique_by_levels)
        legend(legend_position, legend = final_legend_labels,
               col = pred_point_col, pch = pred_point_pch,
               bty = "n", cex = legend_text_size)
      }
    }
  }
  
  # Build preds_df cleanly with relevant predictors only
  # Variables from formula
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
    attr(pred_df, "re.form_warning") <- "When re.form = NA or ~0, grouping variables (random effects) are ignored in predictions."
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
