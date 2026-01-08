# ez-plot-dispatch.R

ez_plot_dispatch <- function(prep,
                             new_data,
                             preds,
                             xlim,
                             ylim,
                             xlab,
                             ylab,
                             font_family,
                             las,
                             bty,
                             plot_args,
                             pred_on_top,
                             show_data_points,
                             binary_data_type,
                             bins,
                             jitter_data_points,
                             point_col,
                             point_pch,
                             point_cex,
                             pred_line_col,
                             pred_line_lty,
                             pred_line_lwd,
                             ci_type,
                             ci_polygon_col,
                             ci_polygon_alpha,
                             ci_line_col,
                             ci_line_lty,
                             ci_line_lwd,
                             pred_point_col,
                             pred_point_pch,
                             pred_point_cex,
                             ci_bar_col,
                             ci_bar_lty,
                             ci_bar_lwd,
                             ci_bar_caps,
                             cat_labels,
                             add_legend,
                             legend_position,
                             legend_horiz,
                             legend_title,
                             legend_labels,
                             legend_title_size,
                             legend_label_size,
                             legend_args,
                             ...) {

  data      <- prep$data
  response  <- prep$response
  predictor <- prep$predictor
  by        <- prep$by
  is_numeric <- prep$predictor_is_numeric

  # Color validation
  colors <- unlist(list(pred_line_col, point_col, ci_polygon_col, ci_line_col,
                        pred_point_col, ci_bar_col), use.names = FALSE)
  colors <- colors[!is.na(colors) & nzchar(colors)]
  
  if (length(colors) > 0) {
    bad <- colors[!vapply(colors, ez_validate_color, logical(1))]
    if (length(bad) > 0) stop("Invalid color(s): ", paste(unique(bad), collapse = ", "))
  }
  
  
  if (!is.numeric(ci_polygon_alpha) || ci_polygon_alpha < 0 || ci_polygon_alpha > 1) {
    stop("'ci_polygon_alpha' must be a numeric value between 0 and 1.")
  }

  # For categorical predictors, ensure factor mapping
  if (!is_numeric) {
    # Ensure the levels of the predictor are explicitly set
    if (is.factor(data[[predictor]])) {
      new_data[[predictor]] <- factor(new_data[[predictor]], levels = levels(data[[predictor]]))
    } else {
      # If levels are not explicitly set, default to the order in the data
      new_data[[predictor]] <- factor(new_data[[predictor]], levels = unique(data[[predictor]]))
    }
  }

  # Define plot limits if not provided
  if (is.null(xlim)) {
    if (!is_numeric) {
      num_levels <- nlevels(factor(data[[predictor]]))
      xlim <- c(0.75, num_levels + 0.25)
    } else {
      xlim <- range(new_data[[predictor]], na.rm = TRUE)
    }
  }
  if (is.null(ylim)) {
    if (show_data_points) {
      # include raw data + prediction intervals
      resp_vals <- data[[response]]
      resp_vals[!is.finite(resp_vals)] <- NA
      ylim <- range(c(resp_vals, preds$lower, preds$upper), na.rm = TRUE)
    } else {
      # ignore raw data: only use prediction intervals
      ylim <- range(c(preds$lower, preds$upper), na.rm = TRUE)
    }
  }

  xlab <- ifelse(is.null(xlab), predictor, xlab)
  ylab <- ifelse(is.null(ylab), response,  ylab)

  # Set up the plot
  old <- par(family = font_family)
  on.exit(par(old))

  plot_defaults <- list(
    x    = 1,
    type = "n",
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    las  = las,
    bty  = bty,
    xaxt = if (!is_numeric) "n" else "s"
  )

  # Warn if plot_args includes global par() settings (which won't work here)
  forbidden_plot_args <- c("mar", "oma", "mfrow", "mfcol", "mgp", "xpd", "plt", "mai", "omi")
  if (any(names(plot_args) %in% forbidden_plot_args)) {
    warning("Some entries in 'plot_args' affect global settings
and will be ignored: ",
            paste(intersect(names(plot_args), forbidden_plot_args), collapse = ", "),
            call. = FALSE)
  }

  do.call(plot, modifyList(plot_defaults, plot_args))

  if (is_numeric) {
    ez_plot_numeric(
      data = data,
      new_data = new_data,
      predictor = predictor,
      response = response,
      preds = preds,
      by = by,
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
      add_legend = add_legend,
      legend_position = legend_position,
      legend_horiz = legend_horiz,
      legend_title = legend_title,
      legend_labels = legend_labels,
      legend_title_size = legend_title_size,
      legend_label_size = legend_label_size,
      legend_args = legend_args,
      ...
    )
  } else {
    ez_plot_categorical(
      data = data,
      new_data = new_data,
      predictor = predictor,
      response = response,
      preds = preds,
      by = by,
      cat_labels = cat_labels,
      show_data_points = show_data_points,
      jitter_data_points = jitter_data_points,
      point_col = point_col,
      point_pch = point_pch,
      point_cex = point_cex,
      pred_point_col = pred_point_col,
      pred_point_pch = pred_point_pch,
      pred_point_cex = pred_point_cex,
      ci_bar_col = ci_bar_col,
      ci_bar_lty = ci_bar_lty,
      ci_bar_lwd = ci_bar_lwd,
      ci_bar_caps = ci_bar_caps,
      las = las,
      plot_args = plot_args,
      add_legend = add_legend,
      legend_position = legend_position,
      legend_horiz = legend_horiz,
      legend_title = legend_title,
      legend_labels = legend_labels,
      legend_title_size = legend_title_size,
      legend_label_size = legend_label_size,
      legend_args = legend_args,
      ...
    )
  }
  
  invisible(TRUE)
}
