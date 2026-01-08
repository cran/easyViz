# ez-plot-numeric.R

ez_plot_numeric <- function(data,
                            new_data,
                            predictor,
                            response,
                            preds,
                            by,
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
                            add_legend,
                            legend_position,
                            legend_horiz,
                            legend_title,
                            legend_labels,
                            legend_title_size,
                            legend_label_size,
                            legend_args) {

  # - numeric/character: sort unique values
  # Plot for numeric predictors (with or without `by`)
  if (!is.null(by)) {
    # IMPORTANT: keep a stable / expected ordering for mapping aesthetics
    # - factors: use their level order
    if (is.factor(data[[by]])) {
      by_levels <- levels(data[[by]])
    } else if (is.numeric(data[[by]])) {
      by_levels <- sort(unique(new_data[[by]]))
    } else {
      # character or other: use sorted unique character
      by_levels <- sort(unique(as.character(new_data[[by]])))
    }
  } else {
    by_levels <- NULL
  }

  # repeat aesthetics to match number of by_levels if needed
  if (!is.null(by)) { # with `by`
    n_levels <- length(by_levels)
    pred_line_col  <- rep(pred_line_col,  length.out = n_levels)
    pred_line_lty  <- rep(pred_line_lty,  length.out = n_levels)
    pred_line_lwd  <- rep(pred_line_lwd,  length.out = n_levels)
    ci_polygon_col <- rep(ci_polygon_col, length.out = n_levels)
    ci_line_col    <- rep(ci_line_col,    length.out = n_levels)
    ci_line_lty    <- rep(ci_line_lty,    length.out = n_levels)
    ci_line_lwd    <- rep(ci_line_lwd,    length.out = n_levels)
  }

  draw_prediction_layers <- function() {
    if (!is.null(by)) { # with `by`
      auto_legend_labels <- vector("character", length(by_levels))
      for (i in seq_along(by_levels)) {
        subset_data  <- new_data[new_data[[by]] == by_levels[i], ]
        subset_lower <- preds$lower[new_data[[by]] == by_levels[i]]
        subset_upper <- preds$upper[new_data[[by]] == by_levels[i]]

        if (!is.null(ci_type)) {
          if (ci_type == "polygon") {
            polygon_x <- c(subset_data[[predictor]], rev(subset_data[[predictor]]))
            polygon_y <- c(subset_upper, rev(subset_lower))
            polygon(polygon_x, polygon_y,
                    col = adjustcolor(ci_polygon_col[i], alpha.f = ci_polygon_alpha),
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
        subset_data  <- new_data[new_data[[by]] == by_levels[i], ]
        subset_preds <- preds$fit[new_data[[by]] == by_levels[i]]
        lines(subset_data[[predictor]], subset_preds,
              col = pred_line_col[i],
              lwd = pred_line_lwd[i],
              lty = pred_line_lty[i])
        auto_legend_labels[i] <- paste(by, "=", by_levels[i])
      }
      if (add_legend) {
        
        # user legend args (may be NULL)
        la <- if (is.null(legend_args)) list() else legend_args
        
        # Determine legend labels
        if (!is.null(legend_labels)) {
          final_legend_labels <- legend_labels
        } else if (!is.null(legend_title)) {
          final_legend_labels <- by_levels
        } else {
          final_legend_labels <- auto_legend_labels
        }
        
        # --- "out" preset (ONLY when legend_position == "out") ---
        out_cfg <- ez_apply_out_legend_preset(
          legend_position = legend_position,
          legend_horiz    = legend_horiz,
          legend_args     = la,
          n_labels        = length(final_legend_labels),
          threshold       = 6
        )
        is_out          <- out_cfg$is_out
        legend_position <- out_cfg$legend_position
        legend_horiz    <- out_cfg$legend_horiz
        la              <- out_cfg$legend_args
        
        # Assemble legend arguments
        legend_args_final <- list(
          legend = final_legend_labels,
          col    = pred_line_col,
          lwd    = pred_line_lwd,
          lty    = pred_line_lty,
          bty    = "n",
          cex    = legend_label_size,
          horiz  = legend_horiz
        )
        
        # Optional auto-inset to avoid overlap in binary-response plots
        is_binary_y <- length(unique(stats::na.omit(data[[response]]))) == 2
        pos <- legend_position
        is_named_pos <- is.character(pos)
        user_supplied_inset <- "inset" %in% names(la)
        
        # IMPORTANT: don't override the "out" preset inset
        if (!is_out && is_binary_y && is_named_pos && !user_supplied_inset) {
          if (pos %in% c("top", "topleft", "topright",
                         "bottom", "bottomleft", "bottomright")) {
            legend_args_final$inset <- c(0, 0.05)
          }
        }
        
        # Add title if specified
        if (!is.null(legend_title)) {
          legend_args_final$title     <- legend_title
          legend_args_final$title.cex <- legend_title_size
        }
        
        # Position: named or numeric
        if (is.character(legend_position)) {
          legend_args_final$x <- legend_position
          do.call(legend, modifyList(legend_args_final, la))
          
        } else if (is.numeric(legend_position) && length(legend_position) == 2) {
          legend_args_final$x <- legend_position[1]
          legend_args_final$y <- legend_position[2]
          do.call(legend, modifyList(legend_args_final, la))
          
        } else {
          warning("Invalid legend_position. 
Must be a string or numeric vector of length 2.")
        }
      }
    } else { # without `by`
      if (!is.null(ci_type)) {
        if (ci_type == "polygon") {
          polygon_x <- c(new_data[[predictor]], rev(new_data[[predictor]]))
          polygon_y <- c(preds$upper, rev(preds$lower))
          polygon(polygon_x, polygon_y,
                  col = adjustcolor(ci_polygon_col[1], alpha.f = ci_polygon_alpha),
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
        breaks <- seq(min(data[[predictor]], na.rm = TRUE),
                      max(data[[predictor]], na.rm = TRUE),
                      length.out = bins + 1)
        data$bin <- cut(data[[predictor]], breaks = breaks, include.lowest = TRUE)
        bin_edges   <- breaks
        bin_centers <- (head(bin_edges, -1) + tail(bin_edges, -1)) / 2
        bin_levels  <- levels(data$bin)

        for (y_val in 0:1) {
          for (i in seq_along(bin_levels)) {
            bin_label  <- bin_levels[i]
            bin_center <- bin_centers[i]
            subset_bin <- data[data$bin == bin_label & data[[response]] == y_val, ]
            if (nrow(subset_bin) > 0) {
              cex_val <- sqrt(nrow(subset_bin)) /
                max(sqrt(table(data$bin)), na.rm = TRUE) *
                point_cex * 2.5
              points(bin_center, y_val,
                     pch = point_pch,
                     col = point_col,
                     cex = cex_val)
            }
          }
        }
      } else {
        points(data[[predictor]], data[[response]],
               pch = point_pch, col = point_col, cex = point_cex)
      }
    } else { # Continuous response
      x_vals <- if (jitter_data_points) jitter(data[[predictor]]) else data[[predictor]]
      points(x_vals, data[[response]],
             pch = point_pch, col = point_col, cex = point_cex)
    }
  }

  # Otherwise, if predictions should go on top of raw data, plot predictions after data
  if (pred_on_top) draw_prediction_layers()
}
