# ez-plot-categorical.R

ez_plot_categorical <- function(data,
                                new_data,
                                predictor,
                                response,
                                preds,
                                by,
                                cat_labels,
                                show_data_points,
                                jitter_data_points,
                                point_col,
                                point_pch,
                                point_cex,
                                pred_point_col,
                                pred_point_pch,
                                pred_point_cex,
                                ci_bar_col,
                                ci_bar_lty,
                                ci_bar_lwd,
                                ci_bar_caps,
                                las,
                                plot_args,
                                add_legend,
                                legend_position,
                                legend_horiz,
                                legend_title,
                                legend_labels,
                                legend_title_size,
                                legend_label_size,
                                legend_args) {
  
  # Apply custom labels to categorical predictor
  if (!is.null(cat_labels)) {
    if (length(cat_labels) != length(levels(data[[predictor]]))) {
      stop("Length of cat_labels must match the number of levels 
in the categorical predictor.")
    }
    levels(data[[predictor]]) <- cat_labels
  }
  
  # Determine x-axis positions
  x_positions <- as.numeric(new_data[[predictor]])
  
  if (!is.null(by)) {
    # Case: with `by`
    unique_by_levels <- levels(factor(data[[by]]))
    offset_values <- seq(-0.1, 0.1, length.out = length(unique_by_levels))
    
    # Dynamically set aesthetics for interaction levels
    pred_point_col <- rep(pred_point_col, length.out = length(unique_by_levels))
    pred_point_pch <- rep(pred_point_pch, length.out = length(unique_by_levels))
    pred_point_cex <- rep(pred_point_cex, length.out = length(unique_by_levels))
    
    new_data$x_with_offset <- NA_real_
    new_data$point_col     <- NA
    new_data$point_pch     <- NA
    new_data$point_cex     <- NA_real_
    
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
          current_pch   <- if (length(point_pch) >= i) point_pch[i] else point_pch[1]
          current_cex   <- if (length(point_cex) >= i) point_cex[i] else point_cex[1]
          
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
            ylim <- par("usr")[3:4]
            points(rep(raw_x_position, 1),
                   mean(ylim), # place it in the center just to show axis level
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
           pch = new_data$point_pch,
           col = new_data$point_col,
           cex = new_data$point_cex)
    
  } else {
    # Case: without `by`
    # Plot raw data points with jitter (if enabled)
    if (show_data_points) {
      x_vals <- as.numeric(data[[predictor]])
      if (jitter_data_points) x_vals <- jitter(x_vals)
      points(x_vals, data[[response]],
             pch = point_pch, col = point_col, cex = point_cex)
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
  axis_args <- list(
    side   = 1,
    at     = 1:length(levels(data[[predictor]])),
    labels = levels(data[[predictor]]),
    las    = las
  )
  # Pull relevant axis customization from plot_args
  axis_mods <- plot_args[names(plot_args) %in% c("cex.axis", "col.axis", "font.axis")]
  axis_args <- modifyList(axis_args, axis_mods)
  do.call(axis, axis_args)
  
  # Add legend if `add_legend` is TRUE
  if (!is.null(by) && add_legend) {
    unique_by_levels <- levels(factor(data[[by]]))
    
    # Determine legend labels
    if (!is.null(legend_labels)) {
      final_legend_labels <- legend_labels
    } else if (!is.null(legend_title)) {
      final_legend_labels <- unique_by_levels
    } else {
      final_legend_labels <- paste(by, "=", unique_by_levels)
    }
    
    # user legend args (may be NULL)
    la <- if (is.null(legend_args)) list() else legend_args
    
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
    
    # Build legend arguments
    legend_args_final <- list(
      legend = final_legend_labels,
      col    = pred_point_col,
      pch    = pred_point_pch,
      bty    = "n",
      cex    = legend_label_size,
      horiz  = legend_horiz
    )
    if (!is.null(legend_title)) {
      legend_args_final$title     <- legend_title
      legend_args_final$title.cex <- legend_title_size
    }
    
    if (is.character(legend_position)) {
      
      # Auto-inset to avoid overlap in binary-response plots (e.g., logistic)
      is_binary_y <- length(unique(stats::na.omit(data[[response]]))) == 2
      user_supplied_inset <- "inset" %in% names(la)
      
      # IMPORTANT: don't override the "out" preset inset
      if (!is_out && is_binary_y && !user_supplied_inset &&
          legend_position %in% c("top", "topleft", "topright",
                                 "bottom", "bottomleft", "bottomright")) {
        legend_args_final$inset <- c(0, 0.05)
      }
      
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
}