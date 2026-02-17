#' Plot Alpha Optimization Cross-Validation Results
#'
#' Creates a boxplot showing perplexity across different topic numbers (k),
#' colored by alpha values from cross-validation results.
#'
#' @param results Data frame output from lda_find_alpha()
#' @param title Main plot title (default: "5-fold cross-validation of topic modelling")
#' @param subtitle Subtitle text (default: "(ie five different models fit for each candidate number of topics)")
#' @param colors Optional color palette for alpha values. If NULL, uses default palette
#'
#' @return A ggplot2 object
#'
#' @importFrom ggplot2 ggplot geom_boxplot aes scale_color_manual ggtitle labs theme_bw theme element_text element_rect margin
#' @importFrom scales hue_pal
#'
#' @export
plot_alpha_crossval <- function(results,
                                title = "5-fold cross-validation of topic modelling",
                                subtitle = "(ie five different models fit for each candidate number of topics)",
                                colors = NULL) {
  
  # Validate input
  required_cols <- c("k", "perplexity", "kalpha", "newalpha2")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Generate color palette if not provided
  if (is.null(colors)) {
    n_colors <- length(unique(results$newalpha2))
    colors <- scales::hue_pal()(n_colors)
  }
  
  # Create plot
  p <- ggplot2::ggplot(results) +
    ggplot2::geom_boxplot(ggplot2::aes(x = k,
                                       y = perplexity,
                                       group = kalpha,
                                       color = factor(newalpha2))) +
    ggplot2::scale_color_manual(values = colors, name = "Alpha") +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::labs(x = "Candidate number of topics",
                  y = "Perplexity when fitting the trained model to the hold-out set") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10, family = "Times New Roman"),
      title = ggplot2::element_text(size = 10, family = "Times New Roman"),
      axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, family = "Times New Roman"),
      axis.text.y = ggplot2::element_text(family = "Times New Roman"),
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      axis.title.y = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      legend.position = "top",
      legend.box = "vertical",
      legend.margin = ggplot2::margin(),
      legend.title = ggplot2::element_text(size = 8),
      legend.key = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = NA),
      legend.text = ggplot2::element_text(size = 8, family = "Times New Roman")
    )
  
  return(p)
}


#' Plot Alpha Optimization with Smooth Lines
#'
#' Creates a smooth line plot showing perplexity trends across different topic
#' numbers (k), with separate lines for each alpha value.
#'
#' @param results Data frame output from lda_find_alpha()
#' @param title Main plot title (default: "")
#' @param x_breaks Numeric vector for x-axis breaks (default: seq(0, 200, by = 10))
#' @param colors Optional color palette for alpha values. If NULL, uses default palette
#'
#' @return A ggplot2 object
#'
#' @importFrom ggplot2 ggplot geom_smooth aes scale_color_manual scale_x_continuous ggtitle xlab ylab theme_bw theme element_text element_rect margin
#' @importFrom scales hue_pal
#'
#' @export
plot_alpha_smooth <- function(results,
                              title = "",
                              x_breaks = seq(from = 0, to = 200, by = 10),
                              colors = NULL) {
  
  # Validate input
  required_cols <- c("k", "perplexity", "newalpha2")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Generate color palette if not provided
  if (is.null(colors)) {
    n_colors <- length(unique(results$newalpha2))
    colors <- scales::hue_pal()(n_colors)
  }
  
  # Create plot
  p <- ggplot2::ggplot(results) +
    ggplot2::geom_smooth(se = FALSE,
                         ggplot2::aes(x = k,
                                      y = perplexity,
                                      color = factor(newalpha2))) +
    ggplot2::scale_color_manual(values = colors, name = "Alpha") +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Candidate number of topics") +
    ggplot2::ylab("Perplexity") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10, family = "Times New Roman"),
      title = ggplot2::element_text(size = 10, family = "Times New Roman"),
      axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, family = "Times New Roman"),
      axis.text.y = ggplot2::element_text(family = "Times New Roman"),
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      axis.title.y = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      legend.position = "top",
      legend.box = "vertical",
      legend.margin = ggplot2::margin(),
      legend.title = ggplot2::element_text(size = 8),
      legend.key = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = NA),
      legend.text = ggplot2::element_text(size = 8, family = "Times New Roman")
    )
  
  return(p)
}


#' Plot Second Derivative of Perplexity
#'
#' Fits a smooth spline to perplexity values and plots the second derivative
#' to identify inflection points, useful for determining optimal topic numbers.
#'
#' @param results Data frame output from lda_find_alpha()
#' @param alpha_value Which alpha value to analyze (default: "10")
#' @param df Degrees of freedom for smooth.spline (default: 3)
#' @param vline_at X-axis position for vertical line. If NULL, no line is drawn (default: 50)
#' @param x_breaks Numeric vector for x-axis breaks (default: seq(0, 200, by = 10))
#' @param title Main plot title (default: " ")
#'
#' @return A ggplot2 object
#'
#' @importFrom ggplot2 ggplot geom_line geom_vline aes scale_x_continuous scale_y_continuous ggtitle xlab ylab theme_bw theme element_text element_rect margin
#' @importFrom stats smooth.spline predict
#' @importFrom stringr str_wrap
#'
#' @export
plot_alpha_second_derivative <- function(results,
                                         alpha_value = "10",
                                         df = 3,
                                         vline_at = 50,
                                         x_breaks = seq(from = 0, to = 200, by = 10),
                                         title = " ") {
  
  # Validate input
  required_cols <- c("k", "perplexity", "newalpha")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Filter for specified alpha and sort by k
  MainDF <- results[results$newalpha == alpha_value, ]
  
  if (nrow(MainDF) == 0) {
    stop("No data found for alpha value: ", alpha_value)
  }
  
  MainDF <- MainDF[order(MainDF$k), ]
  
  # Check if we have enough unique k values for smooth.spline
  n_unique <- length(unique(MainDF$k))
  if (n_unique < 4) {
    stop("Need at least 4 unique k values for second derivative plot. ",
         "Found ", n_unique, " values for alpha = ", alpha_value, ". ",
         "Run lda_find_alpha() with more candidate_k values.")
  }
  
  # Fit smooth spline and calculate second derivative
  cars.spl <- stats::smooth.spline(MainDF$k, MainDF$perplexity, df = df)
  data_temp <- as.data.frame(stats::predict(cars.spl, x = MainDF$k, deriv = 2))
  
  # Create base plot
  p <- ggplot2::ggplot(data_temp, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(color = "grey11")
  
  # Add vertical line if specified
  if (!is.null(vline_at)) {
    p <- p + ggplot2::geom_vline(xintercept = vline_at)
  }
  
  # Add scales and theme
  p <- p +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::scale_y_continuous(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Candidate number of topics") +
    ggplot2::ylab(paste0("Perplexity Second Derivative (alpha ", alpha_value, ")")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 10, family = "Times New Roman"),
      title = ggplot2::element_text(size = 12, family = "Times New Roman"),
      axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, family = "Times New Roman"),
      axis.text.y = ggplot2::element_text(family = "Times New Roman"),
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      axis.title.y = ggplot2::element_text(vjust = -0.25, size = 10, family = "Times New Roman"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = ggplot2::margin(),
      legend.key = ggplot2::element_rect(fill = "white"),
      legend.background = ggplot2::element_rect(fill = NA),
      legend.text = ggplot2::element_text(size = 8, family = "Times New Roman")
    )
  
  return(p)
}


#' Plot Topic Number Optimization Metrics
#'
#' Creates a two-panel plot showing the four metrics for topic number optimization.
#' Upper panel shows metrics to minimize (CaoJuan2009, Arun2010), lower panel shows
#' metrics to maximize (Griffiths2004, Deveaud2014). All metrics are normalized to [0,1].
#'
#' @param results Data frame output from lda_find_topics()
#' @param x_breaks Numeric vector for x-axis breaks (default: seq(0, 200, by = 5))
#'
#' @return A combined ggplot2 object (using ggarrange)
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes scale_color_manual scale_shape_manual scale_x_continuous labs theme_bw theme element_text element_blank
#' @importFrom reshape2 melt
#' @importFrom ggpubr ggarrange
#'
#' @export
plot_topics_metrics <- function(results,
                                x_breaks = seq(0, 200, by = 5)) {
  
  # Validate input
  required_cols <- "topics"
  if (!required_cols %in% names(results)) {
    stop("results must contain column: topics")
  }
  
  # Normalize metrics to [0, 1]
  results_norm <- results
  for (col in names(results)[-1]) {
    if (!all(is.na(results[[col]]))) {
      min_val <- min(results[[col]], na.rm = TRUE)
      max_val <- max(results[[col]], na.rm = TRUE)
      results_norm[[col]] <- (results[[col]] - min_val) / (max_val - min_val)
    }
  }
  
  # Reshape to long format
  results_long <- reshape2::melt(results_norm, id.vars = "topics",
                                 variable.name = "metric", value.name = "value")
  
  # Split by minimize vs maximize
  results_minimize <- results_long[results_long$metric %in% c("CaoJuan2009", "Arun2010"), ]
  results_maximize <- results_long[results_long$metric %in% c("Griffiths2004", "Deveaud2014"), ]
  
  # Plot 1: Minimize metrics
  p1 <- ggplot2::ggplot(results_minimize,
                        ggplot2::aes(x = topics, y = value,
                                     color = metric, shape = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c("CaoJuan2009" = "#bb7438",
                                           "Arun2010"    = "#7f64b9")) +
    ggplot2::scale_shape_manual(values = c("CaoJuan2009" = 17, "Arun2010" = 15)) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::labs(x = NULL, y = "minimize") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 8),
                   legend.position = "right",
                   legend.title    = ggplot2::element_blank())
  
  # Plot 2: Maximize metrics
  p2 <- ggplot2::ggplot(results_maximize,
                        ggplot2::aes(x = topics, y = value,
                                     color = metric, shape = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c("Griffiths2004" = "#72ac5c",
                                           "Deveaud2014"   = "#b94b75")) +
    ggplot2::scale_shape_manual(values = c("Griffiths2004" = 16, "Deveaud2014" = 3)) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::labs(x = "number of topics", y = "maximize") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 8),
                   legend.position = "right",
                   legend.title    = ggplot2::element_blank())
  
  # Combine plots
  combined <- ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2)
  return(combined)
}