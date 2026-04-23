#' Suggest Optimal Topic Number from lda_find_topics() Output
#'
#' For each metric in the results, identifies the elbow of the metric curve
#' via the maximum-distance-from-chord method (Kneedle; Satopaa et al., 2011).
#' Prints a consensus range across metrics.
#'
#' @param results Data frame output from \code{lda_find_topics()}.
#' @param smoothing One of \code{"none"} (default) or \code{"spline"}. Use
#'   \code{"spline"} only if your metric curves are visibly noisy; the
#'   smoother tends to shift elbows toward the asymptote, so
#'   \code{"none"} typically matches visual inspection more closely.
#' @param df Degrees of freedom for \code{smooth.spline} when
#'   \code{smoothing = "spline"} (default: 3). Ignored otherwise.
#'
#' @details
#' After rescaling both axes to \code{[0, 1]}, the elbow is the point with
#' maximum perpendicular distance to the chord connecting the first and last
#' points. This is direction-agnostic: it works for increasing, decreasing,
#' and peaked curves. The approach is applied uniformly across the four
#' supported metrics (\code{Griffiths2004}, \code{CaoJuan2009},
#' \code{Arun2010}, \code{Deveaud2014}) regardless of whether each should
#' be minimized or maximized.
#'
#' Metrics frequently disagree. The returned table and consensus message are
#' intended to narrow the search, not to replace visual inspection with
#' \code{plot_topics_metrics()}.
#'
#' @return A data frame with one row per metric and columns:
#'   \itemize{
#'     \item \code{metric}: metric name
#'     \item \code{elbow_k}: k at the elbow of the curve
#'     \item \code{elbow_value}: metric value (smoothed if
#'       \code{smoothing = "spline"}) at \code{elbow_k}
#'     \item \code{chord_distance}: max perpendicular distance to the chord
#'       in normalized \code{[0, 1]} space; larger values indicate a sharper
#'       elbow
#'   }
#'
#' @references
#' Satopaa, V., Albrecht, J., Irwin, D., & Raghavan, B. (2011). Finding a
#' "Kneedle" in a Haystack: Detecting Knee Points in System Behavior.
#' \emph{31st International Conference on Distributed Computing Systems
#' Workshops}, 166-171.
#'
#' Jacobi, C., van Atteveldt, W., & Welbers, K. (2016). Quantitative analysis
#' of large amounts of journalistic texts using topic modelling.
#' \emph{Digital Journalism}, 4(1), 89-106.
#'
#' @importFrom stats smooth.spline predict median
#'
#' @export
#'
#' @examples
#' \dontrun{
#' topic_results <- lda_find_topics(dtm = my_dtm,
#'                                  topics = seq(10, 200, by = 10))
#' suggestions <- suggest_topics_elbow(topic_results)
#' suggestions
#' }
suggest_topics_elbow <- function(results,
                                 smoothing = c("none", "spline"),
                                 df = 3) {

  # ---- Validate -----------------------------------------------------------
  if (!"topics" %in% names(results)) {
    stop("results must contain a 'topics' column. ",
         "Did you pass the output of lda_find_topics()?")
  }
  smoothing <- match.arg(smoothing)

  # Recognized metrics
  known_metrics <- c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")

  available <- intersect(known_metrics, names(results))
  if (length(available) == 0) {
    stop("No recognized metrics found in results. ",
         "Expected one or more of: ",
         paste(known_metrics, collapse = ", "))
  }

  # ---- Per-metric analysis ------------------------------------------------
  analyze_metric <- function(m) {
    x <- results$topics
    y <- results[[m]]

    ok <- !is.na(y)
    x  <- x[ok]
    y  <- y[ok]

    empty_row <- data.frame(
      metric         = m,
      elbow_k        = NA_real_,
      elbow_value    = NA_real_,
      chord_distance = NA_real_,
      stringsAsFactors = FALSE
    )

    if (length(x) < 3) {
      warning("metric '", m,
              "' has fewer than 3 non-NA values; skipping.")
      return(empty_row)
    }

    ord <- order(x)
    x   <- x[ord]
    y   <- y[ord]

    # Optional smoothing
    if (smoothing == "spline" && length(unique(x)) >= 4) {
      spl      <- stats::smooth.spline(x, y, df = df)
      y_smooth <- as.numeric(stats::predict(spl, x = x)$y)
    } else {
      y_smooth <- y
    }

    xr <- range(x)
    yr <- range(y_smooth)
    if (diff(xr) == 0 || diff(yr) == 0) {
      warning("metric '", m,
              "' has no variation; skipping elbow detection.")
      return(empty_row)
    }

    xn <- (x - xr[1]) / diff(xr)
    yn <- (y_smooth - yr[1]) / diff(yr)

    n <- length(xn)
    A <- yn[n] - yn[1]
    B <- -(xn[n] - xn[1])
    C <- (xn[n] - xn[1]) * yn[1] - (yn[n] - yn[1]) * xn[1]
    dist_to_chord <- abs(A * xn + B * yn + C) / sqrt(A^2 + B^2)

    i <- which.max(dist_to_chord)

    data.frame(
      metric         = m,
      elbow_k        = x[i],
      elbow_value    = y_smooth[i],
      chord_distance = dist_to_chord[i],
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, lapply(available, analyze_metric))
  rownames(out) <- NULL

  # ---- Consensus message --------------------------------------------------
  ks <- out$elbow_k[!is.na(out$elbow_k)]
  if (length(ks) == 1) {
    message(sprintf("Elbow points suggest: k = %d (1 metric).",
                    as.integer(ks)))
  } else if (length(ks) > 1) {
    message(sprintf(
      "Elbow points suggest: k in [%d, %d], median = %d (%d metrics).",
      as.integer(min(ks)), as.integer(max(ks)),
      as.integer(stats::median(ks)),
      length(ks)
    ))
  }
  message("Note: metrics often disagree. ",
          "Cross-reference with plot_topics_metrics().")

  out
}
