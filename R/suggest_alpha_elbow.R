#' Suggest Elbow (Knee) Point from Alpha Cross-Validation
#'
#' For each alpha divisor, identifies the elbow of the perplexity vs k curve
#' using the maximum-distance-from-chord method — a simple form of the
#' Kneedle algorithm (Satopaa et al., 2011). Returns a suggested k per alpha
#' and prints an overall recommendation.
#'
#' @param results Data frame output from \code{lda_find_alpha()}.
#' @param smoothing One of \code{"none"} (default) or \code{"spline"}. Since
#'   perplexity is already averaged across folds, additional smoothing tends
#'   to shift the elbow toward the asymptote; \code{"none"} typically matches
#'   visual inspection more closely. Use \code{"spline"} if your curves are
#'   noisy (e.g., few folds).
#' @param df Degrees of freedom for \code{smooth.spline} when
#'   \code{smoothing = "spline"} (default: 3). Ignored otherwise.
#'
#' @details
#' Method: for each alpha divisor, perplexity is averaged across folds per k,
#' then (optionally) smoothed. Both axes are rescaled to \code{[0, 1]} and a
#' straight line (chord) is drawn from the first to the last point. The
#' elbow is the point with maximum perpendicular distance to the chord.
#'
#' Elbow detection is inherently approximate. No single method is definitive,
#' so the returned values should be cross-referenced with
#' \code{plot_alpha_crossval()}, \code{plot_alpha_smooth()}, and
#' \code{plot_alpha_second_derivative()}.
#'
#' @return A data frame with one row per alpha divisor and columns:
#'   \itemize{
#'     \item \code{alpha_divisor}: the alpha divisor (alpha = divisor / k)
#'     \item \code{elbow_k}: suggested number of topics at the elbow
#'     \item \code{elbow_perplexity}: perplexity at the elbow (from smoothed
#'       curve if \code{smoothing = "spline"})
#'     \item \code{chord_distance}: max perpendicular distance to the chord
#'       in normalized \code{[0, 1]} space; larger values indicate a sharper
#'       elbow
#'   }
#'   Rows are ordered by \code{alpha_divisor}. The overall recommended
#'   (alpha, k) pair — the row with minimum \code{elbow_perplexity} — is
#'   printed via \code{message()}.
#'
#' @references
#' Satopaa, V., Albrecht, J., Irwin, D., & Raghavan, B. (2011). Finding a
#' "Kneedle" in a Haystack: Detecting Knee Points in System Behavior.
#' \emph{31st International Conference on Distributed Computing Systems
#' Workshops}, 166-171.
#'
#' @importFrom stats smooth.spline predict
#'
#' @export
#'
#' @examples
#' \dontrun{
#' alpha_results <- lda_find_alpha(dtm = my_dtm)
#' elbows <- suggest_alpha_elbow(alpha_results)
#' elbows
#' }
suggest_alpha_elbow <- function(results,
                                smoothing = c("none", "spline"),
                                df = 3) {

  # ---- Validate -----------------------------------------------------------
  required_cols <- c("k", "perplexity", "newalpha")
  if (!all(required_cols %in% names(results))) {
    stop("results must contain columns: ",
         paste(required_cols, collapse = ", "),
         ". Did you pass the output of lda_find_alpha()?")
  }
  smoothing <- match.arg(smoothing)

  # ---- Mean perplexity per (alpha divisor, k) -----------------------------
  # Manual split-apply-combine to avoid formula-based aggregate
  key <- paste(results$newalpha, results$k, sep = "::")
  agg <- do.call(rbind, lapply(split(results, key), function(d) {
    data.frame(
      newalpha   = d$newalpha[1],
      k          = d$k[1],
      perplexity = mean(d$perplexity, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  # ---- Elbow per alpha ----------------------------------------------------
  # Order alphas numerically (newalpha is stored as character)
  alphas <- unique(agg$newalpha)
  alphas <- alphas[order(as.numeric(alphas))]

  find_elbow_one <- function(a) {
    sub <- agg[agg$newalpha == a, ]
    sub <- sub[order(sub$k), ]

    if (nrow(sub) < 3) {
      warning("alpha divisor = ", a,
              " has fewer than 3 k values; skipping elbow detection.")
      return(data.frame(
        alpha_divisor    = as.numeric(a),
        elbow_k          = NA_real_,
        elbow_perplexity = NA_real_,
        chord_distance   = NA_real_
      ))
    }

    x <- sub$k
    y <- sub$perplexity

    # Optional smoothing
    if (smoothing == "spline" && length(unique(x)) >= 4) {
      spl      <- stats::smooth.spline(x, y, df = df)
      y_smooth <- as.numeric(stats::predict(spl, x = x)$y)
    } else {
      y_smooth <- y
    }

    # Normalize both axes to [0, 1]
    xr <- range(x)
    yr <- range(y_smooth)
    if (diff(xr) == 0 || diff(yr) == 0) {
      warning("alpha divisor = ", a,
              " has no variation in k or perplexity; skipping.")
      return(data.frame(
        alpha_divisor    = as.numeric(a),
        elbow_k          = NA_real_,
        elbow_perplexity = NA_real_,
        chord_distance   = NA_real_
      ))
    }
    xn <- (x - xr[1]) / diff(xr)
    yn <- (y_smooth - yr[1]) / diff(yr)

    # Perpendicular distance from each point to the chord (first -> last)
    n <- length(xn)
    A <- yn[n] - yn[1]
    B <- -(xn[n] - xn[1])
    C <- (xn[n] - xn[1]) * yn[1] - (yn[n] - yn[1]) * xn[1]
    dist_to_chord <- abs(A * xn + B * yn + C) / sqrt(A^2 + B^2)

    i <- which.max(dist_to_chord)

    data.frame(
      alpha_divisor    = as.numeric(a),
      elbow_k          = x[i],
      elbow_perplexity = y_smooth[i],
      chord_distance   = dist_to_chord[i]
    )
  }

  out <- do.call(rbind, lapply(alphas, find_elbow_one))
  rownames(out) <- NULL

  # ---- Overall suggestion (messaged, not returned) ------------------------
  if (any(!is.na(out$elbow_perplexity))) {
    best <- out[which.min(out$elbow_perplexity), ]
    message(sprintf(
      "Suggested: alpha divisor = %g, k = %d (perplexity = %.2f, alpha = %.4f)",
      best$alpha_divisor, best$elbow_k, best$elbow_perplexity,
      best$alpha_divisor / best$elbow_k
    ))
    message("Note: elbow detection is approximate. ",
            "Cross-reference with plot_alpha_smooth() and ",
            "plot_alpha_second_derivative().")
  }

  out
}
