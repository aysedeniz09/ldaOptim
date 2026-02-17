#' Find Optimal Number of Topics for LDA
#'
#' Fits LDA models across different topic numbers and calculates four metrics
#' (Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014) to help identify the
#' optimal number of topics. Based on methodology from Jacobi et al. (2016).
#'
#' @param dtm A DocumentTermMatrix from the topicmodels package
#' @param topics Numeric vector of topic numbers to test (default: seq(10, 100, by = 10))
#' @param metrics Character vector of metrics to calculate (default: all four)
#' @param method LDA method: "Gibbs" or "VEM" (default: "Gibbs")
#' @param control List of control parameters passed to LDA (default: list())
#' @param mc.cores Number of cores for parallel processing (default: 1)
#' @param verbose Logical, print progress messages (default: FALSE)
#'
#' @return A data frame with columns: topics, Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014
#'
#' @details
#' Metrics interpretation:
#' - Griffiths2004: MAXIMIZE (higher is better)
#' - CaoJuan2009: MINIMIZE (lower is better)
#' - Arun2010: MINIMIZE (lower is better)
#' - Deveaud2014: MAXIMIZE (higher is better)
#'
#' @importFrom topicmodels LDA
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport parLapply
#' @importFrom slam row_sums
#' @importFrom Rmpfr mpfr mean
#' @importFrom utils tail combn txtProgressBar setTxtProgressBar
#' @importFrom stats median
#'
#' @export
lda_find_topics <- function(dtm,
                            topics  = seq(10, 100, by = 10),
                            metrics = c("Griffiths2004", "CaoJuan2009",
                                        "Arun2010", "Deveaud2014"),
                            method   = "Gibbs",
                            control  = list(),
                            mc.cores = 1L,
                            verbose  = FALSE) {
  
  # Validate inputs
  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("dtm must be a DocumentTermMatrix object")
  }
  
  # Check metric compatibility
  if ("Griffiths2004" %in% metrics) {
    if (method == "VEM") {
      message("Griffiths2004 is incompatible with VEM method, excluding.")
      metrics <- setdiff(metrics, "Griffiths2004")
    } else {
      if (!"keep" %in% names(control)) control <- c(control, keep = 50)
    }
  }
  
  if ("Arun2010" %in% metrics) {
    if (max(topics) > ncol(dtm)) {
      message("Arun2010 requires topics <= ncol(dtm), excluding.")
      metrics <- setdiff(metrics, "Arun2010")
    }
  }
  
  # ---- Metric Helper Functions ----
  
  calc_Griffiths2004 <- function(model, control) {
    burnin <- ifelse("burnin" %in% names(control), control$burnin, 0)
    if (length(model@logLiks) == 0) {
      message("No logLiks were kept. Regenerate model with keep > 0 in control.")
      return(NaN)
    }
    logLiks <- utils::tail(model@logLiks,
                           n = length(model@logLiks) - burnin / control$keep)
    llMed  <- stats::median(logLiks)
    metric <- as.double(
      llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLiks, prec = 2000L) + llMed)))
    )
    return(metric)
  }
  
  calc_CaoJuan2009 <- function(model) {
    m1    <- exp(model@beta)
    pairs <- utils::combn(nrow(m1), 2)
    cos.dist <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
    })
    metric <- sum(cos.dist) / (model@k * (model@k - 1) / 2)
    return(metric)
  }
  
  calc_Arun2010 <- function(model, dtm) {
    len    <- slam::row_sums(dtm)
    m1     <- exp(model@beta)
    m1.svd <- svd(m1)
    cm1    <- as.matrix(m1.svd$d)
    m2     <- model@gamma
    cm2    <- len %*% m2
    norm   <- norm(as.matrix(len), type = "m")
    cm2    <- as.vector(cm2 / norm)
    divergence <- sum(cm1 * log(cm1 / cm2)) + sum(cm2 * log(cm2 / cm1))
    return(divergence)
  }
  
  calc_Deveaud2014 <- function(model) {
    m1 <- exp(model@beta)
    if (any(m1 == 0)) m1 <- m1 + .Machine$double.xmin
    pairs <- utils::combn(nrow(m1), 2)
    jsd   <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      0.5 * sum(x * log(x / y)) + 0.5 * sum(y * log(y / x))
    })
    metric <- sum(jsd) / (model@k * (model@k - 1))
    return(metric)
  }
  
  # ---- Fit Models ----
  
  total_models <- length(topics)
  message(sprintf("Fitting %d models...", total_models))
  
  fit_model <- function(k) {
    if (verbose) message(sprintf("Fitting model with k = %d", k))
    model  <- topicmodels::LDA(dtm, k = k, method = method, control = control)
    result <- list(topics = k)
    if ("Griffiths2004" %in% metrics) result$Griffiths2004 <- calc_Griffiths2004(model, control)
    if ("CaoJuan2009"   %in% metrics) result$CaoJuan2009   <- calc_CaoJuan2009(model)
    if ("Arun2010"      %in% metrics) result$Arun2010      <- calc_Arun2010(model, dtm)
    if ("Deveaud2014"   %in% metrics) result$Deveaud2014   <- calc_Deveaud2014(model)
    if (verbose) message(sprintf("Completed k = %d", k))
    return(result)
  }
  
  # Parallel or sequential processing
  if (mc.cores > 1L) {
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterEvalQ(cl, {
      library(topicmodels)
      library(slam)
      library(Rmpfr)
    })
    parallel::clusterExport(cl,
                            c("dtm", "method", "control", "metrics", "verbose",
                              "calc_Griffiths2004", "calc_CaoJuan2009",
                              "calc_Arun2010", "calc_Deveaud2014"),
                            envir = environment())
    results <- parallel::parLapply(cl, topics, fit_model)
    
  } else {
    pb      <- utils::txtProgressBar(min = 0, max = total_models, style = 3)
    results <- lapply(seq_along(topics), function(i) {
      result <- fit_model(topics[i])
      utils::setTxtProgressBar(pb, i)
      return(result)
    })
    close(pb)
  }
  
  message("Complete!")
  
  # Convert to data frame
  metrics_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      topics        = x$topics,
      Griffiths2004 = if ("Griffiths2004" %in% names(x)) x$Griffiths2004 else NA,
      CaoJuan2009   = if ("CaoJuan2009"   %in% names(x)) x$CaoJuan2009   else NA,
      Arun2010      = if ("Arun2010"      %in% names(x)) x$Arun2010      else NA,
      Deveaud2014   = if ("Deveaud2014"   %in% names(x)) x$Deveaud2014   else NA
    )
  }))
  
  return(metrics_df)
}