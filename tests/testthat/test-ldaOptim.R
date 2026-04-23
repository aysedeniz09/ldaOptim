library(testthat)
library(topicmodels)

# Use AssociatedPress as a small reproducible test corpus
data("AssociatedPress", package = "topicmodels")
mini_dtm <- AssociatedPress[1:50, ]  # subset

# ── lda_find_topics ────────────────────────────────────────────────────────────

test_that("lda_find_topics returns a data frame with correct structure", {
  result <- lda_find_topics(
    dtm     = mini_dtm,
    topics  = c(2, 4),
    method  = "Gibbs",
    control = list(seed = 42, keep = 10)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("topics", "Griffiths2004", "CaoJuan2009",
                    "Arun2010", "Deveaud2014") %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("lda_find_topics returns correct number of rows", {
  result <- lda_find_topics(
    dtm    = mini_dtm,
    topics = c(2, 3, 5),
    control = list(seed = 42, keep = 10)
  )
  expect_equal(nrow(result), 3)
})

test_that("lda_find_topics topics column matches input", {
  topics_in <- c(2, 4, 6)
  result <- lda_find_topics(
    dtm     = mini_dtm,
    topics  = topics_in,
    control = list(seed = 42, keep = 10)
  )
  expect_equal(sort(result$topics), sort(topics_in))
})

test_that("lda_find_topics errors on non-DocumentTermMatrix input", {
  expect_error(lda_find_topics(dtm = matrix(1:10, 2, 5), topics = c(2, 4)))
})

# ── lda_find_alpha ────────────────────────────────────────────────────────────

test_that("lda_find_alpha returns a data frame", {
  result <- lda_find_alpha(
    dtm             = mini_dtm,
    candidate_alpha = c(5, 1),
    candidate_k     = c(2, 4),
    folds           = 2,
    seed            = 42,
    ncores = 1
  )
  expect_s3_class(result, "data.frame")
})

test_that("lda_find_alpha contains expected columns", {
  result <- lda_find_alpha(
    dtm             = mini_dtm,
    candidate_alpha = c(5, 1),
    candidate_k     = c(2, 4),
    folds           = 2,
    seed            = 42,
    ncores = 1
  )
  expect_true(all(c("alpha", "k", "perplexity") %in% names(result)))
})

test_that("lda_find_alpha returns numeric perplexity values", {
  result <- lda_find_alpha(
    dtm             = mini_dtm,
    candidate_alpha = c(1),
    candidate_k     = c(2, 4),
    folds           = 2,
    seed            = 42,
    ncores = 1
  )
  expect_true(all(is.numeric(result$perplexity)))
  expect_true(all(result$perplexity > 0))
})

# ── lda_run_models ────────────────────────────────────────────────────────────

test_that("lda_run_models returns a list", {
  result <- lda_run_models(
    dtm      = mini_dtm,
    k_values = c(2, 4),
    control  = list(seed = 42),
    ncores = 1
  )
  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("lda_run_models returns LDA objects", {
  result <- lda_run_models(
    dtm      = mini_dtm,
    k_values = 3,
    control  = list(seed = 42),
    ncores = 1
  )
  expect_s4_class(result[[1]], "LDA")
})

test_that("lda_run_models fitted model has correct number of topics", {
  result <- lda_run_models(
    dtm      = mini_dtm,
    k_values = 5,
    control  = list(seed = 42),
    ncores = 1
  )
  expect_equal(result[[1]]@k, 5)
})

# ── get_top_words ─────────────────────────────────────────────────────────────

test_that("get_top_words returns a data frame", {
  model <- lda_run_models(mini_dtm, k_values = 3, control = list(seed = 42), ncores = 1)[[1]]
  result <- get_top_words(model, n = 5)
  expect_s3_class(result, "data.frame")
})

test_that("get_top_words returns n rows in wide format", {
  model <- lda_run_models(mini_dtm, k_values = 3, control = list(seed = 42), ncores = 1)[[1]]
  result <- get_top_words(model, n = 5)
  expect_equal(nrow(result), 5)       # n rows
  expect_equal(ncol(result), 3)       # one column per topic
})

# ── plot functions ─────────────────────────────────────────────────────────────

test_that("plot_topics_metrics returns a ggplot object", {
  topic_results <- lda_find_topics(
    dtm     = mini_dtm,
    topics  = c(2, 4),
    control = list(seed = 42, keep = 10)
  )
  p <- plot_topics_metrics(topic_results)
  expect_s3_class(p, "ggplot")
})

test_that("plot_alpha_crossval returns a ggplot object", {
  alpha_results <- lda_find_alpha(
    dtm             = mini_dtm,
    candidate_alpha = c(5, 1),
    candidate_k     = c(2, 4),
    folds           = 2,
    seed            = 42,
    ncores = 1
  )
  p <- plot_alpha_crossval(alpha_results)
  expect_s3_class(p, "ggplot")
})

# ── suggest_alpha_elbow ───────────────────────────────────────────────────────

# Synthetic alpha_results with the minimum schema the function expects
# (k, perplexity, newalpha). Using a decaying shape so the elbow is well-defined.
set.seed(42)
alpha_results_mini <- do.call(rbind, lapply(c(1, 2, 5, 10), function(a) {
  do.call(rbind, lapply(c(5, 10, 20, 40, 80, 100), function(k) {
    do.call(rbind, lapply(1:3, function(f) {
      data.frame(
        k          = k,
        perplexity = 700 + 800 * exp(-0.05 * k) + rnorm(1, sd = 5),
        newalpha   = as.character(a),
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

test_that("suggest_alpha_elbow returns a data frame with correct structure", {
  result <- suggest_alpha_elbow(alpha_results_mini)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("alpha_divisor", "elbow_k", "elbow_perplexity",
                    "chord_distance") %in% names(result)))
})

test_that("suggest_alpha_elbow returns one row per alpha divisor", {
  result <- suggest_alpha_elbow(alpha_results_mini)
  expect_equal(nrow(result), 4)  # 4 alpha divisors: 1, 2, 5, 10
})

test_that("suggest_alpha_elbow elbow_k values fall within candidate k range", {
  result <- suggest_alpha_elbow(alpha_results_mini)
  expect_true(all(result$elbow_k >= 5 & result$elbow_k <= 100))
  expect_true(all(is.numeric(result$elbow_k)))
})

test_that("suggest_alpha_elbow smoothing = 'spline' also works", {
  result <- suggest_alpha_elbow(alpha_results_mini, smoothing = "spline")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

test_that("suggest_alpha_elbow errors on invalid input", {
  expect_error(
    suggest_alpha_elbow(data.frame(x = 1:3, y = 4:6)),
    "must contain columns"
  )
})

test_that("suggest_alpha_elbow works on real lda_find_alpha output", {
  alpha_results <- lda_find_alpha(
    dtm             = mini_dtm,
    candidate_alpha = c(5, 1),
    candidate_k     = c(2, 4, 6, 8),
    folds           = 2,
    seed            = 42,
    ncores = 1
  )
  result <- suggest_alpha_elbow(alpha_results)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # 2 alpha divisors
})

# ── suggest_topics_elbow ──────────────────────────────────────────────────────

# Synthetic topic_results with the four standard metrics. Shapes chosen to
# resemble realistic metric curves (monotone for three, peaked for Deveaud).
topic_k <- seq(10, 100, by = 10)
topic_results_mini <- data.frame(
  topics        = topic_k,
  Griffiths2004 = 1000 * log(topic_k),
  CaoJuan2009   = exp(-0.05 * topic_k),
  Arun2010      = 50 * exp(-0.03 * topic_k),
  Deveaud2014   = -((topic_k - 50)^2) / 2000 + 3
)

test_that("suggest_topics_elbow returns a data frame with correct structure", {
  result <- suggest_topics_elbow(topic_results_mini)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("metric", "elbow_k", "elbow_value",
                    "chord_distance") %in% names(result)))
})

test_that("suggest_topics_elbow returns one row per metric", {
  result <- suggest_topics_elbow(topic_results_mini)
  expect_equal(nrow(result), 4)  # 4 metrics
})

test_that("suggest_topics_elbow elbow_k values fall within candidate topics range", {
  result <- suggest_topics_elbow(topic_results_mini)
  expect_true(all(result$elbow_k >= 10 & result$elbow_k <= 100))
})

test_that("suggest_topics_elbow handles subset of metrics", {
  partial <- topic_results_mini
  partial$Arun2010 <- NULL
  result <- suggest_topics_elbow(partial)
  expect_equal(nrow(result), 3)
  expect_false("Arun2010" %in% result$metric)
})

test_that("suggest_topics_elbow smoothing = 'spline' also works", {
  result <- suggest_topics_elbow(topic_results_mini, smoothing = "spline")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

test_that("suggest_topics_elbow errors on missing topics column", {
  expect_error(
    suggest_topics_elbow(data.frame(x = 1:5, Griffiths2004 = 1:5)),
    "must contain a 'topics' column"
  )
})

test_that("suggest_topics_elbow errors when no recognized metrics", {
  expect_error(
    suggest_topics_elbow(data.frame(topics = 1:10, junk = 1:10)),
    "No recognized metrics"
  )
})

test_that("suggest_topics_elbow works on real lda_find_topics output", {
  topic_results <- lda_find_topics(
    dtm     = mini_dtm,
    topics  = c(2, 4, 6, 8),
    control = list(seed = 42, keep = 10)
  )
  result <- suggest_topics_elbow(topic_results)
  expect_s3_class(result, "data.frame")
  expect_true(all(result$elbow_k %in% c(2, 4, 6, 8)))
})