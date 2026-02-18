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