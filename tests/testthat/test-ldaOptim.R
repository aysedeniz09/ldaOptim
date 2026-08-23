# Tests for get_top_words(), get_frex_words(), get_top_docs(), and
# export_lda_results() -- specifically the `format` argument ("xlsx", "csv", "rda").
#
# A lightweight mock LDA object is used instead of fitting a real model, since
# these functions only touch @beta, @gamma, @k, @terms, and @documents.
# `methods::new("LDA", ...)` resolves to the real topicmodels::LDA class,
# which is available here because ldaOptim Imports topicmodels.

make_mock_lda <- function(n_terms = 10, n_docs = 8, k = 3, seed = 1) {
  set.seed(seed)

  terms <- paste0("word", seq_len(n_terms))
  docs  <- paste0("doc", seq_len(n_docs))

  raw_beta <- matrix(stats::runif(k * n_terms), nrow = k)
  beta_log <- log(raw_beta / rowSums(raw_beta))

  raw_gamma <- matrix(stats::runif(n_docs * k), nrow = n_docs)
  gamma_mat <- raw_gamma / rowSums(raw_gamma)

  # topicmodels::LDA is a virtual class - real fitted models are always a
  # concrete subclass. lda_run_models() uses method = "Gibbs" by default, so
  # LDA_Gibbs (which inherits from LDA) is the closest match to what's
  # actually passed to these functions in practice.
  methods::new("LDA_Gibbs",
               beta = beta_log,
               gamma = gamma_mat,
               k = as.integer(k),
               terms = terms,
               documents = docs)
}

make_mock_doc_data <- function(model) {
  data.frame(
    index = model@documents,
    text  = paste("full text of", model@documents),
    stringsAsFactors = FALSE
  )
}

# small local helper so this file has no dependency on the withr package
withr_tempdir <- function() {
  d <- tempfile("ldaoptim-test-")
  dir.create(d)
  d
}

# ---- format validation ----

test_that("get_top_words rejects an invalid format", {
  model <- make_mock_lda()
  expect_error(
    get_top_words(model, n_words = 3, output_file = tempfile(), format = "json"),
    "should be one of"
  )
})

test_that("get_frex_words rejects an invalid format", {
  model <- make_mock_lda()
  expect_error(
    get_frex_words(model, n_words = 3, output_file = tempfile(), format = "json"),
    "should be one of"
  )
})

test_that("get_top_docs rejects an invalid format", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  expect_error(
    get_top_docs(model, doc_data = doc_data, n_docs = 3,
                 output_file = tempfile(), format = "json"),
    "should be one of"
  )
})

test_that("export_lda_results rejects an invalid format", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  expect_error(
    export_lda_results(model, doc_data = doc_data, n_words = 3, n_docs = 3,
                       output_prefix = "test", output_dir = tempdir(),
                       format = "json"),
    "should be one of"
  )
})

# ---- output_file = NULL never writes, regardless of format default ----

test_that("output_file = NULL returns a data frame without writing anything", {
  model <- make_mock_lda()
  before <- list.files(tempdir())

  result <- get_top_words(model, n_words = 3, output_file = NULL)

  expect_s3_class(result, "data.frame")
  expect_identical(list.files(tempdir()), before)
})

# ---- each format actually writes the expected file, for each function ----

test_that("get_top_words writes the correct extension for each format", {
  model <- make_mock_lda()
  dir <- withr_tempdir()

  for (fmt in c("xlsx", "csv", "rda")) {
    path <- file.path(dir, paste0("words_", fmt))
    res <- get_top_words(model, n_words = 3, output_file = path, format = fmt)
    expect_true(file.exists(paste0(path, ".", fmt)))
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), model@k)
  }
})

test_that("get_frex_words writes the correct extension for each format", {
  model <- make_mock_lda()
  dir <- withr_tempdir()

  for (fmt in c("xlsx", "csv", "rda")) {
    path <- file.path(dir, paste0("frex_", fmt))
    res <- get_frex_words(model, n_words = 3, output_file = path, format = fmt)
    expect_true(file.exists(paste0(path, ".", fmt)))
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), model@k)
  }
})

test_that("get_top_docs writes the correct extension for each format", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  dir <- withr_tempdir()

  for (fmt in c("xlsx", "csv", "rda")) {
    path <- file.path(dir, paste0("docs_", fmt))
    res <- get_top_docs(model, doc_data = doc_data, n_docs = 3,
                         output_file = path, format = fmt)
    expect_true(file.exists(paste0(path, ".", fmt)))
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), model@k)
  }
})

# ---- rda files save under the expected object name, not an internal variable name ----

test_that("get_top_words .rda file contains an object named top_words", {
  model <- make_mock_lda()
  path <- file.path(withr_tempdir(), "words_rda_name")
  get_top_words(model, n_words = 3, output_file = path, format = "rda")

  e <- new.env()
  load(paste0(path, ".rda"), envir = e)
  expect_identical(ls(e), "top_words")
})

test_that("get_frex_words .rda file contains an object named frex_words", {
  model <- make_mock_lda()
  path <- file.path(withr_tempdir(), "frex_rda_name")
  get_frex_words(model, n_words = 3, output_file = path, format = "rda")

  e <- new.env()
  load(paste0(path, ".rda"), envir = e)
  expect_identical(ls(e), "frex_words")
})

test_that("get_top_docs .rda file contains an object named top_docs", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  path <- file.path(withr_tempdir(), "docs_rda_name")
  get_top_docs(model, doc_data = doc_data, n_docs = 3,
               output_file = path, format = "rda")

  e <- new.env()
  load(paste0(path, ".rda"), envir = e)
  expect_identical(ls(e), "top_docs")
})

# ---- rda round-trips exactly to the object returned in-session ----

test_that("get_top_docs .rda round-trips to the same data returned in-session", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  path <- file.path(withr_tempdir(), "docs_roundtrip")

  returned <- get_top_docs(model, doc_data = doc_data, n_docs = 3,
                            output_file = path, format = "rda")

  e <- new.env()
  load(paste0(path, ".rda"), envir = e)
  expect_equal(e$top_docs, returned, ignore_attr = TRUE)
})

# ---- export_lda_results passes format through to all three sub-exports ----

test_that("export_lda_results passes format through and names files consistently", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  dir <- withr_tempdir()

  result <- export_lda_results(
    model = model,
    doc_data = doc_data,
    n_words = 3,
    n_docs = 3,
    output_prefix = "MockRun",
    output_dir = dir,
    format = "csv"
  )

  expect_named(result, c("top_words", "frex_words", "top_docs"))
  expect_true(file.exists(file.path(dir, paste0("MockRun_TopWords_k", model@k, ".csv"))))
  expect_true(file.exists(file.path(dir, paste0("MockRun_TopFREX_k", model@k, ".csv"))))
  expect_true(file.exists(file.path(dir, paste0("MockRun_TopDocs_k", model@k, ".csv"))))
})

test_that("export_lda_results defaults to xlsx when format is not specified", {
  model <- make_mock_lda()
  doc_data <- make_mock_doc_data(model)
  dir <- withr_tempdir()

  export_lda_results(
    model = model,
    doc_data = doc_data,
    n_words = 3,
    n_docs = 3,
    output_prefix = "MockRunDefault",
    output_dir = dir
  )

  expect_true(file.exists(file.path(dir, paste0("MockRunDefault_TopWords_k", model@k, ".xlsx"))))
})