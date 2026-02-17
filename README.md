# ldaOptim

> Systematic Parameter Optimization for LDA Topic Modeling

[![R](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![GitHub release](https://img.shields.io/github/v/release/aysedeniz09/ldaOptim)](https://github.com/aysedeniz09/ldaOptim/releases)
[![GitHub last commit](https://img.shields.io/github/last-commit/aysedeniz09/ldaOptim)](https://github.com/aysedeniz09/ldaOptim/commits/main)

## Overview

`ldaOptim` provides a complete, systematic workflow for Latent Dirichlet Allocation (LDA) topic modeling in R. The package addresses the critical challenge of parameter optimization by implementing rigorous cross-validation methods for both alpha (topic concentration) and k (number of topics) parameters. It provides a modern replacement for the deprecated `ldatuning` package.

The package includes functions for alpha optimization through cross-validation, topic number optimization using four complementary metrics (Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014), parallel model fitting, publication-ready visualizations, and Excel export functionality for top words, FREX words, and top documents.

## Installation

Install from GitHub using `devtools`:

```r
# Install devtools if needed
install.packages("devtools")

# Install ldaOptim
devtools::install_github("aysedeniz09/ldaOptim")
```

## Quick Start

```r
library(ldaOptim)

# 1. Preprocess your text data (see "Text Preprocessing" section below)
# This creates: full_data (DTM) and doc_data (metadata)

# 2. Find optimal alpha
alpha_results <- lda_find_alpha(
  dtm = full_data,
  candidate_alpha = c(10, 5, 2, 1),
  candidate_k = seq(5, 100, by = 5),
  folds = 5
)
plot_alpha_crossval(alpha_results)

# 3. Find optimal number of topics
topic_results <- lda_find_topics(
  dtm = full_data,
  topics = seq(10, 100, by = 10),
  control = list(alpha = 5/50)  # Use optimal alpha from Stage 1
)
plot_topics_metrics(topic_results)

# 4. Fit final models
models <- lda_run_models(
  dtm = full_data,
  k_values = c(50, 55, 60),  # Your chosen optimal k values
  alpha_divisor = 5
)

# 5. Export results
export_lda_results(
  model = models$k_50,
  doc_data = doc_data,
  output_prefix = "myproject"
)
```

## Complete Workflow

### Text Preprocessing (Before Using ldaOptim)

This package expects a DocumentTermMatrix as input. Here's the recommended preprocessing workflow:

```r
library(dplyr)
library(tidytext)
library(quanteda)
library(stopwords)
library(stringr)
library(cld2)

# Load your data
data <- read_csv("your_data.csv")
data$index <- 1:nrow(data)
data$text <- data$message  # Or your text column

# Remove duplicates
data <- data[!duplicated(data$text), ]

# SECTION: Preprocessing
data2 <- data

# Filter for English language
data2$CLD2 <- cld2::detect_language(data2$text)
table(data2$CLD2)
data2 <- data2 |> filter(CLD2 == "en")

# Remove URLs
url_pattern <- "(?:http[s]?://)?(?:www\\.)?[a-zA-Z0-9-]+\\.[a-zA-Z]{2,}(?:/\\S*)?"
data2$text <- str_remove_all(data2$message, url_pattern)

# Remove forum quotes
data2$text <- gsub("Quote:.*?Originally Posted by.*?\\n", "", data2$text)

# Filter by word count (remove outliers)
data2$nwords <- str_count(data2$text, "\\w+")
meanwc <- mean(data2$nwords, na.rm = TRUE)
sdwc <- sd(data2$nwords, na.rm = TRUE)
data2 <- data2[which(data2$nwords < (meanwc + 2*sdwc)), ]
data2 <- data2[which(data2$nwords > (meanwc - 2*sdwc)), ]

# SECTION: Tidy Preprocess
data3 <- data2 |> distinct(text, .keep_all = TRUE)

# Create stopword list
mystopwords <- c(
  stopwords("en"),
  stopwords(source = "smart"),
  "posted", "quote", "http", "click", "image",
  "things", "thing", "read", "post",
  "thread", "threads", "ame"
)
mystopwords <- unique(tolower(mystopwords))

# Tokenize and clean
tidy_data <- data3 |>
  unnest_tokens(word, text) |>
  anti_join(data.frame(word = mystopwords)) |>
  mutate(nchar = nchar(word)) |>
  filter(nchar > 2) |>
  filter(!grepl("[0-9]{1}", word)) |>
  filter(!grepl("\\W", word))

# Filter by document frequency
maxndoc <- 0.5
minndoc <- 0.0005
templength <- length(unique(tidy_data$index))

good_common_words <- tidy_data |>
  count(index, word, sort = TRUE) |>
  group_by(word) |>
  summarize(doc_freq = n()/templength) |>
  filter(doc_freq < maxndoc) |>
  filter(doc_freq > minndoc)

tidy_data_pruned <- tidy_data |> inner_join(good_common_words)

# Inspect top words
tidy_data_pruned |>
  group_by(word) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  mutate(word = reorder(word, n)) |>
  top_n(100) |>
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# STOP HERE - check words - correct stopwords if needed - continue only if happy

# Create DFM and convert to topicmodels format
tidy_dfm <- tidy_data_pruned |>
  count(index, word) |>
  cast_dfm(index, word, n)

full_data <- convert(tidy_dfm, to = "topicmodels")

# Create document metadata for later export
doc_data <- data3 |>
  select(index, text) |>
  filter(index %in% rownames(full_data))

# Now ready for ldaOptim!
library(ldaOptim)
```

### Stage 1: Alpha Optimization

Use cross-validation to find the optimal alpha parameter:

```r
alpha_results <- lda_find_alpha(
  dtm = full_data,
  candidate_alpha = c(10, 5, 2, 1),
  candidate_k = c(seq(5, 100, by = 5)),
  folds = 5
  # ncores auto-detected (uses detectCores() - 2 by default)
  # Or specify manually: ncores = 4
)

# Visualize results
plot_alpha_crossval(alpha_results)
plot_alpha_smooth(alpha_results)
plot_alpha_second_derivative(alpha_results, alpha_value = "10")
```

### Stage 2: Topic Number Optimization

Find the optimal number of topics using multiple metrics:

```r
topic_results <- lda_find_topics(
  dtm = full_data,
  topics = seq(10, 100, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  control = list(alpha = 5/50),  # Use optimal alpha from Stage 1
  mc.cores = detectCores() - 6
)

# Visualize all four metrics
plot_topics_metrics(topic_results)
```

### Stage 3: Fit Final Models

Fit LDA models at your chosen optimal k values:

```r
models <- lda_run_models(
  dtm = full_data,
  k_values = c(50, 55, 60),
  alpha_divisor = 5,
  control = list(iter = 2000, seed = 123),
  ncores = 8
)

# Access individual models
model_50 <- models$k_50
```

### Stage 4: Export Results

Export top words, FREX words, and top documents to Excel:

```r
# Export all results for one model
export_lda_results(
  model = models$k_50,
  doc_data = doc_data,
  n_words = 30,
  n_docs = 30,
  output_prefix = "myproject_k50",
  output_dir = "output/"
)

# Or export selectively
top_words <- get_top_words(models$k_50, n_words = 30)
frex_words <- get_frex_words(models$k_50, n_words = 30)
top_docs <- get_top_docs(models$k_50, doc_data = doc_data)
```

## Key Functions

### Optimization
- `lda_find_alpha()` - Cross-validation for alpha parameter
- `lda_find_topics()` - Find optimal number of topics using 4 metrics

### Modeling
- `lda_run_models()` - Fit final LDA models at chosen k values

### Export
- `export_lda_results()` - Export all results (words, FREX, docs) to Excel
- `get_top_words()` - Extract top words by beta probability
- `get_frex_words()` - Extract FREX (frequency-exclusivity) words
- `get_top_docs()` - Extract top documents by gamma probability

### Visualization
- `plot_alpha_crossval()` - Boxplot of alpha cross-validation results
- `plot_alpha_smooth()` - Smooth trend lines for alpha
- `plot_alpha_second_derivative()` - Second derivative analysis
- `plot_topics_metrics()` - Two-panel plot of topic optimization metrics

## Methodology

This package implements the systematic parameter optimization approach described in:

> Jacobi, C., van Atteveldt, W., & Welbers, K. (2016). Quantitative analysis of large amounts of journalistic texts using topic modelling. *Digital Journalism*, 4(1), 89-106. https://doi.org/10.1080/21670811.2015.1093271

### Alpha Optimization

Uses k-fold cross-validation to evaluate different alpha values across multiple topic numbers. Lower perplexity indicates better model fit.

### Topic Number Optimization

Implements four complementary metrics:

- **Griffiths2004**: Log-likelihood (maximize)
- **CaoJuan2009**: Topic similarity (minimize)
- **Arun2010**: Symmetric KL divergence (minimize)
- **Deveaud2014**: Inter-topic divergence (maximize)

## Citation

If you use this package in your research, please cite:

```
Lokmanoglu, A.D., Walter, D., & Ophir, Y. (2026). ldaOptim: Systematic 
  Parameter Optimization for LDA Topic Modeling. R package version 0.1.0.
  https://github.com/aysedeniz09/ldaOptim
```

## References

This package provides a modern replacement for the deprecated `ldatuning` package:

Murzintcev, N. (2020). ldatuning: Tuning of the Latent Dirichlet Allocation Models Parameters. R package version 1.0.2. https://CRAN.R-project.org/package=ldatuning (Archived)

Methodology based on:

Jacobi, C., van Atteveldt, W., & Welbers, K. (2016). Quantitative analysis of large amounts of journalistic texts using topic modelling. *Digital Journalism*, 4(1), 89-106. https://doi.org/10.1080/21670811.2015.1093271

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Authors

- Ayse Deniz Lokmanoglu (Boston University)
- Dror Walter
- Yotam Ophir

## Acknowledgments

- Methodology based on Jacobi et al. (2016) and Murzintcev, N. (2020)
- Built on `topicmodels`, `quanteda`, and `tidytext` packages