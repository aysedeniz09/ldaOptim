#' Export Top Words from LDA Model
#'
#' Extracts the top n words for each topic based on beta (word-topic probabilities)
#' and exports to Excel.
#'
#' @param model A fitted LDA model from the topicmodels package
#' @param n_words Number of top words to extract per topic (default: 30)
#' @param output_file Path for output Excel file. If NULL, does not save (default: NULL)
#'
#' @return A data frame with topics as columns and top words as rows
#'
#' @importFrom dplyr select arrange top_n
#' @importFrom openxlsx write.xlsx
#'
#' @export
get_top_words <- function(model, n_words = 30, output_file = NULL) {
  
  if (!inherits(model, "LDA")) {
    stop("model must be an LDA object from the topicmodels package")
  }
  
  # Extract the beta (term x topic) matrix
  mybeta <- data.frame(model@beta)
  colnames(mybeta) <- model@terms
  
  # Transpose to see it more comfortably
  mybeta <- t(mybeta)
  colnames(mybeta) <- seq(1:ncol(mybeta))
  
  # Exponentiate the values back as software logs them
  mybeta <- exp(mybeta)
  mybeta <- data.frame(mybeta)
  
  # Create a container
  topwords <- mybeta[1:n_words, ]
  
  # Loop over all topics
for (i in 1:model@k) {
  col_name <- colnames(mybeta)[i]
  tempframe <- mybeta |>
    dplyr::arrange(dplyr::desc(.data[[col_name]])) |>
    dplyr::slice_head(n = n_words) |>
    dplyr::select(dplyr::all_of(col_name))
  
  tempvec <- as.vector(rownames(tempframe))
  topwords[, i] <- tempvec[1:n_words]
}
  
  # Add row names to look pretty
  rownames(topwords) <- c(1:n_words)
  topwords <- as.data.frame(topwords)
  
  # Write to Excel if output_file is provided
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(topwords, output_file)
    message("Top words saved to: ", output_file)
  }
  
  return(topwords)
}


#' Export FREX Words from LDA Model
#'
#' Calculates and exports FREX (frequency-exclusivity) words for each topic.
#' FREX balances word frequency and exclusivity to a topic (Roberts et al.).
#'
#' @param model A fitted LDA model from the topicmodels package
#' @param n_words Number of top FREX words to extract per topic (default: 30)
#' @param weight Weight parameter for FREX calculation (default: 0.3)
#' @param output_file Path for output Excel file. If NULL, does not save (default: NULL)
#'
#' @return A data frame with topics as columns and top FREX words as rows
#'
#' @importFrom openxlsx write.xlsx
#'
#' @export
get_frex_words <- function(model, n_words = 30, weight = 0.3, output_file = NULL) {
  
  if (!inherits(model, "LDA")) {
    stop("model must be an LDA object from the topicmodels package")
  }
  
  # Extract beta matrix
  mybeta <- data.frame(model@beta)
  colnames(mybeta) <- model@terms
  mybeta <- t(mybeta)
  colnames(mybeta) <- seq(1:ncol(mybeta))
  mybeta <- exp(mybeta)
  
  # Apply FREX formula: 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  myw <- weight
  word_beta_sums <- rowSums(mybeta)
  my_beta_for_frex <- mybeta
  
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n, m] <- 1 / (
        myw / (my_beta_for_frex[n, m] / word_beta_sums[n]) + 
          ((1 - myw) / my_beta_for_frex[n, m])
      )
    }
  }
  
  # Print top FREX words
  topwords <- my_beta_for_frex[1:n_words, ]
  
  for (i in 1:model@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[, i]), ]
    tempframe <- tempframe[1:n_words, ]
    tempvec <- as.vector(rownames(tempframe))
    topwords[, i] <- tempvec[1:n_words]
  }
  
  rownames(topwords) <- c(1:n_words)
  topFREX <- as.data.frame(topwords)
  
  # Write to Excel if output_file is provided
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(topFREX, output_file)
    message("FREX words saved to: ", output_file)
  }
  
  return(topFREX)
}


#' Export Top Documents from LDA Model
#'
#' Extracts the top n documents for each topic based on gamma (document-topic probabilities)
#' and exports to Excel.
#'
#' @param model A fitted LDA model from the topicmodels package
#' @param doc_data Data frame containing document IDs and text
#' @param index_col Name of column containing document IDs (default: "index")
#' @param text_col Name of column containing document text (default: "text")
#' @param n_docs Number of top documents to extract per topic (default: 30)
#' @param output_file Path for output Excel file. If NULL, does not save (default: NULL)
#'
#' @return A data frame with topics as columns and top documents as rows
#'
#' @importFrom tidytext tidy
#' @importFrom dplyr select group_by slice_max arrange mutate ungroup left_join
#' @importFrom tidyr pivot_wider
#' @importFrom openxlsx write.xlsx
#'
#' @export
get_top_docs <- function(model, doc_data, index_col = "index", text_col = "text", 
                         n_docs = 30, output_file = NULL) {
  
  if (!inherits(model, "LDA")) {
    stop("model must be an LDA object from the topicmodels package")
  }
  
  if (!all(c(index_col, text_col) %in% names(doc_data))) {
    stop("doc_data must contain columns '", index_col, "' and '", text_col, "'")
  }
  
  # Ensure index is character
  doc_data[[index_col]] <- as.character(doc_data[[index_col]])
  
  # Get gamma matrix in tidy format
  tidy_gamma <- tidytext::tidy(model, matrix = "gamma")
  tidy_gamma$document <- as.character(tidy_gamma$document)
  
  # Create temporary column names for joining
  temp_data <- doc_data |> dplyr::select(dplyr::all_of(c(index_col, text_col)))
  names(temp_data) <- c("doc_id", "doc_text")
  
  # Join to get the corresponding text from doc_data
  gamma_with_text <- tidy_gamma |>
    dplyr::left_join(temp_data, by = c("document" = "doc_id"))
  
  # For each topic, get top texts by gamma
  top_texts_long <- gamma_with_text |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(gamma, n = n_docs, with_ties = FALSE) |>
    dplyr::arrange(topic, -gamma) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::select(topic, rank, doc_text) |>
    dplyr::ungroup()
  
  # Convert to wide format: columns = topics, rows = top texts
  top_texts_wide <- top_texts_long |>
    tidyr::pivot_wider(names_from = topic, values_from = doc_text) |>
    dplyr::arrange(rank) |>
    dplyr::select(-rank)
  
  # Write to Excel if output_file is provided
  if (!is.null(output_file)) {
    openxlsx::write.xlsx(top_texts_wide, output_file)
    message("Top documents saved to: ", output_file)
  }
  
  return(top_texts_wide)
}


#' Export All LDA Results to Excel
#'
#' Convenience wrapper that exports top words, FREX words, and top documents
#' for a fitted LDA model to separate Excel files.
#'
#' @param model A fitted LDA model from the topicmodels package
#' @param doc_data Data frame containing document IDs and text
#' @param index_col Name of column containing document IDs (default: "index")
#' @param text_col Name of column containing document text (default: "text")
#' @param n_words Number of top words/FREX words to extract (default: 30)
#' @param n_docs Number of top documents to extract (default: 30)
#' @param output_prefix Prefix for output files (e.g., "myproject_k50")
#' @param output_dir Directory for output files (default: current directory)
#'
#' @return A list containing three data frames: top_words, frex_words, top_docs
#'
#' @export
export_lda_results <- function(model, 
                               doc_data,
                               index_col = "index",
                               text_col = "text",
                               n_words = 30,
                               n_docs = 30,
                               output_prefix,
                               output_dir = ".") {
  
  k <- model@k
  
  # Create output file paths
  words_file <- file.path(output_dir, paste0(output_prefix, "_TopWords_k", k, ".xlsx"))
  frex_file <- file.path(output_dir, paste0(output_prefix, "_TopFREX_k", k, ".xlsx"))
  docs_file <- file.path(output_dir, paste0(output_prefix, "_TopDocs_k", k, ".xlsx"))
  
  message("Exporting results for k = ", k)
  
  # Export top words
  message("Getting top words...")
  top_words <- get_top_words(model, n_words = n_words, output_file = words_file)
  
  # Export FREX words
  message("Getting FREX words...")
  frex_words <- get_frex_words(model, n_words = n_words, output_file = frex_file)
  
  # Export top documents
  message("Getting top documents...")
  top_docs <- get_top_docs(model, doc_data = doc_data, 
                           index_col = index_col, text_col = text_col,
                           n_docs = n_docs, output_file = docs_file)
  
  message("Export complete!")
  
  return(list(
    top_words = top_words,
    frex_words = frex_words,
    top_docs = top_docs
  ))
}