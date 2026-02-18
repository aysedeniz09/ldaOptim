#' Find Optimal Alpha Parameter for LDA
#'
#' Performs k-fold cross-validation to identify optimal alpha values across
#' different topic numbers (k). Uses parallel processing for efficiency.
#'
#' @param dtm A DocumentTermMatrix from the topicmodels package
#' @param candidate_alpha Numeric vector of alpha divisors to test (default: c(10, 5, 2, 1))
#' @param candidate_k Numeric vector of topic numbers to test (default: c(2, seq(5, 100, by=5), 125, 150, 175, 200))
#' @param folds Integer number of cross-validation folds (default: 5)
#' @param ncores Integer number of cores to use. If NULL, uses detectCores() - 2 (default: NULL)
#' @param seed Integer seed for reproducibility (default: 123)
#'
#' @return A data frame with columns: k, alpha, fold, perplexity, kalpha, newalpha, newalpha2
#'
#' @importFrom parallel makeCluster stopCluster detectCores clusterEvalQ clusterExport
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom topicmodels LDA perplexity
#'
#' @export
lda_find_alpha <- function(dtm,
                           candidate_alpha = c(10, 5, 2, 1),
                           candidate_k = c(2, seq(5, 100, by = 5), 125, 150, 175, 200),
                           folds = 5,
                           ncores = NULL,
                           seed = 123) {
  
  # Validate inputs
  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("dtm must be a DocumentTermMatrix object from the topicmodels package")
  }
  if (folds < 2) {
    stop("folds must be at least 2")
  }
  if (any(candidate_alpha <= 0)) {
    stop("All candidate_alpha values must be positive")
  }
  if (any(candidate_k < 2)) {
    stop("All candidate_k values must be at least 2")
  }
  
  # Set up cores
  if (is.null(ncores)) {
    ncores <- parallel::detectCores() - 2
  }
  ncores <- max(1, ncores)  # Ensure at least 1 core
  
  # Create fold assignments
  n <- nrow(dtm)
  set.seed(seed)
  splitfolds <- sample(1:folds, n, replace = TRUE)
  
  # Build validation queue
  validationQueueDF <- expand.grid(
    k = candidate_k,
    alpha_divisor = candidate_alpha,
    fold = 1:folds
  )
  validationQueueDF$alpha <- validationQueueDF$alpha_divisor / validationQueueDF$k
  validationQueueDF <- validationQueueDF[, c("k", "alpha", "fold")]
  validationQueueDF <- validationQueueDF[order(-validationQueueDF$k), ]
  
  # Run cross-validation
  total_models <- nrow(validationQueueDF)
  message("Starting validation of ", total_models, " models using ", ncores, " cores")
  message("Started at: ", Sys.time())
  
  cluster <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cluster)
  
  parallel::clusterEvalQ(cluster, library(topicmodels))
  parallel::clusterExport(cluster,
                          c("dtm", "splitfolds", "validationQueueDF"),
                          envir = environment())
  
  results <- foreach::foreach(
    j = 1:nrow(validationQueueDF),
    k_val = validationQueueDF$k,
    alpha_val = validationQueueDF$alpha,
    fold_val = validationQueueDF$fold,
    .combine = rbind,
    .packages = "topicmodels"
  ) %dopar% {
    
    total_models <- nrow(validationQueueDF)
    remaining <- total_models - j
    percent_complete <- round((j / total_models) * 100, 1)
    
    print(paste0(
      "[", j, "/", total_models, "] (", percent_complete, "% complete, ",
      remaining, " remaining) - K:", k_val,
      " | Alpha:", round(alpha_val, 3),
      " | Fold:", fold_val
    ))
    
    train_set <- dtm[splitfolds != fold_val, ]
    valid_set <- dtm[splitfolds == fold_val, ]
    
    fitted <- topicmodels::LDA(
      train_set,
      k = k_val,
      method = "Gibbs",
      control = list(alpha = alpha_val, seed = seed)
    )
    
    perp <- topicmodels::perplexity(fitted, newdata = valid_set)
    
    print(paste0(
      "Completed [", j, "/", total_models, "] - Perplexity: ",
      round(perp, 2)
    ))
    
    data.frame(k = k_val, alpha = alpha_val, fold = fold_val, perplexity = perp)
  }
  
  parallel::stopCluster(cluster)
  message("Completed at: ", Sys.time())
  
  # Add derived columns
  results$kalpha <- paste0(as.character(results$k), "_", results$alpha)
  results$newalpha <- as.character(results$alpha * results$k)
  results$newalpha2 <- sprintf("%02d", as.integer(results$alpha * results$k))
  
  return(results)
}