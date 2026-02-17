#' Run LDA Models for Specified Topic Numbers
#'
#' Fits LDA models in parallel for specific k values after optimization is complete.
#' Useful for fitting final models at your chosen optimal topic numbers.
#'
#' @param dtm A DocumentTermMatrix from the topicmodels package
#' @param k_values Numeric vector of topic numbers to fit (e.g., c(52, 55, 170))
#' @param alpha_divisor Numeric value for alpha divisor. Final alpha = alpha_divisor/k (default: 5)
#' @param method LDA method: "Gibbs" or "VEM" (default: "Gibbs")
#' @param control List of additional control parameters for LDA (default: list(seed = 123))
#' @param ncores Integer number of cores to use. If NULL, uses detectCores() - 2 (default: NULL)
#'
#' @return A named list of fitted LDA models, where names are "k_[number]"
#'
#' @details
#' This function is typically used after lda_find_alpha() and lda_find_topics() to fit
#' the final models at your chosen optimal k values. Each model uses alpha = alpha_divisor/k.
#'
#' @importFrom parallel makeCluster stopCluster detectCores clusterEvalQ clusterExport
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom topicmodels LDA
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After finding optimal k values of 50, 55, and 170
#' models <- lda_run_models(
#'   dtm = my_dtm,
#'   k_values = c(50, 55, 170),
#'   alpha_divisor = 5
#' )
#' 
#' # Access individual models
#' model_50 <- models$k_50
#' model_55 <- models$k_55
#' }
lda_run_models <- function(dtm,
                           k_values,
                           alpha_divisor = 5,
                           method = "Gibbs",
                           control = list(seed = 123),
                           ncores = NULL) {
  
  # Validate inputs
  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("dtm must be a DocumentTermMatrix object from the topicmodels package")
  }
  
  if (missing(k_values) || length(k_values) == 0) {
    stop("k_values must be provided (e.g., c(50, 55, 170))")
  }
  
  if (any(k_values < 2)) {
    stop("All k_values must be at least 2")
  }
  
  if (alpha_divisor <= 0) {
    stop("alpha_divisor must be positive")
  }
  
  # Set up cores
  if (is.null(ncores)) {
    ncores <- parallel::detectCores() - 2
  }
  ncores <- max(1, ncores)
  
  # Create runs dataframe
  runsdf <- data.frame(k = k_values)
  
  message("Fitting ", nrow(runsdf), " LDA models using ", ncores, " cores")
  message("k values: ", paste(k_values, collapse = ", "))
  message("Alpha formula: ", alpha_divisor, "/k")
  message("Started at: ", Sys.time())
  
  # Set up parallel processing
  cluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cluster)
  
  parallel::clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  parallel::clusterExport(cluster,
                          c("dtm", "runsdf", "alpha_divisor", "method", "control"),
                          envir = environment())
  
  # Fit models in parallel
  mymodels <- foreach::foreach(j = 1:nrow(runsdf)) %dopar% {
    k_run <- runsdf[j, 1]
    
    # Calculate alpha for this k
    alpha_run <- alpha_divisor / k_run
    
    # Merge alpha into control list
    control_run <- control
    control_run$alpha <- alpha_run
    
    # Print progress
    print(paste0("Fitting model with k = ", k_run, ", alpha = ", round(alpha_run, 4)))
    
    # Fit model
    fitted <- topicmodels::LDA(dtm, k = k_run, method = method, control = control_run)
    
    print(paste0("Completed k = ", k_run))
    
    return(fitted)
  }
  
  parallel::stopCluster(cluster)
  message("Completed at: ", Sys.time())
  
  # Name the list elements
  names(mymodels) <- paste0("k_", k_values)
  
  return(mymodels)
}