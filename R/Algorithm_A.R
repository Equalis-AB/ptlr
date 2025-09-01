#' Calculate Robust Mean and Standard Deviation using Algorithm A
#'
#' Implements Algorithm A with iterated scale as described in Annex C.3.1
#' of the ISO 13528:2022(E) standard, with an adjustable degrees of freedom parameter.
#'
#' @param data A numeric vector of data points (e.g., measurements or residuals).
#' @param max_iter An integer specifying the maximum number of iterations.
#' @param tol A numeric value for the convergence tolerance. The iteration stops
#'   when the relative change in both x_star and s_star is less than this tolerance.
#' @param df An integer for the degrees of freedom to use in the denominator of the
#'   standard deviation calculation (`p - df`). Defaults to 1 for standard
#'   univariate estimation. For regression residuals, this would typically be the
#'   number of parameters in the model (intercept + slopes).
#'
#' @return A list with robust mean and sd.
#' \itemize{
#'   \item robust_mean - the robust mean of data.
#'   \item robust_sd - the robust standard deviation of data.
#' }
#' 
#' @examples
#' # --- Validation against Example E.1 from ISO 13528:2022(E) ---
#'
#' # Original participant data from Table E.1, page 68.
#' # Censored values are stored as negative numbers for easy processing.
#' original_data <- c(-10, -10, 12, 19, -20, 20, 23, 23, 25, 25, 26, 28, 28,
#'                    -30, 28, 29, 30, 30, 31, 32, 32, 45, -50)
#'
#' # Scenario 1: '<' ignored (treat '<10' as 10)
#' cat("--- Scenario 1: '<' ignored ---\n")
#' data_ignored <- abs(original_data)
#' result_ignored <- algorithm_A(data_ignored)
#' cat("Calculated x*:", round(result_ignored$robust_mean, 2), "\n")
#' cat("Calculated s*:", round(result_ignored$robust_sd, 2), "\n")
#' cat("Expected x* from Table E.1: 26.01\n")
#' cat("Expected s* from Table E.1: 7.23\n\n")
#'
#' # Scenario 2: '<' deleted (remove results with '<')
#' cat("--- Scenario 2: '<' deleted ---\n")
#' data_deleted <- original_data[original_data > 0]
#' result_deleted <- algorithm_A(data_deleted)
#' cat("Calculated x*:", round(result_deleted$robust_mean, 2), "\n")
#' cat("Calculated s*:", round(result_deleted$robust_sd, 2), "\n")
#' cat("Expected x* from Table E.1: 26.81\n")
#' cat("Expected s* from Table E.1: 5.29\n\n")
#'
#' # Scenario 3: 0.5 * '<' value (replace '<10' with 5)
#' cat("--- Scenario 3: 0.5 * '<' value ---\n")
#' data_half <- ifelse(original_data < 0, 0.5 * abs(original_data), original_data)
#' result_half <- algorithm_A(data_half)
#' cat("Calculated x*:", round(result_half$robust_mean, 2), "\n")
#' cat("Calculated s*:", round(result_half$robust_sd, 2), "\n")
#' cat("Expected x* from Table E.1: 23.95\n")
#' cat("Expected s* from Table E.1: 8.60\n\n")
#'
#' # --- Example: Usage with Regression Residuals ---
#' set.seed(42)
#' x <- 1:30
#' y <- 2 * x + 5 + rnorm(30, mean = 0, sd = 3)
#' y[c(5, 15, 25)] <- c(40, 10, 90) # Add outliers
#'
#' model <- lm(y ~ x)
#' residuals <- resid(model)
#'
#' # Model has 2 parameters (intercept, slope), so df = 2
#' robust_stats_residuals <- algorithm_A(residuals, df = 2)
#'
#' cat("\n--- Robust Statistics for Regression Residuals ---\n")
#' cat("Robust Mean of Residuals (x*):", robust_stats_residuals$robust_mean, "\n")
#' cat("Robust Std. Dev. of Residuals (s*):", robust_stats_residuals$robust_sd, "\n")
#' cat("Compare with non-robust estimate:", summary(model)$sigma, "\n")
#' }
#'
#' @references ISO 13528:2022(E), "Statistical methods for use in proficiency
#'   testing by interlaboratory comparison", Annex C.3.1

Algorithm_A <- function(data, max_iter = 100, tol = 1e-5, df = 1) {
  
  # Ensure data is a numeric vector and remove NAs
  data <- as.numeric(data)
  data <- data[!is.na(data)]
  p <- length(data)
  
  if (p <= df) {
    stop("Length of data must be greater than degrees of freedom (df).")
  }
  
  # Step 1: Calculate initial values for x* and s* (Formulas C.5 and C.6)
  x_star <- median(data)
  s_star <- 1.483 * median(abs(data - x_star))
  
  # NOTE 2 from C.3.1: Handle cases where the initial s* is zero.
  if (s_star == 0) {
    # Calculate initial sd with appropriate degrees of freedom
    s_star <- sqrt(sum((data - mean(data))^2) / (p - df))
  }
  
  # Iteration loop
  for (i in 1:max_iter) {
    
    x_star_old <- x_star
    s_star_old <- s_star
    
    # Step 2: Update values
    # Calculate delta (Formula C.7)
    delta <- 1.5 * s_star
    
    # Create a new data vector x_prime by "winsorizing" (Formula C.8)
    x_prime <- pmin(pmax(data, x_star - delta), x_star + delta)
    
    # Calculate new x* (Formula C.9)
    x_star <- mean(x_prime)
    
    # Calculate new s* (Formula C.10) with specified degrees of freedom
    sum_sq_dev <- sum((x_prime - x_star)^2)
    s_star <- 1.134 * sqrt(sum_sq_dev / (p - df))
    
    # Check for convergence
    # Using a relative tolerance check is a robust programming equivalent.
    if (is.finite(x_star) && is.finite(s_star) &&
        abs((x_star - x_star_old) / x_star_old) < tol &&
        abs((s_star - s_star_old) / s_star_old) < tol) {
      break
    }
  }
  
  # Return the final robust estimates
  return(list(robust_mean = x_star, robust_sd = s_star))
}