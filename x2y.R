# thanks to https://github.com/rama100/x2y/blob/main/x2y.R
require(dplyr)
require(rpart)
require(furrr)
require(progressr)
furrr_options(seed=42)
plan(multisession, workers = 8)

# TODO add a col selector function
get_high_mi_cols <- function(dx2y_df, threshold=99, keep="first") {
  if (keep!="first") {
    rlang::abort("'longest' method is not implemented yet")
  }
  dx2y_df %>% filter(x2y >= threshold) %>% pull(y) %>% unique
}

calc_mae_reduction <- function(y_hat, y_actual) {
  model_error <- mean(abs(y_hat - y_actual))
  baseline <- mean(y_actual, na.rm = TRUE)
  baseline_error <-  mean(abs(baseline - y_actual))
  result <- 1 - model_error/(baseline_error+1e-14)
  # cat("MAE - baseline:", baseline_error, "\n")
  # cat("MAE - model:", model_error, "\n")
  # cat("MAE - before cleaning up:", result, "\n")
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

calc_misclass_reduction <- function(y_hat, y_actual) {
  tab <- table(y_hat, y_actual)
  model_error <- 1 - sum(diag(tab))/sum(tab)
  majority_class <- names(which.max(table(y_actual)))
  baseline.preds <- rep(majority_class, length(y_actual))
  baseline_error <- mean(baseline.preds != y_actual)
  result <- 1 - model_error/(baseline_error+1e-14)
  # cat("MISCLASS - baseline:", baseline_error, "\n")
  # cat("MISCLASS - model:", model_error, "\n")
  # cat("MISCLASS - before cleaning up:", result, "\n")
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

x2y_inner <- function(x, y) {
  
  if (length(unique(x)) == 1 |
      length(unique(y)) == 1 ) {
    return(NA)
  } 
  # cast POSIXct, dates, difftime, ...
  if (mode(x)=="numeric") x <- as.numeric(x)
  if (mode(y)=="numeric") y <- as.numeric(y)

  # if y is continuous
  if (is.numeric(y) && !is.factor(y)) {
    preds <- predict(rpart(y ~ x, method = "anova"), type = 'vector')
    calc_mae_reduction(preds, y)
  }
  # if y is categorical
  else {
    preds <- predict(rpart(y ~ x, method = "class"), type = 'class')
    calc_misclass_reduction(preds, y)
  }
}


simple_boot <- function(x,y) {
  ids <- sample(length(x), replace = TRUE)
  x2y_inner(x[ids], y[ids])
}

x2y <- function(x, y, confidence = FALSE, sample_n = 5000) {
  results <- list()
  
  missing <-  is.na(x) | is.na(y)
  results$perc_of_obs <- round(100 * (1 - sum(missing) / length(x)), 2)
  
  x <- x[!missing]
  y <- y[!missing]
  
  if (is.null(sample_n)) sample_n <- length(x)
  if (length(x)>sample_n) {
    ids <- sample(length(x), sample_n)
    x <- x[ids]
    y <- y[ids]
  }
  
  results$x2y <- x2y_inner(x, y)
  
  if (confidence) {
    results$CI_95_Lower = NA
    results$CI_95_Upper = NA
    if (!is.na(results$x2y) & results$x2y > 0) {
      n <- length(x)
      draws <- replicate(1000, simple_boot(x, y))
      errors <- draws - results$x2y
      results$CI_95_Lower <- results$x2y - round(quantile(errors,
                                                          probs = 0.975,
                                                          na.rm = TRUE), 2)
      results$CI_95_Upper <- results$x2y - round(quantile(errors,
                                                          probs = 0.025,
                                                          na.rm = TRUE), 2)
    }
  }
  results
}

dx2y <- function(d, target = NA, confidence = FALSE, sample_n=5000) {
  if (is.na(target)) {
    pairs <- combn(ncol(d), 2)
    pairs <- cbind(pairs, pairs[2:1, ])
  }
  else {
    n <- 1:ncol(d)
    idx <- which(target == names(d))
    n <- n[n != idx]
    pairs <- cbind(rbind(n, idx), rbind(idx, n))
  }
  
  n <- dim(pairs)[2]
  p <- progressr::progressor(steps = n)
  
  if (is.null(sample_n)) sample_n <- length(x)
  if (length(d)>sample_n) {
    d <- dplyr::sample_n(seq_along(x), sample_n)
  }
  
  
  results12 <- data.frame(x = names(d)[pairs[1,]],
                          y = names(d)[pairs[2,]])
  #  turn multi-process with furrr + progressr
  results36 <- future_map_dfr(seq(n), ~{
    p()
    x2y(d %>% pull(pairs[1, .x]), d %>% pull(pairs[2, .x]), confidence = confidence, sample_n = sample_n)
    })
 
  results <- bind_cols(results12, results36) %>%
    arrange(desc(x2y), desc(perc_of_obs))
  
  results
}
step_mutual_information <- function(df, mutual_information, ...) {
  df_x2y <- dx2y(df ,...)
  high_mi_df <- df_x2y %>% filter(x2y >= 99) 
  high_mi_cols <- high_mi_df %>% pull(y) %>% unique
  df <- df %>% select(-high_mi_cols) 
}