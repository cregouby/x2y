# thanks to https://github.com/rama100/x2y/blob/main/x2y.R

# TODO add a col selector function
#' filter columns of a dx2y dataframe result based on a minimal mutual-information
#'
#' @param dx2y_df dataframe result of a dx2y function
#' @param threshold minimum mutual-information value to keep a column
#' @param keep method to choose the column to keeep. only "first" is implemented
#'
#' @return a vector of column names from the original dataframe
#' @importFrom rlang abort
#' @export
#'
#' @examples
#'
get_high_mi_cols <- function(dx2y_df, threshold=99, keep="first") {
  if (keep!="first") {
    abort("'longest' method is not implemented yet")
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

  # naive cast of list vectors
  if (mode(x)=="list") x <- x %>% as.character %>% as.factor
  if (mode(y)=="list") y <- x %>% as.character %>% as.factor

  # if y is continuous
  if (is.numeric(y) && !is.factor(y)) {
    preds <- predict(rpart(y ~ x, method = "anova"), type = 'vector')
    return(calc_mae_reduction(preds, y))
  }
  # if y is categorical
  else {
    preds <- predict(rpart(y ~ x, method = "class"), type = 'class')
    return(calc_misclass_reduction(preds, y))
  }
}


simple_boot <- function(x,y) {
  ids <- sample(length(x), replace = TRUE)
  x2y_inner(x[ids], y[ids])
}

#' Compute mutual information between two vectors
#'
#' @param x first vector
#' @param y second vector
#' @param confidence boolean, shall we compute 95% confidence interval (default FALSE)
#' @param sample_n max number of sample to consider (default 5000 )
#'
#' @return a list with `perc_of_obs` percentage of observations present in the two vectors,
#'   `x2y` mutual information value and eventually `CI_95_Lower` and `CI_95_Upper` values.
#' @importFrom rpart rpart
#' @export
#'
#' @examples
#' x2y(iris$Sepal.Length, iris$Species)
#' #>$perc_of_obs
#' #>[1] 100
#' #>
#' #>$x2y
#' #>[1] 62
#'
#' x2y(iris$Sepal.Length, iris$Species, confidence = TRUE)
#' #>$perc_of_obs
#' #>[1] 100
#' #>
#' #>$x2y
#' #>[1] 62
#' #>
#' #>$CI_95_Lower
#' #>97.5%
#' #>51.08
#' #>
#' #>$CI_95_Upper
#' #>2.5%
#' #>70.15
#'
x2y <- function(x, y, confidence = FALSE, sample_n = 5000) {
  results <- list()

  missing <-  is.na(x) | is.na(y)
  results$perc_of_obs <- round(100 * (1 - sum(missing) / length(x)), 2)
  if (results$perc_of_obs==0) {
    results$x2y <- 0
    return(results)
  }

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

#' Compute the mutual information between dataframe columns
#'
#' @param df dataframe to compare columns
#' @param target (optional) the outcome column if any
#' @param confidence Boolean, shall we compute the 95% Confidence interval (default FALSE)
#' @param sample_n max number of sample to consider (default 5000 )
#'
#' @return a dataframe with columns `x` and `y` filled with all combinations of column pairs
#'  (when `target` is NA), or with all combinations of column with target (if `target` is given)
#'  with a `perc_of_obs` column for the percentage of observations present,
#'  and  `x2y` value of the mutual information between the two columns.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull arrange bind_cols desc
#' @importFrom purrr map_dfr
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom stats predict quantile
#' @export
#'
#' @examples
#' pairwise_iris <- dx2y(iris)
#' head(pairwise_iris)
#' #>              x            y perc_of_obs   x2y
#' #>1  Petal.Length      Species         100 89.94
#' #>2   Petal.Width      Species         100 88.97
#' #>3   Petal.Width Petal.Length         100 80.73
#' #>4       Species Petal.Length         100 79.72
#' #>5  Petal.Length  Petal.Width         100 77.32
#'
#' iris_species <- dx2y(iris, target="Species")
#' iris_species
#' #>x            y perc_of_obs   x2y
#' #>1 Petal.Length      Species         100 89.94
#' #>2  Petal.Width      Species         100 88.97
#' #>3      Species Petal.Length         100 79.72
#' #>4      Species  Petal.Width         100 76.31
#' #>5 Sepal.Length      Species         100 47.09
#' #>6      Species Sepal.Length         100 42.08
#' #>7      Species  Sepal.Width         100 22.37
#' #>8  Sepal.Width      Species         100 15.87

dx2y <- function(df, target = NA, confidence = FALSE, sample_n=5000) {
  if (is.na(target)) {
    pairs <- combn(ncol(df), 2)
    pairs <- cbind(pairs, pairs[2:1, ])
  }
  else {
    n <- 1:ncol(df)
    idx <- which(target == names(df))
    n <- n[n != idx]
    pairs <- cbind(rbind(n, idx), rbind(idx, n))
  }

  n <- dim(pairs)[2]
  p <- progressr::progressor(steps = n)

  if (is.null(sample_n)) sample_n <- nrow(df)
  if (nrow(df)>sample_n) {
    df <- dplyr::sample_n(seq_along(x), sample_n)
  }


  results12 <- data.frame(x = names(df)[pairs[1,]],
                          y = names(df)[pairs[2,]])
  if (n>30) {
    #  turn into multi-process with furrr + progressr
    furrr_options(seed=42)
    # TODO manage system specific cases
    plan(multisession, workers = 8)
    progressr::with_progress({
      results36 <- future_map_dfr(seq(n), ~ {
        p()
        x2y(
          df %>% pull(pairs[1, .x]),
          df %>% pull(pairs[2, .x]),
          confidence = confidence,
          sample_n = sample_n
        )
      })
    })

  } else {
    progressr::with_progress({
      results36 <- purrr::map_dfr(seq(n), ~ {
        p()
        x2y(
          df %>% pull(pairs[1, .x]),
          df %>% pull(pairs[2, .x]),
          confidence = confidence,
          sample_n = sample_n
        )
      })
    })

  }
  results <- bind_cols(results12, results36) %>%
    arrange(desc(x2y), desc(perc_of_obs))

  results
}
#' reciepe step for removing low mutual information columns form a dataframe
#'
#' @param df dataframe
#' @param mutual_information mutual information
#' @param ... other parameters passed onto dx2y function
#'
#' @return
#' @export
#'
#' @examples
step_mutual_information <- function(df, mutual_information=80, ...) {
  df_x2y <- dx2y(df ,...)
  high_mi_df <- df_x2y %>% filter(x2y >= mutual_information)
  high_mi_cols <- high_mi_df %>% pull(y) %>% unique
  df <- df %>% dplyr::select(-high_mi_cols)
}
