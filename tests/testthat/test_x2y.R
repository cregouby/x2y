library(testthat)
test_that("x2y works high level of missing values",{
  x <- rep(NA, 1000)
  x[34] <- 71
  y <- rep(NA, 1000)
  y[107] <- 1
  x2y_mi <- x2y(x, y)
  expect_equal(x2y_mi$perc_of_obs, 0)
  expect_equal(x2y_mi$x2y, 0)
})

test_that("x2y works with pure POSIXct",{
  x <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now())
  y <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now())
  x2y_mi <- x2y(x, y)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gte(x2y_mi$x2y, 0)
})

test_that("x2y works with dates",{
  x <- (rpois(200, 200)*1e4) %>% as.POSIXct(origin = lubridate::now()) %>% lubridate::date()
  y <- (rpois(200, 200)*1e4) %>% as.POSIXct(origin = lubridate::now()) %>% lubridate::date()
  x2y_mi <- x2y(x, y)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gte(x2y_mi$x2y, 0)
})

test_that("x2y works with difftimes",{
  x <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now())
  y <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now())
  x2 <-  difftime(x,y)
  y2 <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now()) %>% difftime(x)
  x2y_mi <- x2y(x2, y2)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gte(x2y_mi$x2y, 0)
})

test_that("x2y works with mixed POSIXct and character",{
  x <- rpois(200, 200) %>% as.POSIXct(origin = lubridate::now())
  y <- sample(LETTERS, 200, replace = TRUE)
  x2y_mi <- x2y(x, y)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gt(x2y_mi$x2y, 0)
  x2y_mi <- x2y(y, x)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gt(x2y_mi$x2y, 0)
})

test_that("x2y works with list columns",{
  data(starwars, package="dplyr")
  x <- starwars[1:20,"films"]
  y <- sample(LETTERS, 20, replace = TRUE)
  x2y_mi <- x2y(x, y)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gt(x2y_mi$x2y, 0)
  x2y_mi <- x2y(y, x)
  expect_equal(x2y_mi$perc_of_obs, 100)
  expect_gt(x2y_mi$x2y, 0)
})

test_that("sample_n defaults allows human acceptable compute time",{
  skip_on_cran()
  x <- rpois(1e4, 200)
  y <- rpois(1e4, 200)

  tictoc::tic()
  x2y_mi <- x2y(x, y)
  time <- tictoc::toc()
  duration1 <- time$toc - time$tic

  x <- rpois(1e7, 200)
  y <- rpois(1e7, 200)

  tictoc::tic()
  x2y_mi <- x2y(x, y)
  time <- tictoc::toc()
  duration2 <- time$toc - time$tic

  expect_lte(abs(duration1 - duration2), 1)
})

test_that("sample_n works",{
  skip_on_cran()
  x <- rpois(1e4, 200)
  y <- rpois(1e4, 200)

  tictoc::tic()
  x2y_mi <- x2y(x, y, sample_n=NULL)
  time <- tictoc::toc()
  duration1 <- time$toc - time$tic

  x <- rpois(1e6, 200)
  y <- rpois(1e6, 200)

  tictoc::tic()
  x2y_mi <- x2y(x, y, sample_n=NULL)
  time <- tictoc::toc()
  duration2 <- time$toc - time$tic

  expect_lte(duration1, 0.1)
  expect_gte(abs(duration1 - duration2), 1.5)
})


