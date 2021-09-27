test_that("dx2y works with small dataframe", {
  dx2y_iris <- dx2y(iris)
  expect_equal(dim(dx2y_iris), c(20,4) )
  expect_equal(class(dx2y_iris), "data.frame" )
})

test_that("dx2y works with an outcome col", {
  dx2y_iris <- dx2y(iris, target="Species")
  expect_equal(dim(dx2y_iris), c(20,4) )
  expect_equal(class(dx2y_iris), "data.frame" )
})

test_that("dx2y works with progress bar", {
  dx2y_iris <- dx2y(iris, target="Species")
  expect_equal(dim(dx2y_iris), c(20,4) )
  expect_equal(class(dx2y_iris), "data.frame" )
})

test_that("dx2y works with large nrow dataframe", {
  data(diamonds, package = "ggplot2")
  dx2y_diamonds <- dx2y(diamonds)
  expect_equal(dim(dx2y_diamonds), c(90,4) )
})

test_that("dx2y works with large ncol dataframe", {
  data(openfoodfacts, package = "x2y")
  dx2y_offcts <- dx2y(openfoodfacts %>% dplyr::sample_n(100))
  expect_equal(dim(dx2y_offcts), c(3306,4) )
})
