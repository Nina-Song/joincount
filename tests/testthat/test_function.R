context("Test that function works.")

test_that("function works", {
  x <- "/Users/ninasong/Desktop/Craig_lab/GeoSpatial/breast_cancer"
  expect_warning(spatialDataPrep(x),
                 "'giveCsparse' has been deprecated")
})


