context("check execution tests")

test_that("runCheck() works", {
  #skip_on_cran()
  library(metadig)

  checkFile <- system.file("extdata/dataset_title_length-check.xml", package="metadig")
  metadataFile <- system.file("extdata/example_eml.xml", package="metadig")
  results <- runCheck(checkFile, metadataFile)
  result <- results[[1]]
  # The title for the test EML file is too short, so this check should fail.
  expect_match(result$status, "FAILURE")
  expect_match(result$output[[1]]$value, ".*minimum required word count")

  checkFile <- system.file("extdata/entity_attributes_sufficient_check.xml", package="metadig")
  results <- runCheck(checkFile, metadataFile)
  result <- results[[1]]
  # Some attributes descriptions aren't enough words, so this test should fail.
  expect_match(result$status, "FAILURE")

  checkFile <- system.file("extdata/datatype_check.xml", package="metadig")
  results <- runCheck(checkFile, metadataFile)
  result <- results[[1]]
  expect_match(result$status, "SUCCESS")

  checkFile <- system.file("extdata/methods_present.xml", package="metadig")
  results <- runCheck(checkFile, metadataFile)
  result <- results[[1]]
  expect_match(result$status, "SUCCESS")

})
