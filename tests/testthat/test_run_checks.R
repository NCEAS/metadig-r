context("check execution tests")

test_that("runCheck() works", {
  checkFile <- system.file("extdata/dataset_title_length-check.xml", package = "metadig")
  metadataFile <- system.file("extdata/example_EML.xml", package = "metadig")
  sysmetaXML <- system.file("extdata/example_sysmeta.xml", package = "metadig")

  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  # The title for the test EML file is too short, so this check should fail.
  expect_match(result$status, "FAILURE")
  expect_match(result$output[[1]]$value, ".*minimum required word count")

  checkFile <- system.file("extdata/entity_attributes_sufficient_check.xml", package = "metadig")
  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  # Some attributes descriptions aren't enough words, so this test should fail.
  expect_match(result$status, "FAILURE")

  checkFile <- system.file("extdata/datatype_check.xml", package = "metadig")
  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  expect_match(result$status, "SUCCESS")

  checkFile <- system.file("extdata/methods_present.xml", package = "metadig")
  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  expect_match(result$status, "SUCCESS")
})

test_that("runCheck() handles sysmetaXML argument correctly", {

  checkFile <- system.file("extdata/dataset_title_length-check.xml", package = "metadig")
  metadataFile <- system.file("extdata/example_EML.xml", package = "metadig")
  # case 1: argument is a file that can be found
  sysmetaXML <- system.file("extdata/example_sysmeta.xml", package = "metadig")
  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  expect_match(result$status, "FAILURE")

  # case2: argument is a string literal of sysmeta
  sysmetaXML <- readChar(sysmetaXML, file.info(sysmetaXML)$size)
  results <- runCheck(checkFile, metadataFile, sysmetaXML)
  result <- results[[1]]
  expect_match(result$status, "FAILURE")

  # case3: argument is a file that doesn't exist
  sysmetaXML <- "notafile.xml"
  expect_warning(runCheck(checkFile, metadataFile, sysmetaXML))

})
