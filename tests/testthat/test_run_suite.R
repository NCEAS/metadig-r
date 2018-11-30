context("check quality suite tests")

test_that("runSuite() works", {
  metadataXML <- system.file("extdata/example_EML.xml", package = "metadig")
  suiteXML <- system.file("extdata/example_suite.xml", package = "metadig")
  dirXML <- system.file("extdata", package = "metadig")

  expect_error(runSuite(7, 7, 7))
  expect_error(runSuite(suiteXML, 7, 7))
  expect_error(runSuite(suiteXML, dirXML, 7))
  expect_error(runSuite(c(suiteXML, dirXML), dirXML, metadataXML))
  expect_error(runSuite(suiteXML, c(suiteXML, dirXML), metadataXML))
  expect_error(runSuite(suiteXML, dirXML, c(suiteXML, dirXML)))

  results <- runSuite(suiteXML, dirXML, metadataXML)

  expect_match(results[[1]]$value$status, "FAILURE")
  expect_match(results[[2]]$value$status, "SUCCESS")
  expect_match(results[[3]]$value$status, "FAILURE")
  expect_match(results[[4]]$value$status, "SUCCESS")
})
