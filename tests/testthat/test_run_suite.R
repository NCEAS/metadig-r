context("check quality suite tests")

test_that("runSuite() works", {
  metadataFile <- system.file("extdata/example_EML.xml", package = "metadig")
  suiteXML <- system.file("extdata/example_suite.xml", package = "metadig")
  dirXML <- system.file("extdata", package = "metadig")
  sysmetaXML <- system.file("extdata/example_sysmeta.xml", package = "metadig")

  expect_error(runSuite(7, 7, 7, 7))
  expect_error(runSuite(suiteXML, 7, 7, 7))
  expect_error(runSuite(suiteXML, dirXML, 7, 7))
  expect_error(runSuite(c(suiteXML, dirXML), dirXML, metadataFile))
  expect_error(runSuite(suiteXML, c(suiteXML, dirXML), metadataFile))
  expect_error(runSuite(suiteXML, dirXML, c(suiteXML, dirXML)))

  results <- runSuite(suiteXML, dirXML, metadataFile, sysmetaXML)

  expect_match(results[[1]]$value$status, "FAILURE")
  expect_match(results[[2]]$value$status, "SUCCESS")
  expect_match(results[[3]]$value$status, "FAILURE")
  expect_match(results[[4]]$value$status, "SUCCESS")
})
