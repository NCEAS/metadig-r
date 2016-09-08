context("set_status")

test_that("a status can be set", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  set_status("SUCCESS")
  expect_is(mdq_result, "list")
  expect_equal(mdq_result[["status"]], "SUCCESS")
})

test_that("setting a status on top of an existing status produces an error", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  set_status("SUCCESS")

  expect_error(set_status("SUCCESS"))
})