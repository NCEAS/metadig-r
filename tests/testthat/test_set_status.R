context("set_status()")

test_that("a status can be set", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  set_status("SUCCESS")
  expect_is(mdq_result, "list")
  expect_equal(mdq_result[["status"]], "SUCCESS")
})

test_that("once set to failure, status cannot be set back to success", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  set_status("FAILURE")
  set_status("SUCCESS")

  expect_equal(mdq_result[["status"]], "FAILURE")
})

test_that("once set to success, status can be changed to failure", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  set_status("SUCCESS")
  set_status("FAILURE")

  expect_equal(mdq_result[["status"]], "FAILURE")
})

test_that("status can be set multiple times correctly within a check", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)
  set_status("SUCCESS")
  set_status("FAILURE")
  set_status("SUCCESS")
  expect_equal(mdq_result[["status"]], "FAILURE")

  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)
  set_status("SUCCESS")
  set_status("FAILURE")
  set_status("SUCCESS")
  set_status("FAILURE")
  expect_equal(mdq_result[["status"]], "FAILURE")

  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)
  set_status("FAILURE")
  set_status("SUCCESS")
  set_status("FAILURE")
  expect_equal(mdq_result[["status"]], "FAILURE")
})
