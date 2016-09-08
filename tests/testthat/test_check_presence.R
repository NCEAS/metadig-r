context("check_presence")

test_that("throws an error if checking a non-list", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  expect_error(check_presence(""))
})

test_that("status is set to FAILURE if any expectation is FAILURE", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  # F
  check_presence(list())
  expect_true(mdq_result[["status"]] == "FAILURE")

  # FS
  rm(list = ls(all=TRUE))
  check_presence(list())
  check_presence(list(""))
  expect_true(mdq_result[["status"]] == "FAILURE")

  # SF
  rm(list = ls(all=TRUE))
  check_presence(list(""))
  check_presence(list())
  expect_true(mdq_result[["status"]] == "FAILURE")

  # FSF
  rm(list = ls(all=TRUE))
  check_presence(list())
  check_presence(list(""))
  check_presence(list())
  expect_true(mdq_result[["status"]] == "FAILURE")

  # SFS
  check_presence(list(""))
  check_presence(list())
  check_presence(list(""))
  expect_true(mdq_result[["status"]] == "FAILURE")
})