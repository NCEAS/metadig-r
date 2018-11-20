context("save_output()")

test_that("a text output can be saved", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  save_output("test")

  expect_is(mdq_result, "list")
  expect_length(mdq_result[["output"]], 1)
  expect_equal(mdq_result[["output"]][[1]][["value"]], "test")
})

test_that("an output type that isn't supported cannot be saved", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  expect_error(save_output("test", "test"))
})

test_that("an output with the text 'NULL' is saved if a NULL value is saved", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  save_output(NULL)

  expect_is(mdq_result, "list")
  expect_length(mdq_result[["output"]], 1)
  expect_equal(mdq_result[["output"]][[1]][["value"]], "NULL")
})

test_that("an output is newline-separated when it is multi-valued", {
  if ("mdq_result" %in% ls(envir = .GlobalEnv)) rm(mdq_result, envir = .GlobalEnv)

  save_output(c("hi", "test"))
  expect_match(mdq_result[["output"]][[1]]$value, "\\n")
})
