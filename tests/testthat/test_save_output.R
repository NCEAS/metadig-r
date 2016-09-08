context("save_output")

test_that("a text output can be saved", {
  save_output("test")

  expect_is(mdq_result, "list")
  expect_length(mdq_result[["output"]], 1) # TODO: FIX THIS
  expect_equal(mdq_result[["output"]][[1]][["value"]], "test") # TODO: FIX THIS
})

test_that("an output type that isn't supported cannot be saved", {
  expect_error(save_output("test", "test"))
})