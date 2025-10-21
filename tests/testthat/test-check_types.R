# TODO: add regexp to expect_error

test_that("good inputs to check_numeric are accepted", {
  expect_true(check_numeric(0))
  expect_true(check_numeric(0L))
  expect_true(check_numeric(0, lower = 0))
  expect_true(check_numeric(0, upper = 0))
  expect_true(check_numeric(NA_real_, allow_missing = TRUE))
  expect_true(check_numeric(Inf))
  expect_true(check_numeric(-Inf))

})

test_that("bad inputs to check_numeric trigger errors", {
  # Errors in the thing being checked:
  expect_error(check_numeric("foo"))
  expect_error(check_numeric(1:2))
  expect_error(check_numeric(NA_real_))
  expect_error(check_numeric(NA, allow_missing = TRUE))
  expect_error(check_numeric(0, lower = 1))
  expect_error(check_numeric(0, lower = 0, lower_inclusive = FALSE))
  # Errors in how the function is called:
  msg <- "check_numeric function called incorrectly:"
  expect_error(check_numeric(1, lower = "foo"), regexp = )
  expect_error(check_numeric(1, upper = "foo"), regexp = msg)
  expect_error(check_numeric(1, lower_inclusive = "foo"), regexp = msg)
  expect_error(check_numeric(1, upper_inclusive = "foo"), regexp = msg)
  expect_error(check_numeric(1, lower = c(1, 1)), regexp = msg)
  expect_error(check_numeric(1, upper = c(1, 1)), regexp = msg)
  expect_error(check_numeric(1, lower_inclusive = c(TRUE, TRUE)), regexp = msg)
  expect_error(check_numeric(1, upper_inclusive = c(TRUE, TRUE)), regexp = msg)
  expect_error(check_numeric(1, lower = NA_real_), regexp = msg)
  expect_error(check_numeric(1, upper = NA_real_), regexp = msg)
  expect_error(check_numeric(1, lower_inclusive = NA), regexp = msg)
  expect_error(check_numeric(1, upper_inclusive = NA), regexp = msg)
  expect_error(check_numeric(1, lower = 1, upper = 0), regexp = msg)
})

test_that("good inputs to check_logical are accepted", {
  expect_true(check_logical(TRUE))
  expect_true(check_logical(FALSE))
  expect_true(check_logical(NA, allow_missing = TRUE))
})

test_that("bad inputs to check_logical trigger errors", {
  # Errors in the thing being checked:
  expect_error(check_logical("foo"))
  expect_error(check_logical(c(TRUE, TRUE)))
  expect_error(check_logical(NA))
  expect_error(check_logical(NA_real_, allow_missing = TRUE))
  # Errors in how the function is called:
  msg <- "check_logical function called incorrectly:"
  expect_error(check_logical(TRUE, allow_missing = "foo"), regexp = msg)
  expect_error(check_logical(TRUE, allow_missing = c(TRUE, TRUE)), regexp = msg)
  expect_error(check_logical(TRUE, allow_missing = NA), regexp = msg)
  expect_error(check_logical(TRUE, allow_missing = NA_real_), regexp = msg)
})
