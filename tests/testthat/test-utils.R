test_that("format_digits", {
  expect_equal(format_digits(0.6, decimal_digits_max = 3), "0.600")
})
