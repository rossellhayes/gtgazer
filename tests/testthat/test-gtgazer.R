test_that("allow trailing comma", {
  expect_silent(
  	gtgazer(lm(1 ~ 1), )
  )
})
