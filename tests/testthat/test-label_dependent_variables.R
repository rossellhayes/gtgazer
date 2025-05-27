test_that("can give same name to multiple columns", {
  models <- list(
  	lm(mpg ~ wt, data = mtcars),
  	lm(mpg ~ disp, data = mtcars)
  )

	expect_equal(
		label_dependent_variables(
			models,
			c("Miles per gallon" = mpg)
		),
		c("Miles per gallon", "Miles per gallon")
	)

	expect_equal(
		label_dependent_variables(
			models,
			c("Miles per gallon" = "mpg")
		),
		c("Miles per gallon", "Miles per gallon")
	)

	expect_equal(
		label_dependent_variables(
			models,
			c("Miles per gallon" = any_of("mpg"))
		),
		c("Miles per gallon", "Miles per gallon")
	)
})
