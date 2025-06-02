test_that("allow trailing comma", {
  expect_silent(
  	gtgazer(lm(1 ~ 1), )
  )
})

test_that("independent_variables from variable", {
	independent_variable_exprs <- rlang::exprs(
		"Weight" = any_of("wt"),
		"Horsepower" = any_of("hp")
	)

	expect_equal(
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(mpg ~ hp, data = mtcars),
			independent_variables = independent_variable_exprs
		),
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(mpg ~ hp, data = mtcars),
			independent_variables = c(
				"Weight" = any_of("wt"),
				"Horsepower" = any_of("hp")
			)
		)
	)

	independent_variable_expr <- rlang::expr(
		c(
			"Weight" = any_of("wt"),
			"Horsepower" = any_of("hp")
		)
	)

	expect_equal(
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(mpg ~ hp, data = mtcars),
			independent_variables = independent_variable_expr
		),
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(mpg ~ hp, data = mtcars),
			independent_variables = c(
				"Weight" = any_of("wt"),
				"Horsepower" = any_of("hp")
			)
		)
	)
})
