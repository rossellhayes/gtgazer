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

test_that("dependent_variables from variable", {
	dependent_variable_exprs <- rlang::exprs(
		"Miles per gallon" = any_of("mpg"),
		"Horsepower" = any_of("hp")
	)

	expect_equal(
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(hp ~ wt, data = mtcars),
			dependent_variables = dependent_variable_exprs
		),
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(hp ~ wt, data = mtcars),
			dependent_variables = c(
				"Miles per gallon" = any_of("mpg"),
				"Horsepower" = any_of("hp")
			)
		)
	)

	dependent_variable_expr <- rlang::expr(
		c(
			"Miles per gallon" = any_of("mpg"),
			"Horsepower" = any_of("hp")
		)
	)

	expect_equal(
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(hp ~ wt, data = mtcars),
			dependent_variables = dependent_variable_expr
		),
		gtgazer(
			lm(mpg ~ wt, data = mtcars),
			lm(hp ~ wt, data = mtcars),
			dependent_variables = c(
				"Miles per gallon" = any_of("mpg"),
				"Horsepower" = any_of("hp")
			)
		)
	)
})
