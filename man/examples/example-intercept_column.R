# Move intercepts to the top of a table
gtgazer(
	lm(mpg ~ wt + hp, data = mtcars),
	ordinal::clm(as.ordered(cyl) ~ hp + vs, data = mtcars),
	independent_variables = c(
		where(is_intercept),
		everything()
	)
)
