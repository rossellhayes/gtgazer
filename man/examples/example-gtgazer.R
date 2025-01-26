example_lm <- lm(mpg ~ wt + hp, data = mtcars)
example_glm <- glm(vs ~ qsec + cyl + disp, data = mtcars, family = binomial)
example_clm <- ordinal::clm(as.ordered(cyl) ~ hp + vs, data = mtcars)

gtgazer(example_lm, example_glm)

gtgazer(
	example_lm, example_glm,
	dependent_variables = c(
		"MPG" = "mpg",
		"Straight" = contains("vs")
	),
	independent_variables = c(
		"Power" = any_of(c("hp", "qsec")),
		"Cylinders" = "cyl",
		"Size" = any_of(c("wt", "disp")),
		everything()
	)
)

gtgazer(
	example_lm, example_glm,
	dependent_variables = c(
		"MPG" = "mpg",
		"Straight" = contains("vs")
	),
	independent_variables = c(
		"Power" = any_of(c("hp", "qsec")),
		"Cylinders" = "cyl",
		"Size" = any_of(c("wt", "disp")),
		everything()
	),
	additional_summaries = list(
		"Test" = c("Test", "Test")
	)
)

gtgazer(
	example_lm, example_glm, example_clm,
	independent_variables = c(
		"Weight" = "wt",
		where(is_intercept),
		everything()
	),
	additional_coefficients = list(
		"Education fixed effects" = cli::symbol$tick,
		"Party ID fixed effects" = cli::symbol$tick,
		"Survey week fixed effects" = cli::symbol$tick
	)
)

dependent_variables <- rlang::expr(
	c(
		"MPG" = "mpg",
		"Straight" = contains("vs")
	)
)

indepdendent_variables <- rlang::expr(
	c(
		"Power" = any_of(c("hp", "qsec")),
		"Cylinders" = "cyl",
		"Size" = any_of(c("wt", "disp")),
		everything()
	)
)

gtgazer(
	example_lm, example_glm,
	dependent_variables = {{ dependent_variables }},
	independent_variables = {{ indepdendent_variables }}
)
