library(broom)

test_lm1 <- lm(tropicalstorm_force_diameter ~ year, data = dplyr::storms)
test_lm2 <- lm(hurricane_force_diameter ~ year, data = dplyr::storms)
test_lm3 <- lm(wind ~ year, data = dplyr::storms)

test_table_1 <- gtgazer(
	test_lm1,
	test_lm2,
	test_lm3,
	dependent_variables = list(
		"Wind" = wind,
		"Diameter" = everything()
	)
)

test_table_2 <- gtgazer(
	test_lm1,
	test_lm2,
	test_lm3,
	dependent_variables = c("Diameter", "Diameter", "Wind")
)

test_that("Apply the same dependent variable name to multiple columns", {
	expect_equal(
		test_table_1$`_boxhead`$var[2:4],
		c("Diameter_(1)", "Diameter_(2)", "Wind_(3)")
	)

	expect_equal(
		test_table_2$`_boxhead`$var[2:4],
		c("Diameter_(1)", "Diameter_(2)", "Wind_(3)")
	)
})

