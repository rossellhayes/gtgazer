#' Create a stargazer-style regression table using gt
#'
#' Create a regression table in the style of the
#' [stargazer][https://cran.r-project.org/web/packages/stargazer/index.html]
#' package, powered by [gt][gt::gt-package].
#'
#' @param ... One or more model objects
#'
#' @inherit gt::gt return
#' @example man/examples/example-gtgazer.R
#' @export
gtgazer <- function(
	...,
	dependent_variables = NULL,
	independent_variables = everything(),
	summary_statistics = gtgazer_default_summary_statistics(),
	additional_coefficients = NULL,
	additional_summaries = NULL,
	model_names = NULL,
	model_types = NULL,
	include_model_types = NULL,
	include_model_numbers = TRUE,
	decimal_digits_max = 3,
	decimal_mark = ".",
	keep_models = TRUE
) {
	models <- flatten_list(list(...))

	coefficients <- construct_coefficients_df(
		models,
		independent_variables = {{ independent_variables }},
		additional_coefficients = additional_coefficients,
		decimal_digits_max = decimal_digits_max,
		decimal_mark = decimal_mark
	)

	summaries <- construct_summaries_df(
		models,
		summary_statistics = summary_statistics,
		additional_summaries = additional_summaries,
		decimal_digits_max = decimal_digits_max,
		decimal_mark = decimal_mark
	)

	columns <- dplyr::bind_rows(coefficients, summaries) %>%
		prepare_column_names(
			models,
			{{ dependent_variables }},
			model_names,
			model_types,
			include_model_types,
			include_model_numbers
		)

	table <- construct_gtgazer_table(columns)

	if (keep_models) {
		table$models <- models
	}

	table
}

gtgazer_default_summary_statistics <- function() {
	rlang::exprs(
		"Observations" = any_of("nobs"),
		"<i>R</i><sup>2</sup>" = any_of("r.squared"),
		"Adjusted <i>R</i><sup>2</sup>" = any_of("adj.r.squared"),
		"Log likelihood" = any_of("logLik"),
		"Akaike inf. crit." = any_of("AIC"),
		"Residual std. err." = any_of("sigma"),
		"<i>F</i> statistic" = any_of("statistic")
	)
}
