construct_summaries_df <- function(
	models,
	summary_statistics,
	additional_summaries,
	decimal_digits_max,
	decimal_mark
) {
	glance_df <- models %>%
		purrr::map(
			function(model) {
				broom::glance(model) %>%
					purrr::map(unclass) %>%
					dplyr::bind_cols()
			}
		) %>%
		dplyr::bind_rows()

	if (
		!exists("df", glance_df) &&
		exists("df.residual", glance_df) &&
		exists("nobs", glance_df)
	) {
		glance_df <- glance_df %>%
			mutate(df = .data$nobs - .data$df.residual)
	}

	summaries_df <- glance_df %>%
		dplyr::mutate(
			dplyr::across(
				tidyselect::where(is.numeric),
				function(x) {
					format_digits(
						x,
						decimal_digits_max = decimal_digits_max,
						decimal_mark = decimal_mark,
						maybe_integer = TRUE
					)
				}
			),
			dplyr::across(
				tidyselect::any_of("sigma"),
				function(sigma) {
					dplyr::if_else(
						is.na(sigma),
						NA,
						paste0(sigma, "<br>(*df* = ", df.residual, ")")
					)
				}
			),
			dplyr::across(
				tidyselect::any_of("statistic"),
				function(statistic) {
					dplyr::if_else(
						is.na(statistic),
						NA,
						paste0(
							statistic,
							dplyr::case_when(
								p.value < 0.001 ~ "***",
								p.value < 0.01 ~ "**",
								p.value < 0.05 ~ "*",
								TRUE ~ ""
							),
							"<br>",
							"(*df* = ", df, "; ", df.residual, ")"
						)
					)
				}
			)
		) %>%
		dplyr::bind_cols(additional_summaries) %>%
		dplyr::select(!!!summary_statistics)

	if (!all(names(additional_summaries) %in% names(summaries_df))) {
		summaries_df <- bind_cols(
			summaries_df,
			additional_summaries
		)
	}

	summaries_df %>%
		dplyr::mutate(model = dplyr::row_number()) %>%
		transpose(names_from = "model") %>%
		dplyr::mutate(group = "summaries", .before = 1)
}
