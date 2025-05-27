construct_coefficients_df <- function(
	models,
	independent_variables,
	additional_coefficients,
	decimal_digits_max,
	decimal_mark
) {
	coefficients_and_intercepts <- models %>%
		purrr::map(
			construct_coefficients_and_intercepts_df,
			independent_variables = independent_variables,
			decimal_digits_max = decimal_digits_max,
			decimal_mark = decimal_mark
		) %>%
		dplyr::bind_rows() %>%
		select_independent_variables({{ independent_variables }}) %>%
		dplyr::mutate(model = dplyr::row_number(), .before = 1) %>%
		dplyr::mutate(!!!additional_coefficients) %>%
		transpose(names_from = "model") %>%
		dplyr::mutate(group = "coefficients", .before = 1)
}

select_independent_variables <- function(data, independent_variables) {
	variables_expr <- rlang::quo_get_expr(rlang::enexpr(independent_variables))

	if (isTRUE(try(variables_expr[[1]] == "list", silent = TRUE))) {
		variables_expr[[1]] <- rlang::expr(c)
	}

	# tidyselect on as.list(data) because this doesn't enforce unique names
	tidy_selection <- tidyselect::eval_select(variables_expr, as.list(data))

	data <- data[tidy_selection]
	names(data) <- names(tidy_selection)

	name_counts <- table(names(data))
	duplicated_names <- names(name_counts)[name_counts > 1]

	for (duplicated_name in duplicated_names) {
		duplicated_columns <- which(names(data) == duplicated_name)

		rowwise_filled_columns <- data[, duplicated_columns] %>%
			rlang::set_names(duplicated_columns) %>%
			dplyr::mutate(dplyr::across(everything(), Negate(is.na))) %>%
			rowSums()

		can_be_collapsed <- max(rowwise_filled_columns) == 1

		if (can_be_collapsed) {
			data[duplicated_columns[[1]]] <- purrr::reduce(
				data[duplicated_columns],
				dplyr::coalesce
			)

			data[duplicated_columns[-1]] <- NULL
		}
	}

	data
}

construct_coefficients_and_intercepts_df <- function(
	model,
	independent_variables,
	decimal_digits_max,
	decimal_mark
) {
	tidy_df <- broom::tidy(model)

	is_intercept <- if ("coef.type" %in% names(tidy_df)) {
		tidy_df$coef.type == "intercept"
	} else {
		tidy_df$term == "(Intercept)"
	}

	dplyr::mutate(
		tidy_df,
		stars = dplyr::case_when(
			p.value < 0.001 ~ "***",
			p.value < 0.01 ~ "**",
			p.value < 0.05 ~ "*",
			TRUE ~ ""
		),
		dplyr::across(
			tidyselect::where(is.numeric),
			function(x) {
				format_digits(
					x,
					decimal_digits_max = decimal_digits_max,
					decimal_mark = decimal_mark,
					maybe_integer = FALSE
				)
			}
		),
		value = paste0(estimate, stars, "<br>(", std.error, ")")
	) %>%
		dplyr::select(term, value) %>%
		dplyr::mutate(term = as.character(term)) %>%
		tidyr::pivot_wider(names_from = "term", values_from = "value") %>%
		dplyr::mutate(dplyr::across(which(is_intercept), as_intercept_column))
}

transpose <- function(df, names_from, ...) {
	if (ncol(df) == 1) {
		out <- rep(list(character(0)), nrow(df))
		names(out) <- df$model
		return(dplyr::as_tibble(out))
	}

	df %>%
		tidyr::pivot_longer(
			cols = -tidyselect::all_of(c(names_from, ...)),
			values_to = "value"
		) %>%
		tidyr::pivot_wider(
			names_from = names_from,
			values_from = "value",
			names_repair = "minimal"
		)
}
