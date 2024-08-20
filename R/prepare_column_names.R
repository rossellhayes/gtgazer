prepare_column_names <- function(
	columns,
	models,
	dependent_variables,
	model_names,
	model_types,
	include_model_types,
	include_model_numbers
) {
	dependent_variables <- label_dependent_variables(
		models,
		dependent_variables,
		call = rlang::caller_env()
	)

	include_model_names <- !is.null(model_names)
	if (include_model_names) {
		assert_recyclable(
			model_names,
			models,
			arg = "model_names",
			call = rlang::caller_env()
		)
	}

	model_types <- label_model_types(
		models,
		model_types,
		call = rlang::caller_env()
	)
	include_model_types <- include_model_types %||%
		length(unique(model_types)) > 1

	delim <- determine_delim(dependent_variables, model_types)

	names(columns) <- c(
		"group",
		"term",
		do.call(
			paste,
			purrr::compact(
				list(
					dependent_variables,
					if (include_model_names) model_names,
					if (include_model_types) italicize(model_types),
					if (include_model_numbers) parenthesize(seq_along(models)),
					sep = delim
				)
			)
		)
	)

	attr(columns, "delim") <- delim

	columns
}

determine_delim <- function(...) {
	labels <- c(...)
	present_characters <- stringr::str_split_fixed(labels, "", n = Inf)

	punctuation <- c(
		"_", ".", "-", "~", "+", ",", "=", ":", ";", "/", "*", "|",
		"!", "#", "$", "%", "&",
		"(", ")", "<", "=", ">", "?", "[", "]", "^", "{",  "}"
	)
	delim <- punctuation[!punctuation %in% present_characters][[1]]

	if (is.null(delim)) {
		# TODO
	}

	delim
}

italicize <- function(x) paste0("*", x, "*")
parenthesize <- function(x) paste0("(", x, ")")
