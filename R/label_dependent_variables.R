label_dependent_variables <- function(models, dependent_variables, call) {
	label_rename(
		purrr::map_chr(models, label_dependent_variable),
		{{ dependent_variables }},
		models,
		arg = "dependent_variables",
		call = call
	)
}

label_dependent_variable <- function(model, ...) {
	UseMethod("label_dependent_variable")
}

#' @export
label_dependent_variable.default <- function(model, ...) {
	as.character(attr(model$terms, "variables"))[[2]]
}

#' @export
label_dependent_variable.felm <- function(model, ...) {
	model$lhs
}
