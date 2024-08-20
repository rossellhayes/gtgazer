label_model_types <- function(models, model_types, call) {
	label_rename(
		purrr::map_chr(models, label_model_type),
		model_types,
		models,
		arg = "model_types",
		call = call
	)
}

label_model_type <- function(model) {
	UseMethod("label_model_type")
}

#' @export
label_model_type.default <- function(model) {
	class(model)[[1]]
}

#' @export
label_model_type.lm <- function(model) {
	"OLS"
}

#' @export
label_model_type.lm_robust <- function(model) {
	"OLS"
}

#' @export
label_model_type.glm <- function(model) {
	model$family$link %||% model$family %||% "GLM"
}

#' @export
label_model_type.clm <- function(model) {
	if (is.null(model$link)) {
		return("CLM")
	}

	paste("ordered", model$link)
}
