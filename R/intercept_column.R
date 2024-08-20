#' @importFrom vctrs vec_ptype2 vec_cast vec_ptype_abbr
NULL

new_intercept_column <- function(x = character()) {
	vctrs::new_vctr(x, class = "gtgazer_intercept_column")
}

intercept_column <- function(x = character()) {
	x <- vctrs::vec_cast(x, character())
	new_intercept_column(x)
}

#' Selection helper for model intercepts
#'
#' @param x An independent variable in a [gtgazer()] table.
#'
#' @return A [`TRUE`]/[`FALSE`] value.
#' @export
#'
#' @example man/examples/example-intercept_column.R
is_intercept <- function(x) {
	inherits(x, "gtgazer_intercept_column")
}

as_intercept_column <- function(x) {
	vctrs::vec_cast(x, new_intercept_column())
}

#' @export
format.gtgazer_intercept_column <- function(x, ...) {
	out <- format(vctrs::vec_data(x), ...)
	out[is.na(x)] <- NA
	out
}

#' @export
vec_ptype_abbr.gtgazer_intercept_column <- function(x, ...) {
	"intcpt"
}

#' @export
vec_ptype2.gtgazer_intercept_column.gtgazer_intercept_column <- function(x, y, ...) {
	new_intercept_column()
}

#' @export
vec_ptype2.gtgazer_intercept_column.character <- function(x, y, ...) {
	new_intercept_column()
}

#' @export
vec_ptype2.character.gtgazer_intercept_column <- function(x, y, ...) {
	new_intercept_column()
}

#' @export
vec_cast.gtgazer_intercept_column.gtgazer_intercept_column <- function(x, to, ...) {
	x
}

#' @export
vec_cast.gtgazer_intercept_column.character <- function(x, to, ...) {
	intercept_column(x)
}

#' @export
vec_cast.character.gtgazer_intercept_column <- function(x, to, ...) {
	vctrs::vec_data(x)
}
