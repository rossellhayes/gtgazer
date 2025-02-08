assert_recyclable <- function(x, y, arg, call) {
	if (length(x) == 1 || length(x) == length(y)) {
		return(invisible())
	}

	cli::cli_abort(
		c(
			"x" = "{.arg {arg}} must have a length equal to the number of models or 1.",
			"i" = "{.arg {arg}} has a length of {.val {length(x)}}.",
			"i" = "The number of models is {.val {length(y)}}."
		),
		call = call
	)
}


flatten_list <- function(x) {
	is_list <- purrr::map_lgl(x, vctrs::obj_is_list)

	if (!any(is_list)) {
		return(x)
	}

	x[!is_list] <- purrr::map(x[!is_list], list)

	x <- vctrs::list_unchop(
		x,
		ptype = list(),
		name_spec = function(outer, inner) inner
	)

	flatten_list(x)
}

format_digits <- function(
	x,
	decimal_digits_max = 3,
	decimal_mark = ".",
	maybe_integer = TRUE
) {
	out <- character(length(x))

	is_na <- is.na(x)
	out[is_na] <- NA_character_
	x <- x[!is_na]

	negative <- x < 0
	x <- abs(x)

	is_integer <- if (decimal_digits_max <= 0) {
		TRUE
	} else if (!maybe_integer) {
		FALSE
	} else {
		x %% 1 == 0 | x >= 10 ^ (decimal_digits_max)
	}

	if (any(is_integer)) {
		out[!is_na][is_integer] <- formatC(
			x[is_integer],
			digits = 0,
			width = 1,
			format = "f",
			big.mark = ","
		)
	}

	if (any(!is_integer)) {
		integer_parts <- x[!is_integer] %/% 1
		decimal_parts <- substr(
			formatC(x[!is_integer] %% 1, digits = decimal_digits_max, format = "f"),
			start = 3,
			stop = 3 + decimal_digits_max - nchar(integer_parts)
		)

		out[!is_na][!is_integer] <- paste0(
			formatC(integer_parts, format = "fg", big.mark = ","),
			strrep(decimal_mark, nzchar(decimal_parts)),
			decimal_parts
		)
	}

	out[!is_na] <- paste0(strrep("\u2212", negative), out[!is_na])
	out
}

starviewer <- function(...) {
	# make sure stargazer is available
	rlang::check_installed("stargazer")
	rlang::check_installed("rstudioapi")

	# if not latex, run stargazer in text / html (or both)
	dir <- tempfile()
	dir.create(dir)
	html_file <- file.path(dir, "tempfile.html")
	capture.output(stargazer::stargazer(..., type = "html", out = html_file))
	rstudioapi::viewer(html_file)
}
