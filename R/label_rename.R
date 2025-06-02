label_rename <- function(
	automatic_labels,
	manual_labels,
	models,
	arg,
	call
) {
	label_exprs <- rlang::quo_get_expr(rlang::enquo(manual_labels))

	if (isTRUE(try(is.name(label_exprs), silent = TRUE))) {
		label_exprs <- get0(label_exprs)
	}

	label_exprs <- as.list(label_exprs)
	label_exprs <- label_exprs[!purrr::map_lgl(label_exprs, is.name)]

	manual_labels <- purrr::map(
		seq_along(label_exprs),
		\(i) {
			this_manual_label <- as.call(c(rlang::expr(c), label_exprs[i]))

			these_manual_labels <- purrr::imap(
				automatic_labels,
				function(this_automatic_label, index) {
					result <- try(
						tidyselect::eval_rename(
							this_manual_label,
							rlang::set_names(this_automatic_label)
						),
						silent = TRUE
					)

					result[result == 1] <- index

					result
				}
			)

			if (
				purrr::every(these_manual_labels, function(x) inherits(x, "try-error"))
			) {
				return(rlang::eval_bare(this_manual_label))
			}

			Filter(function(x) !inherits(x, "try-error"), these_manual_labels)
		}
	)

	manual_labels <- unlist(manual_labels)

	if (!is.null(manual_labels) && is.null(names(manual_labels))) {
		assert_recyclable(
			manual_labels,
			models,
			arg = arg,
			call = call
		)

		return(manual_labels)
	}

	labels <- automatic_labels

	if (is.null(manual_labels)) {
		return(labels)
	}

	for (i in seq_along(labels)) {
		if (i %in% manual_labels) {
			labels[[i]] <- names(manual_labels)[manual_labels == i][[1]]
		}
	}

	labels
}
