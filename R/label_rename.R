label_rename <- function(
	automatic_labels,
	manual_labels,
	models,
	arg,
	call
) {
	label_exprs <- as.list(rlang::quo_get_expr(rlang::enquo(manual_labels)))

	manual_labels <- purrr::map(
		seq_along(label_exprs)[-1],
		\(i) {
			this_manual_label <- as.call(c(rlang::expr(c), label_exprs[i]))

			these_manual_labels <- try(
				tidyselect::eval_rename(
					this_manual_label,
					rlang::set_names(automatic_labels)
				),
				silent = TRUE
			)

			if (inherits(these_manual_labels, "try-error")) {
				return(rlang::eval_bare(this_manual_label))
			}

			these_manual_labels
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
