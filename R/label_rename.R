label_rename <- function(
	automatic_labels,
	manual_labels,
	models,
	arg,
	call
) {
	manual_labels <- tidyselect::with_vars(automatic_labels, manual_labels)

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

	manual_match <- match(labels, manual_labels)

	for (i in seq_along(manual_labels)) {
		if (is.character(manual_labels[[i]])) {
			labels[labels == manual_labels[[i]]] <- names(manual_labels)[[i]]
		} else if (is.integer(manual_labels[[i]])) {
			labels[manual_labels[[i]]] <- names(manual_labels)[[i]]
		}
	}

	labels
}
