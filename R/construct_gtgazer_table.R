construct_gtgazer_table <- function(columns) {
	gt::gt(columns) %>%
		gt::fmt_markdown(tidyselect::everything()) %>%
		gt::sub_missing(missing_text = "") %>%
		gt::cols_label("term" ~ "", .fn = gt::md) %>%
		gt::cols_align(align = "center", columns = -term) %>%
		gt::cols_align(align = "left", columns = term) %>%
		gt::tab_source_note(gt::md("*Note:* **p* < 0.05; ***p* < 0.01; ****p* < 0.001")) %>%
		gt::tab_style(
			gt::cell_borders(sides = "top", color = "#D3D3D3", weight = gt::px(2)),
			gt::cells_body(
				rows = .data$group == "summaries" &
					dplyr::lag(.data$group) != "summaries"
			)
		) %>%
		gt::tab_spanner_delim(
			delim = attr(columns, "delim"),
			columns = c(-1, -2),
			split = "last"
		) %>%
		gt::tab_spanner(
			italicize("Dependent variable:"),
			columns = c(-1, -2),
		) %>%
		gt::cols_move_to_end("group") %>%
		gt::cols_hide("group") %>%
		md_tab_spanners()
}

md_tab_spanners <- function(table) {
	table$`_spanners`$spanner_label <- purrr::map(
		table$`_spanners`$spanner_label,
		gt::md
	)

	table
}
