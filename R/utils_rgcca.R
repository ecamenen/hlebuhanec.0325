#' @export
get_cor_rgcca <- function(x, block = 1, comp = 1, cutoff = .5, metadata = NULL, ...) {
  correlate(
    x$blocks[[block]],
    x$Y[[block]][, comp, drop = FALSE],
    cutoff = cutoff,
    ...
  ) %>%
    pluck(1) %>%
    .[1, .[1, ] != 0] %>%
    enframe(name = "gene", value = "V1") %>%
    column_to_rownames("gene") %>%
    join_metadata(metadata)
}

#' @export
get_ctr_rgcca <- function(x, block = 1, comp = 1, metadata = NULL) {
  res <- x$a[[block]][, comp, drop = FALSE] %>%
    .[. != 0, , drop = FALSE] %>%
    as.data.frame() %>%
    join_metadata(metadata)

  pull(res, 1) %>% set_names(rownames(res))
}

#' @export
join_metadata <- function(x, metadata = NULL) {
  if (!is.null(metadata)) {
    x <- x %>%
      set_rownames(rownames(.) %>% str_remove("^s-")) %>%
      rownames_to_column("ensembl_gene_id")%>%
      left_join(metadata, by = "ensembl_gene_id") %>%
      filter(gene_name != "") %>%
      mutate(gene_name2 = snakecase::to_any_case(gene_name, case = "none", sep_in = NULL, unique_sep = "_") %>% str_replace("_1", " (2)")) %>%
      column_to_rownames("gene_name2") %>%
      select("V1")
  }
  x
}

#' @export
plot_bar_rgcca <- function(x, n_max = 15, ...) {
    plot_bar(x, n_max = n_max, abs = TRUE, label_x = "value", threshold = 0, digits = 2, colour = "black", ...)
}
