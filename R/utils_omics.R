format_dea <- function(
    x,
    metadata_genes = NULL,
    fc_threshold = 1,
    p_threshold = 0.05
) {
  # required_cols <- c("ensembl_gene_id", "gene_name")
  # if (!all(required_cols %in% colnames(metadata_genes))) {
  #     stop("metadata_genes must contain the columns: ", paste(required_cols, collapse = ", "))
  # }
  tmp <- x %>%
    as.data.frame()

  if (!is.null(metadata_genes)) {
    tmp <- tmp %>%
      rownames_to_column("ensembl_gene_id") %>%
      left_join(
        metadata_genes,
        by = "ensembl_gene_id"
      )
  }
  mutate(
    tmp,
    log10p = -log10(padj),
    gene_name = str_remove_all(gene_name, "_\\d+"),
    Expression = case_when(
      log2FoldChange >= fc_threshold & padj <= p_threshold ~ "Up-regulated",
      log2FoldChange <= -fc_threshold & padj <= p_threshold ~ "Down-regulated",
      TRUE ~ "ns"
    ),
    padj = ifelse(padj == 0, min(padj[padj > 0], na.rm = TRUE), padj)
  ) %>%
    filter(abs(log2FoldChange) >= 0.01) %>%
    relocate("gene_name") %>%
    as_tibble()
}

print_dea <- function(x, base = 2, ...) {
  func <- if (base == 2) function(x) 2^x else exp
  GimmeMyOmics:::top_genes(x, n = 10000, fc_threshold = 0, p_threshold = 1, ...) %>%
    filter(Expression != "ns") %>%
    mutate(
      FC = ifelse(
        log2FoldChange > 0,
        round(func(log2FoldChange), 2),
        -round(func(abs(log2FoldChange)), 2)
      ),
      FDR = format(padj, digits = 2, scientific = TRUE)
    ) %>%
    rename(
      alias = "gene_name",
      full_name = "description"
    ) %>%
    select(alias, full_name, FC, FDR)
}
