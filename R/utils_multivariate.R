#' @export
plot_eig <- function(x, method = "mean", title = NULL, digits = 1) {
  if (method == "cum"){
    x <- cumsum(x)
    title <- ifelse(is.null(title), "% Cumulated\n explained variance", title)
    yintercept <- 80
    title_lab <- x %>% .[. < 80]
  } else {
    title <- ifelse(is.null(title), "% Explained variance", title)
    yintercept <- mean(x)
    title_lab <- x %>% .[. > mean(.)] %>% cumsum()
  }
  plot_eig0(x, title) +
    # scale_x_continuous(breaks = seq(length(x))) +
    ylim(0, NA) +
    geom_hline(yintercept = yintercept, color = "gray50", lwd = 1, lty = 2) +
    ggtitle(
      paste0(length(title_lab), " dimensions kept\n(", last(title_lab) %>% round(digits), "% of explained variance)"))
}

#' @export
plot_eig0 <- function(x, y_label = "Eigen value") {
  x %>%
    data.frame(
      dim = seq_along(.),
      var = .
    ) %>%
    ggplot(aes(x = dim)) +
    geom_line(aes(y = var), color = "red", lwd = 1) +
    geom_point(aes(y = var), color = "red", size = 2, pch = 3) +
    labs(
      title = NULL,
      x = "Dimension",
      y = y_label
    ) +
    xlim(1, NA) +
    theme_minimal() +
    GimmeMyPlot:::theme_custom()
}

#' @export
plot_ctr <- function(x, type = "ind", max_dim = NULL, n_max = 10, ...) {
  if(is.null(max_dim)) {
    max_dim <- ncol(x[[type]]$contrib)
  }
  list.map(
    seq(max_dim),
    f(i) ~ {
      x[[type]]$contrib[, i] %>%
        plot_bar(abs = TRUE, label_x = "none", n_max = n_max, colour = "black", ...) +
        ggtitle(paste("Dimension", i))
    }
  )
}

#' @export
plot_ctr2 <- function(x, choice = "ind") {
list.map(
  seq(4),
  f(i) ~ {
    p_base <- fviz_contrib(x, choice = choice, axes = i, top = 50)
    df_contrib <- p_base$data %>% arrange(desc(contrib))
    med <- mean(df_contrib$contrib)
    idx_cut <- which(df_contrib$contrib < med)[1]

    p_base +
      geom_vline(
        xintercept = idx_cut - 0.5,
        colour = "red",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      GimmeMyPlot:::theme_custom(cex = 0.8) +
      ggtitle(paste("Dimension", i))
  }
)
}

#' @export
theme_perso_2D <- function(p) {
  p +
    theme_classic() +
    geom_vline(
      xintercept = 0,
      col = "grey",
      linetype = "dashed",
      size = 0.5
    ) +
    geom_hline(
      yintercept = 0,
      col = "grey",
      linetype = "dashed",
      size = 0.5
    ) +
    GimmeMyPlot:::theme_custom()
}

#' @export
plot_cor_circle <- function(
    x,
    y,
    comp =  seq(2),
    colour = brewer.pal(9, "Reds"),
    n_max = 20,
    force = 500,
    metadata = NULL,
    var_to_join = "ensembl_gene_id",
    var_to_name = "gene_name",
    ...
    ) {

  get_circle <- function(
    center = c(0, 0),
    diameter = 2,
    npoints = 100
  ) {
    r <- diameter / 2
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  x$ind$coord <- x$ind$coord[, comp]  %>%
    as.data.frame()
  y <- as.data.frame(y)

  print_comp <- function(x, i) paste0("Comp .", i, " (", round(x[i], 1), "%)")
  df <- correlate(y, x$ind$coord[, comp], method_adjust = "none", cutoff = 0) %>%
    pluck("r") %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(c("V1", "V2")) %>%
    filter(V1 != 0 | V2 != 0) %>%
    mutate(
      r1 = ifelse(V1 != 0, rank(-abs(V1)), NA),
      r2 = ifelse(V2 != 0, rank(-abs(V2)), NA),
      rank = rowMeans(cbind(r1, r2), na.rm = TRUE)
    ) %>%
    arrange(rank) %>%
    head(n_max)

  if (!is.null(metadata)) {
    df <- left_join(rownames_to_column(df, var_to_join), metadata) %>%
      filter(!!sym(var_to_name) != "", !is.na(!!sym(var_to_name))) %>%
      set_rownames(
        pull(., var_to_name) %>%
        snakecase::to_any_case( case = "none", sep_in = NULL, unique_sep = "_") %>%
        str_replace("_1", " (2)") %>%
        str_replace("_2", " (3)")
      ) %>%
      select(V1, V2)
  }

  df$strength <- sqrt(df[, 1]^2 + df[, 2]^2)

  p <- ggplot(df, aes(df[, 1], df[, 2])) +
    geom_text_repel(
        aes(label = rownames(df), colour = strength),
        hjust = 0.5,
        vjust = -1,
        force = force,
        ...
      ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    labs(
      x = print_comp(x$eig[, 2], comp[1]),
      y = print_comp(x$eig[, 2], comp[2]),
      colour = "Correlation"
    ) +
    scale_color_gradientn(colours = colour, limits = c(0, 1)) +
    theme_classic() +
    theme_custom()  +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    ) +
    scale_x_continuous(expand = expansion(mult = .1)) +
    scale_y_continuous(expand = expansion(mult = .1))

  circles <- list(
    list(diameter = 2,   linetype = "solid"),
    list(diameter = 1,   linetype = "dashed"),
    list(diameter = 1.5, linetype = "dotted")
  )

  p +
    lapply(circles, \(c) {
      geom_path(
        aes(.data$x, .data$y),
        data = get_circle(diameter = c$diameter),
        col = "black",
        linewidth = 1,
        linetype = c$linetype
      )
    })
}

plot_ind_quant <- function(x, var, axes = seq(2), legend = "Score") {
  fviz_mca_ind(
    x,
    repel = TRUE,
    axes = axes,
    col.ind = group_vector,
    gradient.cols = colPers,
    addEllipses = FALSE,
    ellipse.type = "confidence",
    ggtheme = theme_minimal()
  ) %>% theme_perso_2D() +
    labs(title = NULL, color = legend)
}
