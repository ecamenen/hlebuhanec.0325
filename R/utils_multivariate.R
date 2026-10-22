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

  print_comp <- function(x, i) paste0("Comp .", i, " (", round(x[i], 1), "%)")
  df <- correlate(y, x$ind$coord[, comp], method_adjust = "none", cutoff = 0) %>%
    pluck("r") %>%
    t() %>%
    as.data.frame()
df$strength <- sqrt(df[, 1]^2 + df[, 2]^2)

  ggplot(df, aes(df[, 1], df[, 2])) +
    geom_text_repel(
        aes(label = rownames(df), colour = strength),
        hjust = 0.5,
        vjust = -1
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
    scale_y_continuous(expand = expansion(mult = .1)) +
    geom_path(
      aes(.data$x, .data$y),
      data = get_circle(),
      col = "black",
      linewidth = 1
    ) +
    geom_path(
      aes(.data$x, .data$y),
      data = get_circle() / 2,
      col = "black",
      linewidth = 1,
      linetype = "dashed"
    )
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
