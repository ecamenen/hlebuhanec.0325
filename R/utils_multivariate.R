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
