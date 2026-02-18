#' export
plot_cor <- function(
    x,
    y = NULL,
    method = "spearman",
    color = "red",
    alpha = 0.5,
    cex = 1,
    pch_size = cex * 3,
    xlab = NULL,
    ylab = NULL,
    legend = NULL,
    dec = 1,
    wrap = 20,
    model = "lm"
) {
  x <- as.data.frame(x)
  if (is.null(y)) {
    if (NCOL(x) < 2) {
      stop("x must be have at least two columns.")
    } else if (NCOL(x) > 2) {
      warning(
        "If x has more than two variables and y is null",
        "only the two first are plotted."
      )
    }
    if (is.null(xlab)) {
      xlab <- colnames(x)[1]
    }
    if (is.null(ylab)) {
      ylab <- colnames(x)[2]
    }
    df <- select(x, seq(2)) %>% set_colnames(c("x", "y"))
    n <- na.omit(df) %>% nrow()
  } else {
    y <- as.data.frame(y)
    if (NCOL(y) > 1) {
      warning(
        "If y is non-null",
        "only the first column of y is plotted."
      )
    }
    if (is.null(ylab) && !is.null(colnames(y))) {
      ylab <- colnames(y)[1]
    }
    if (NCOL(x) > 1) {
      n <- nrow(x)

    } else {
      if (is.null(xlab) && !is.null(colnames(x))) {
        xlab <- colnames(x)
      }
      df <- data.frame(x = x, y = y) %>% set_colnames(c("x", "y"))
      n <- na.omit(df) %>% nrow()
    }
  }

  if (NCOL(x) > 2 && !is.null(y)) {
    cor <- GimmeMyPlot:::mcor_test(x, y, TRUE, TRUE)
  } else {
    cor <- cor.test(pull(df, x), pull(df, y), method = method, use = "complete.obs")
  }

  pval <- paste("=", format(cor$p.value, digits = dec))
  sign <- as.vector(cor$p.value) %>%
    data.frame() %>%
    add_significance0(".") %>%
    pull(2) %>%
    str_replace_all("ns", "") %>%
    str_replace_all("\\*\\*\\*\\*", "\\*\\*\\*")
  pval[cor$p.value < 0.001] <- "< 0.001"

  if (NCOL(x) > 2 && !is.null(y)) {
    subtitle0 <- paste0("R = ", round(cor$estimate, dec + 1) * sign(cor$estimate), sign) %>%
      paste0(colnames(x), "\n", .)
    subtitle <- NULL
    df <- cbind(x, y) %>% gather(, , -!!sym(colnames(y)))
    colnames(df)[1] <- "y"
    if (is.null(legend))
      legend <- "Legend"
    print(subtitle0)
    if(length(unique(color)) != ncol(x))
      color <- palette_discrete()
    p <- ggplot(df, aes(x = value, y = y, colour = key)) +
      geom_point(aes(color = key), size = pch_size, alpha = alpha) +
      geom_smooth(
        method = model,
        aes(fill = key),
        alpha = alpha / 3,
        size = 0,
        na.rm = TRUE
      ) +
      stat_smooth (
        method = model,
        aes(color = key),
        geom = "line",
        lwd = 1.25
      ) +
      scale_fill_manual(values = color, name = legend, labels = subtitle0) +
      scale_color_manual(values = color, name = legend, labels = subtitle0)
  } else {
    subtitle <- paste0("R = ", round(cor$estimate, dec + 1) * sign(cor$estimate), ", ", "p ", pval, sign, ", N = ", n)

    df <- na.omit(df)
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = color, size = pch_size, alpha = alpha, na.rm = TRUE) +
      # geom_spline(color = color) +
      geom_smooth(
        method = model,
        fill = color,
        alpha = alpha / 3,
        size = 0,
        na.rm = TRUE
      ) +
      stat_smooth (
        method = model,
        color = color,
        geom = "line",
        lwd = 1.25
      )
  }

  p +
    labs(
      x = str_wrap(xlab, wrap),
      y = str_wrap(ylab, wrap),
      subtitle = subtitle
    ) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(
        hjust = 0.5,
        size = cex * 14,
        color = "gray50",
        face = "italic"
      ),
      axis.title = element_text(
        hjust = 0.5,
        size = cex * 16,
        color = "gray50",
        face = "bold.italic"
      ),
      axis.text = element_text(
        size = cex * 10,
        color = "gray50"
      ),
      legend.title = element_text(
        size = cex * 14,
        color = "gray50",
        face = "bold.italic"
      )
    )
}
