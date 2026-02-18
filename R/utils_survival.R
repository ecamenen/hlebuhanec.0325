plot_survival <- function(x, y, digits = 1, ...) {

  stats <- survdiff(formula(x), data = y)
  pval <- 1 - pchisq(stats$chisq, df = length(stats$n) - 1)
  pval_text <- format_pval(x)

  plot <- ggsurvplot(
    x,
    data = y,
    pval = pval_text,
    conf.int = TRUE,
    # risk.table = "abs_pct",
    risk.table.col = "strata",
    # risk.table.y.text.col = TRUE,
    # risk.table.y.text = FALSE,
    linetype = "strata",
    surv.median.line = "hv",
    ggtheme = GimmeMyPlot:::theme_custom() +
      theme(
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major =  element_line(color = "gray80", linewidth  = .5, linetype = "dashed")
      ),
    palette = GimmeMyPlot:::palette_discrete(),
    ...
  ) +
    labs(x = "Days of survival")

  plot$plot <- plot$plot +
    geom_text_repel(
      data = surv_median(x),
      aes(x = median, y = 0, label = round(median, digits)),
      vjust = 0,
      color = "black",
      size = 4.5
    )

  return(plot$plot)
}

summary_hr <- function(x, y) {
  tidy(x, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(term = y)
}

plot_hr <- function(x, y, width_title = 30) {
  mod <- summary_hr(x, y) %>%
    mutate(
      term_label = paste0(
        str_wrap(y, width_title), "\n (",
        format_auto(estimate), ", ",
        format_auto(conf.low), "-",
        format_auto(conf.high),
        ", ",
        format_pval(p.value, digits = 2), ")"
      )
    )
  ggplot(
    mod,
    aes(
      y = term,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high
    )
  ) +
    geom_errorbarh(height = 0.3, linewidth = 0.8) +
    geom_point(size = 3) +
    # geom_pointrange(size = 1) +
    geom_vline(xintercept = 1, color = "red", linewidth = 1) +
    labs(x = "Hazard ratios and 95% CIs", title = mod$term_label, y = NULL) +
    GimmeMyPlot:::theme_custom() +
    theme(
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major =  element_line(color = "gray80", linewidth  = .5, linetype = "dashed")
    )
}

