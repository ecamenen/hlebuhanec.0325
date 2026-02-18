plot_roc <- function(x, cex = 1, dec = 3) {
  p <- ggplot(x, aes(m = pred, d = obs)) +
    geom_roc(n.cuts = 0, labels = FALSE, color = "red", size = 1.2) +
    theme_classic() +
    ylab("Sensitivity (TPR)") +
    xlab("1 - Specificity (FPR)") +
    labs(subtitle = paste0("AUC: ", auc(x$obs, x$pred) %>% round(dec))) +
    GimmeMyPlot:::theme_custom() +
    theme(plot.subtitle = element_text(color = "gray", hjust = 0.5, size = cex * 15))
}

plot_auc_classes <- function(x, Y) {
  list.map(
    levels(Y),
    f(i) ~ data.frame(
      pred = x[, i],
      obs = as.numeric(Y == i)
    ) %>%
      plot_roc() +
      ggtitle(i)
  )
}

# + geom_rocci(fill="pink") +
print_scores <- function(x) {
  lapply(
    c("byClass", "overall"), # Se, Sp, ACC, kappa
    function(i) {
      lapply(
        seq(2),
        function(j) x[[i]][j] %>% round(3)
      )
    }
  ) %>% unlist()
}

col_kable <- function(k, x, i) {
  n <- sum(x[, i])
  k %>%
    column_spec(
      i+1,
      background =  colorRampPalette(c("white", "black"))(n+1)[x[, i]+1],
      color = ifelse(x[, i]/n > 0.5, "white", "black")
    )
}

print_conf <- function(tmp) {
  res <- kable(tmp$table, align = "c") %>%
    kable_styling(full_width = F, stripe_color  = "white")
  seq_len(ncol(tmp$table)) %>%
    reduce(~col_kable(.x, tmp$table, .y), .init = res) %>%
    column_spec(1, bold = TRUE, color = "black") %>%
    row_spec(0:2, extra_css = "border-bottom-style: none; border-top-style: none")
}

col_kable3 <- function(k, x, i) {
  k %>%
    column_spec(
      i+1,
      background =  colorRampPalette(c("white", "black"))(101)[abs(x[, i]) * 100 +1],
      color = ifelse(x[, i] > 0.5, "white", "black")
    )
}

kable_class0 <- function(tmp) {
  res <- kable(tmp, align = "c") %>%
    kable_styling(full_width = F, stripe_color  = "white")
  seq_len(ncol(tmp)) %>%
    reduce(~col_kable3(.x, tmp, .y), .init = res) %>%
    column_spec(1, bold = TRUE, color = "black") %>%
    row_spec(0:2, extra_css = "border-bottom-style: none; border-top-style: none")
}

kable_class <- function(x) {
  tmp <- round(x$byClass, 2)
  if(ncol(x$table) > 2)
    tmp[seq(ncol(x$table)), seq(2)] %>%
    as.data.frame() %>%
    set_rownames(rownames(.) %>% str_remove_all("Class\\: ")) %>%
    kable_class0()
  else
    tmp[seq(2)]
}

calculate_class_weights <- function(y, method = "inverse", power = 1, custom_weights = NULL) {
  #' Calcule automatiquement les poids de classes pour déséquilibre
  #'
  #' @param y Variable réponse factor
  #' @param method Méthode de calcul ("inverse", "balanced", "sqrt", "custom")
  #' @param power Puissance pour les méthodes inverse/sqrt (défaut=1)
  #' @param custom_weights Vecteur personnalisé (si method="custom")
  #'
  #' @return Vecteur nommé de poids normalisés

  if(!is.factor(y)) y <- as.factor(y)
  class_counts <- table(y)
  n_classes <- length(class_counts)

  if(!is.null(custom_weights)) {
    if(length(custom_weights) != n_classes) {
      stop("custom_weights length must match number of classes")
    }
    weights <- custom_weights
    names(weights) <- names(class_counts)
  } else {
    switch(method,
           "inverse" = {
             weights <- 1 / (class_counts ^ power)
           },
           "balanced" = {
             weights <- sum(class_counts) / (n_classes * class_counts)
           },
           "sqrt" = {
             weights <- 1 / sqrt(class_counts)
           },
           stop("Unknown method. Choose 'inverse', 'balanced', 'sqrt' or 'custom'")
    )
  }

  weights <- weights / sum(weights)

  return(weights)
}

plot_ctr_rf <- function(x, ...)
  plot_ctr_classif(importance(x), ...)


plot_ctr_classif <- function(x, i = "MeanDecreaseGini", width_text = 25, ...) {
  plot_bar(
    x[, i, drop = FALSE],
    n_max = 10,
    width_text = width_text,
    title = i %>% str_replace_all("Inc", "Increase") %>% {
      ifelse(!str_detect(., "^[A-Z]+$"),
             str_replace_all(., "([A-Z])", " \\1") %>% trimws(),
             .)
    },
    threshold = 0,
    colour = c("#99000D", "gray"),
    label_x = "value",
    ...
  )
}
