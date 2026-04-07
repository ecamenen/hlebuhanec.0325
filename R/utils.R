find_dates <- function(x, sep = "/") {
  x <- as.data.frame(x)
  sapply(
    seq_along(x),
    function(i) any(str_detect(na.omit(x[, i]), paste("(([0-3])?\\d", ")?(0|1)?\\d", "(19|20)\\d{2}")), sep = sep)
  )
}

#' @export
find_dates <- function(x, sep = "-") {
  x <- as.data.frame(x)
  colnames(x)[
    sapply(
      seq_along(x),
      function(i) any(str_detect(na.omit(x[, i]), paste("(19|20)\\d{2}", "(0|1)?\\d", "(([0-3])?\\d)?", sep = sep)))
    )
  ]
}

#' @export
get_name_num <- function(x) {
  x <- as.data.frame(x)
  colnames(x)[
    sapply(
      colnames(x),
      function(i) {
        !any(
          x[, i] %>%
            na.omit() %>%
            as.character() %>%
            as.numeric() %>%
            is.na() %>%
            unique() %>%
            suppressWarnings()
        )
      }
    )
  ]
}


#' @export
get_binomial <- function(x) {
  x %>%
    select(where(~ {
      (is.integer(.x) || is.factor(.x)) &&
        all(.x[!is.na(.x)] %in% c(0, 1))
    })) %>%
    names()
}

#' @export
get_binomial2 <- function(x) {
  x %>%
    select(where(~ {
      (is.integer(.x) || is.factor(.x)) &&
        length(unique(.x[!is.na(.x)])) <= 2
    })) %>%
    names()
}

#' @export
get_multinomial <- function(x, n_levels = 12, max_value = 20) {
  x %>%
    select(where(~ {
      res <- (is.integer(.x) || is.factor(.x)) &&
        length(unique(.x[!is.na(.x)])) > 2 &&
        length(unique(.x[!is.na(.x)])) <= n_levels
      if (!is.null(max_value) && is.integer(.x)) {
        res && (max(.x[!is.na(.x)]) <= max_value)
      } else {
        res
      }
    })) %>%
    names()
}

#' @export
age_pyramid <- function(x, years, sex, breaks = seq(0, 100, 10), colour = c("#3438a7", "#38bcbd")) {
  df_years <- mutate(
    x,
    years = cut(
      !!sym(years),
      breaks = breaks,
      right = FALSE
    ),
    sex = !!sym(sex)
  ) %>% drop_na(sex)
  df_year_gender <- unique(pull(x, sex)) %>%
    list.map(f(i) ~ {
      fct_count(subset(df_years, `sex` == i)$years) %>%
        mutate(perc = (n / sum(n) * 100) %>% round(1), Sex = i)
    })
  df_year_gender[[1]]$n <- -df_year_gender[[1]]$n
  df_year_gender <- list.stack(df_year_gender)
  df_year_gender <- df_years$years %>%
    fct_count() %>%
    right_join(df_year_gender, by = "f")

  ggplot(df_year_gender, aes(x = n.y, y = f, fill = Sex)) +
    geom_col(width = 0.95, alpha = 0.75) +
    geom_text(
      aes(label = ifelse(Sex == "Male", n.x, "")),
      hjust = -1,
      color = "gray",
      size = 5
    ) +
    theme_minimal(base_size = 18) +
    scale_fill_manual(values = colour) +
    scale_x_continuous(
      name = "Count",
      labels = abs,
      expand = c(0.1, 0)
    ) +
    scale_y_discrete(
      name = "Age (decades)",
      labels = function(x) {
        str_remove_all(x, "\\[|\\)") %>% str_replace_all(",", "-")
      }
    )
}

#' @export
plot_date <- function(x, date_breaks = "1 month", colour = "red", cex = 1, regex = "%Y-%m", func = dmy, title = NULL, ...) {
  format_dates <- function(x) {
    months <- strftime(x, format = "%b") %>%
      str_to_upper() %>%
      str_sub(start = 1, end = 1)
    years <- year(x)
    if_else(
      is.na(lag(years)) | lag(years) != years,
      true = paste(months, years, sep = "\n"),
      false = months
    )
  }
  func2 <- switch(
    regex,
    "%Y"        = identity,
    "%Y-%m"     = ym,
    "%Y-%m-%d"  = ymd,
    ym
  )
  dates <- func(x) %>% tibble(date = .)
  # dates <- mutate(
  #     dates,
  #     year = year(date),
  #     month = month(date, label = TRUE, abbr = FALSE),
  #     day = day(date)
  # )
  x0 <- format(dates$date, regex) %>%
    fct_count() %>%
    filter(!is.na(f)) %>%
    mutate(f = func2(f))
  # days <- as.factor(dates$date) %>%
  #     fct_count() %>%
  #     mutate(f = as.Date(f)) %>%
  #     arrange(f)
  p <- ggplot(x0, aes(f, n)) +
    geom_line(color = colour, lwd = 1) +
    geom_point(color = colour) +
    geom_text_repel(
      aes(
        label = paste0(
          str_replace_all(f, "\\d{4}\\-(\\d{2})\\-(\\d{2})", "\\2"),
          ": ",
          n
        )
      ),
      colour = "red",
      size = 4.5 * cex,
      ...
    ) +
    theme_minimal() +
    labs(y = "Nb. samples", x = NULL) +
    ggtitle(title) +
    theme(
      axis.title = element_text(size = cex * 20, face = "bold.italic"),
      axis.text = element_text(colour = "gray50", size = cex * 12),
      plot.title = element_text(
        size = cex * 25,
        face = "bold",
        hjust = 0.5
      )
    )

  if(regex != "%Y")
    p + scale_x_date(date_breaks = date_breaks, labels = format_dates)
  else
    p + geom_line(aes(group = 1), color = colour, lwd = 1)
}

#' @export
label_date <- function(x) {
    sapply(x, function(val) {
      val <- abs(val)
      if (!is.na(val) & val < 30) {
        paste0(round(val), " day", ifelse(val > 1, "s", ""))
      } else if (!is.na(val) & val < 365) {
        val <- round(val / 30)
        paste0(val, " month", ifelse(val > 1, "s", ""))
      } else {
        val <- round(val / 365, 1)
        paste0(val, " year", ifelse(val > 1, "s", ""))
      }
    })
}

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

get_ctr <- function(x, type = "var", method = "contrib", max_dim = NULL) {
  if(is.null(max_dim)) {
    max_dim <- ncol(x[[type]][[method]])
  }
  x <- x[[type]][[method]]
  sapply(
    seq(max_dim),
    function(i) {
      which(abs(x[, i]) %>% sort(TRUE) > (1 / nrow(x) * 100))
    }
  )
}

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

plot_ctr0 <- function(x, type = "ind", top = 50, max_dim = NULL) {
  if(is.null(max_dim)) {
    max_dim <- ncol(x[[type]]$contrib)
  }
  list.map(
    seq(max_dim),
    f(i) ~
      {
        res <- x[[type]]$contrib[, i]
        mean_ctr <- mean(res)
        fviz_contrib(
          x,
          choice = type,
          axes = i,
          top = top,
          fill = "black",
          color  = "black"
          ) +
        GimmeMyPlot:::theme_custom() +
        ggtitle(paste("Dimension", i)) +
        geom_vline(
          xintercept = res %>% .[. > mean_ctr] %>% length() %>% `+`(0.5),
          color = "red",
          lwd = 0.5,
          lty = 2
        )
      }
  )
}

plot_ctr_omics <- function(p, Y, colour = colGrp, n_max = Inf) {
  p <- p %>%
    rownames_to_column(var = "Variable") %>%
    mutate(abs_importance = abs(importance)) %>%
    arrange(abs_importance) %>%
    tail(n_max)
  p$GroupContrib <- factor(p$GroupContrib, levels = levels(Y))
  ggplot(p, aes(x = importance, y = reorder(Variable, abs_importance), fill = GroupContrib)) +
    geom_bar(stat = "identity") +
    # scale_fill_manual(values = colour) +
    scale_fill_manual(values = setNames(colGrp[1:length(levels(Y))], levels(Y))) +
    labs(
      title = "Variable contributions",
      x = NULL,
      y = NULL,
      # fill = "Groupe"
    )  +
    theme_minimal(base_size = 14) +
    GimmeMyPlot:::theme_custom() +
    theme(panel.grid = element_blank())
}

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

plot_dimred <- function(
    coord,
    axis,
    eig = NULL,
    geom = "point",
    axis_text = FALSE,
    legend = "Batch",
    colour = GimmeMyPlot:::palette_discrete(),
    force = 0.2,
    alpha = 1,
    ellipse = FALSE,
    ...) {

  coord <- as.data.frame(coord) %>% set_colnames(c("x", "y", "var"))
  if (!is.null(eig)) {
    axis <- paste0(axis, " (", paste(round(eig[seq(2)], 1), "%)"))
  }
  coord <- as.data.frame(coord) %>% set_colnames(c("x", "y", "var"))
  p <- ggplot(coord, aes(x = x, y = y, color = var)) +
    geom_hline(yintercept = 0, size = .5, color = "gray70") +
    geom_vline(xintercept = 0, size = .5, color = "gray70") +
    labs(x = axis[1], y = axis[2], color = legend) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 15, face = "bold.italic", color = "gray50"),
      legend.title = element_text(size = 15, face = "bold.italic", color = "gray50"),
      legend.text = element_text(size = 13, color = "gray50")
    )
  if (pull(coord, "var") %>% is.numeric()) {
    p <- p + scale_color_gradientn(colors = c("darkblue", "#A6CEE3", "#FB9A99", "darkred"))
  } else {
    p <- p +
      scale_color_manual(
        values = colour,
        na.value = brewer.pal(n = 8, name = "Set2")[6]
      ) +
      guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
  }

  if (axis_text) {
    p <- p + theme(axis.text = element_text(size = 13, color = "gray50"))
  } else {
    p <- p + theme(axis.text = element_blank())
  }
  if (geom == "point") {
    p <- p + geom_point(alpha = alpha)
  } else {
    p <- p + geom_text_repel(aes(label = rownames(coord)), force = force, max.iter = 500, ..., alpha = alpha)
  }
  if (ellipse) {
    p + stat_ellipse()
  } else {
    p
  }
}

trans_coord <- function(coord, add = NULL) {
  res <- as.data.frame(coord) %>%
    set_colnames(paste0("Dim. ", seq(ncol(.))))
  if (!is.null(add))
    res %>% mutate(batch = add)
  else
    res
}

format_cytometry <- function(x) {
  str_replace_all(x, "Foxp3", "FOXP3") %>%
    str_replace_all("p", "\\+") %>%
    str_replace_all("n", "\\-") %>%
    str_replace_all("[\\._]", " \\/ ") %>%
    str_replace_all("Tco-v", "Tconv")
}

col_kable2 <- function(x, ...) {

  format_column <- function(x, i, data) {
    column_spec(
      x,
      column = i + 1,
      background = colorRampPalette(c("white", "black"))(100)[abs(round(data[, i], 2) * 100)],
      color = ifelse(data[, i] > 0.5, "white", "black")
    )
  }

  res <-  kable0(x, ...)
  seq_len(ncol(x)) %>%
    reduce(
      .f = function(k, i) format_column(k, i, x),
      .init = res
    )
}

dist_centroids <- function(x, dim = 1) {

  ind_coord <- res_plsda$variates$X[, dim]
  centroids <- GimmeMyCluster::calculate_centroids(ind_coord, as.numeric(res_plsda$Y)) %>%
    as.data.frame() %>%
    select(V1) %>%
    set_rownames(levels(Y))

  list.map(ind_coord, f(x, y, z) ~ abs(centroids - x) %>% set_colnames(z)) %>%
    list.cbind()
}

best_class <- function(x, dim = 1) {
  best <- dist_centroids(x, dim) %>% apply(2, which.min)
  levels(x$Y) %>% .[best] %>% set_names(rownames(x$X))
}

plot_probs <- function(probs, Y) {
  Y <- set_names(Y, rownames(probs))
  list.map(
    levels(Y),
    f(x) ~plot_bar(
      probs[which(Y == x), x, drop = FALSE] * 100,
      cex = 0.8,
      sample_size = 100,
      # label_x = "value",
      n_max = 10,
      colour = c("#99000D", "gray"),
      title = x,
      sort ="desc"
    )
  )
}

minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

minmax2 <- function(df) {
  as.data.frame(lapply(df, function(x) (x - min(x)) / (max(x) - min(x)))) %>%
    set_rownames(rownames(df))
}

rescale_from_df <- function(x, y = c(1, 100)) {
  old_min <- min(x, na.rm = TRUE)
  new_min <- min(y, na.rm = TRUE)

  as.data.frame(
    lapply(x, function(i) {
      ((i - old_min) / (max(x, na.rm = TRUE) - old_min)) * (max(y, na.rm = TRUE) - new_min) + new_min
    })
  )
}

pval_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  # if (p < 0.1) return(".")
  return("")
}

format_pval <- function(x, digits = 2) {
  paste0("p = ", signif(x, digits), pval_stars(x))
}

format_auto <- function(x, digits = 2, sci_thresh = 1e4) {
  ifelse(
    abs(x) >= sci_thresh | abs(x) < 1 / sci_thresh,
    formatC(x, format = "e", digits = digits),
    formatC(x, format = "f", digits = digits)
  )
}

plot_missing0 <- function(
    df_sub,
    type = 1,
    colour  = rev(brewer.pal(9, "Reds")[-seq(2)]),
    label_x = "value",
    all = FALSE,
    threshold = Inf,
    ...
  ) {
  func <- ifelse(type == 1, nrow, ncol)
  func2 <- ifelse(type == 1, ncol, nrow)
  var <- ifelse(type == 1, "Samples", "Variables")
  res <- apply(df_sub, type, function(x) (is.na(x)) %>% which() %>% length())
  if (threshold != Inf) {
    res <- res %>%
      .[. <= threshold]
    if (length(res) == 0) {
      tmp <- 0
    } else {
      tmp <- length(res) / func(df_sub)
    }
    print(paste0(var, " removed with the selected threshold: ", func(df_sub) - length(res), " (", round(100 - (tmp * 100), 2), "%)."))
  }
  res <- res %>% .[. != 0]
  if (type == 1) {
    colour <- colour[which(rownames(df_sub) %in% names(res))]
  } else {
    colour <- rev(brewer.pal(9, "Reds")[-seq(2)])
  }
  p <- plot_bar(
    res,
    sample_size = func2(df_sub),
    colour = colour,
    label_x = label_x,
    ...
  ) +
    ggtitle(NULL)

  if (isTRUE(all)) {
    p <- p +
      geom_hline(
        yintercept = func2(df_sub)/2,
        linetype = "dashed",
        linewidth = 1
      )
  }
  return(list(p = p, res = res))
}
#' @export
plot_missing <- function(
    df_sub,
    colour  = rev(brewer.pal(9, "Reds")[-seq(2)]),
    label_x = "value",
    all = FALSE,
    threshold1 = Inf,
    threshold2 = Inf,
    ...
  ) {
  p <- list()
  p[[1]] <- plot_missing0(
    df_sub,
    type = 2,
    colour  = colour,
    label_x = label_x,
    all = all,
    threshold = threshold1,
    ...
  )
  p[[2]] <- plot_missing0(
    df_sub,
    type = 1,
    colour  = colour,
    label_x = label_x,
    all = all,
    threshold = threshold2,
    ...
  )

  list.map(p, .$p) %>%
    plot_grid(plotlist = .) %>%
    plot()
  return(sort(p[[2]]$res, decreasing = TRUE))
}

#' @export
plot_outliers <- function(df, n_max = 15, ...) {
  p2 <- list()
  categorical_variables <- df %>%
    select(where(is.factor)) %>%
    names()
  numeric_variables <- df  %>%
    select(-categorical_variables) %>%
    get_name_num(.)
  l_outliers <- select(df, numeric_variables) %>%
    list.map(
      f(x, y, z) ~
        select(df, z) %>%
        filter(!is.na(!!sym(z))) %>%
        identify_outliers(method = "sd") %>%
        names()
    ) %>%
    unlist()
  p2[[1]] <- plot_bar_mcat(
    l_outliers,
    n_max = n_max,
    sample_size = (length(numeric_variables)),
    ...
  ) + ggtitle(NULL)

 l_rare <- extract_rare(df) %>%
   map(rownames) %>%
   unlist()

  p2[[2]] <- plot_bar_mcat(
    l_rare,
    n_max = n_max,
    sample_size = length(categorical_variables),
    ...
  ) + ggtitle(NULL)

  p2[[3]] <- plot_bar_mcat(
    c(l_rare, l_outliers),
    n_max = n_max,
    sample_size = length(categorical_variables) +
      length(numeric_variables),
    ...
  ) + ggtitle(NULL)

  plot_grid(plotlist = p2, ncol = 3) %>% plot()
  return(
    c(l_rare, l_outliers) %>%
      fct_count() %>%
      arrange(desc(n)) %>%
      select(f, n) %>%
      pull(n, f)
    )
}

#' @export
extract_rare <- function(df, threshold = 5) {
  categorical_variables <- df %>%
    select(where(is.factor)) %>%
    names()

  tmp <- select(df, categorical_variables) %>%
    mutate(across(where(is.factor), as.character))

  tab_stats <- list.map(tmp, f(x, x, z) ~ select(tmp, z) %>% print_multinomial()) %>%
    list.rbind()

  mutate(
    tab_stats,
    N = str_remove_all(Statistics, " \\(.*") %>% as.numeric()) %>%
    filter(N < threshold) %>%
    split(., seq(nrow(.))) %>%
    map(~ tmp %>%
          filter(
            if (is.na(.x$Levels))
              is.na(!!sym(.x$Variables[[1]]))
            else
              !!sym(.x$Variables[[1]]) == .x$Levels[[1]]
          ) %>%
          select(.x$Variables[[1]])
    )
}

#' @export
print_group_numeric <- function(df, vars, format = identity) {
  list.map(
    vars,
    f(x) ~ {
      df %>%
        group_by(groupe) %>%
        reframe(
          if (!all(is.na(.data[[x]]))) {
            print_numeric(.data[[x]])
          } else {
            tibble()
          }
        ) %>%
        mutate(Variable = x) %>%
        relocate(Variable)
    }
  ) %>%
    compact() %>%
    list.rbind() %>%
    select(-c("Variables", "Kurtosis", "Skewness", "Normality")) %>%
    mutate(Variable = format(Variable))
}

#' @export
write_statistics <- function(x, file = file.path(getwd(), "statistics.xlsx"), format = identity) {
  l_var <- list()
  l_var$categorical <- select(x, where(is.factor)) %>%
    print_binomial() %>%
    mutate(across(everything(), ~replace_na(.x, "NA")))

  l_var$numerical <- select(x, where(is.numeric)) %>%
    print_numeric()

    wb <- createWorkbook()
    list.map(
      l_var,
      f(x, y, z) ~ {
        addWorksheet(wb, sheetName = z)
        mutate(x, Variables = format(Variables)) %>%
          writeData(wb, sheet = z, x = .)
      }
    )
    saveWorkbook(wb, file, overwrite = TRUE)
}

#' @export
plot_heatmap_clinic <- function(x, vars, func = func_format, normalize = TRUE, ...) {
  yintercept <- x %>%
    count(groupe) %>%
    deframe() %>%
    cumsum() %>%
    head(2)

  column_to_rownames(x, "patient") %>%
    select(all_of(vars)) %>%
    rename_with(func) %>%
    plot_heatmap(sort = FALSE, normalize = normalize, ...) +
    geom_vline(
      xintercept = yintercept + .5,
      linewidth = 1,
      linetype = "dashed"
    ) +
    theme(
      axis.text.x = element_text(
        size = 10,
        color = "gray50",
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.ticks = element_line(colour = "gray50")
    )
}

#' @export
plot_radar_clinic <- function(x, vars, func = func_format, ...) {
  x %>%
    column_to_rownames("patient") %>%
    select(c(all_of(vars), "groupe")) %>%
    group_by(groupe) %>%
    summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
    column_to_rownames("groupe") %>%
    rename_with(~ .x %>% func()) %>%
    plot_radar()
}

#' @export
table_groupe_numeric <- function(x, vars, format = identity, file = NULL) {
  (p <- print_group_numeric(x, vars, format))

  if (!is.null(file))
    kable0(p) %>% save_kable(file = file, zoom = 2)
}
