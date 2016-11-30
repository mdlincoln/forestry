#' Create plot of variable influence in a random forest model
#'
#' Specify which variables to compare in the model.
#'
#' @param rf random forest
#' @param data Data (if not already in the environment of the random forest)
#' @param var1 (Required) Primary variable (preferably continuous)
#' @param var2 Secondary variable
#' @param var3 Teritary variable
#' @param log_var1 Log-scale the x axis?
#'
#' @export
chart_forest <- function(rf, data = NULL, var1, var2 = NULL, var3 = NULL, log_var1 = TRUE) {
  # Use data if they have been supplied, otherwise attempt to access from the
  # model
  if (is.null(data))
    data <- rf_data(rf)

  p <- ggplot(harvest_forest(rf, d = data, var2, var3), aes_(x = as.name(var1), y = ~votes, color = ~predicted)) +
    scale_color_brewer(type = "qual") +
    geom_jitter(alpha = 0.1) +
    geom_smooth() +
    theme_bw(base_size = 18) +
    ylim(0, 1) +
    labs(y = "Probability of falling to selected class")

  if (log_var1)
    p <- p + scale_x_log10(labels = scales::comma)

  if (var2 != "(none)") {
    if (var3 != "(none)") {
      p <- p + facet_grid(paste0(var3, " ~ ", var2), labeller = label_both)
    } else {
      p <- p + facet_wrap(var2)
    }
  }

  p
}

harvest_forest <- function(rf, d = NULL, var2 = NULL, var3 = NULL) {
  rf_votes <- rf[["votes"]]

  d <- bind_cols(d, as.data.frame(rf_votes)) %>%
    tidyr::gather_(key_col = "predicted", value_col = "votes", gather_cols = colnames(rf_votes))

  if (!is.null(var2) && is.numeric(d[[var2]])) {
    mdots <- list(lazyeval::interp(
      ~cut(dvar2, breaks = quantile(dvar2, probs = seq(0, 1, length.out = min(n_distinct(dvar2), 5)))),
      dvar2 = as.name(var2)
    ))

    d <- mutate_(d, .dots = setNames(mdots, var2))

    if (!is.null(var3) && is.numeric(d[[var3]])) {
      mdots <- list(lazyeval::interp(
        ~cut(dvar3, breaks = quantile(dvar3, probs = seq(0, 1, length.out = min(n_distinct(dvar3), 5)))),
        dvar3 = as.name(var3)
      ))
      d <- mutate_(d, .dots = setNames(mdots, var3))
    }
  }

  d
}
