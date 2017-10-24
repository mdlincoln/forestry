#' Create plot of variable influence in a random forest model
#'
#' Specify which variables to compare in the model.
#'
#' @param sim_data Data returned from \link{simulate_data}
#' @param log_var1 Log-scale the x axis?
#'
#' @export
chart_forest <- function(sim_data, log_var1 = TRUE) {
  stopifnot(inherits(sim_data, "lumberjackData"))

  head(sim_data)

  var1 <- names(sim_data)[2]
  var2 <- names(sim_data)[3]
  var3 <- names(sim_data)[4]

  p <- ggplot(sim_data, aes_(x = as.name(var1), y = ~preds)) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw(base_size = 18) +
    labs(y = "Probability of falling to selected class")

  if (log_var1)
    p <- p + scale_x_log10(labels = scales::comma)

  if (is.na(var2)) {
    p <- p + geom_line(size = 1)
  } else {
    p <- p + geom_line(aes_(color = as.name(var2)), size = 1)
    if (!is.na(var3))
      p <- p + facet_wrap(var3, labeller = label_both)
  }

  p
}

#' Simulate data for partial dependence plotting
#'
#' @param rf random forest
#' @param d The data frame used to train the original forest
#' @param class Which class to plot
#' @param var1 (Required) Primary variable (preferably continuous)
#' @param breaks1 How many values of the primary variable should be sampled when
#'   calculating partial dependence?
#' @param var2 Secondary variable
#' @param breaks2 If \code{var2} is numeric, in to how many categories should it
#'   be cut?
#' @param shiny_session If a Shiny \link[shiny]{session} object is passed, a
#'   progress bar will be displayed while simulating new data.
#' @param n_cores How many cores to use when calculating in parallel? Defaults
#'   to all available cores.
#'
#' @import doParallel
#' @import foreach
#'
#' @export
simulate_data <- function(rf, d, class, var1, breaks1 = 50, var2 = NULL, breaks2 = 50, var3 = NULL, n_cores = parallel::detectCores(), ...) {

  combos <- create_combos(d, var1, breaks1, var2, var3)

  registerDoParallel(cores = n_cores)

  new_d <- combo_handler(rf, d, class, combos, n_cores, var1, var2, var3)

  class(new_d) <- c(class(new_d), "lumberjackData")
  new_d
}

#' Simulate data from a list of random forests
#' @export
#' @inheritParams simulate_data
list_sim_data <- function(rf, class, var1, breaks1 = 50, var2 = NULL, var3 = NULL, n_cores = parallel::detectCores(), progress = interactive(), ...) {
  combos <- create_combos(d = rf[[1]][["train_data"]], var1, breaks1, var2, var3)

  registerDoParallel(cores = n_cores)

  if (progress)
    pb <- utils::txtProgressBar(max = length(rf), title = "Simulating data...", style = 3)
  new_d <- purrr::map_df(rf, function(x) {
    if (exists("pb")) utils::setTxtProgressBar(pb, value = utils::getTxtProgressBar(pb) + 1)
    combo_handler(x[["rf"]], d = rf[[1]][["train_data"]], class, combos, n_cores, var1, var2, var3)
  })
  if (exists("pb")) close(pb)

  class(new_d) <- c(class(new_d), "lumberjackData")
  new_d
}

create_combos <- function(d, var1, breaks1, var2, var3) {
  if (is.numeric(d[[var1]])) {
    sim_var1 <- quantile(d[[var1]], seq(0, 1, length.out = min(dplyr::n_distinct(d[[var1]]), breaks1)))
  } else {
    sim_var1 <- unique(d[[var1]])
  }

  sim_var2 <- NULL
  if (!is.null(var2)) {
    if (is.numeric(d[[var2]])) {
      sim_var2 <- quantile(d[[var2]])
    } else {
      sim_var2 <- unique(d[[var2]])
    }
  }

  sim_var3 <- NULL
  if (!is.null(var3)) {
    if (is.numeric(d[[var3]])) {
      sim_var3 <- quantile(d[[var3]])
    } else {
      sim_var3 <- unique(d[[var3]])
    }
  }

  all_vars <- purrr::discard(list(sim_var1, sim_var2, sim_var3), is.null)

  purrr::cross_n(all_vars)
}

combo_handler <- function(rf, d, class, combos, n_cores, var1, var2, var3) {
  foreach(i = seq_along(combos), .combine = dplyr::bind_rows, .inorder = FALSE) %dopar% {
    d[[var1]] <- combos[[i]][[1]]
    if (!is.null(var2))
      d[[var2]] <- combos[[i]][[2]]
    if (!is.null(var3))
      d[[var3]] <- combos[[i]][[3]]
    preds <- predict(rf, newdata = d, type = "prob")
    sim_d <- data.frame(preds = boot::logit(mean(preds[, class])), var1 = combos[[i]][[1]])
    if (!is.null(var2))
      sim_d[[var2]] <- combos[[i]][[2]]
    if (!is.null(var3))
      sim_d[[var3]] <- combos[[i]][[3]]

    names(sim_d) <- c("preds", var1, var2, var3)

    sim_d
  }
}
