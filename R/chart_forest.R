#' Create plot of variable influence in a random forest model
#'
#' Specify which variables to compare in the model.
#'
#' @param sim_data Data returned from \link{simulate_data}
#' @param log_var1 Log-scale the x axis?
#'
#' @export
chart_forest <- function(sim_data, log_var1 = TRUE) {
  #stopifnot(inherits(sim_data, "lumberjackData"))

  if (attr(sim_data, "n_vars") == 1) {
    var1 <- names(sim_data)[1]
    var2 <- NULL
  } else {
    var1 <- names(sim_data)[1]
    var2 <- names(sim_data)[2]
  }

  p <- ggplot(sim_data, aes_(x = as.name(var1), y = ~preds)) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw(base_size = 18) +
    labs(y = "Probability of falling to selected class")

  if (log_var1)
    p <- p + scale_x_log10(labels = scales::comma)

  if (is.null(var2)) {
    p <- p + geom_line(size = 1)
  } else {
    p <- p + geom_line(aes_(color = as.name(var2)), size = 1)
  }

  p
}

#' Simulate data for partial dependence plotting
#'
#' @param rf random forest
#' @param data The data frame used to train the original forest
#' @param class Which class to plot
#' @param var1 (Required) Primary variable (preferably continuous)
#' @param breaks1 How many values of the primary variable should be sampled when
#'   calculating partial dependence?
#' @param var2 Secondary variable
#' @param breaks2 If \code{var2} is numeric, in to how many categories should it
#'   be cut?
#' @param shiny_session If a Shiny \link[shiny]{session} object is passed, a
#'   progress bar will be displayed while simulating new data.
#'
#' @export
simulate_data <- function(rf, d, class, var1, breaks1 = 50, var2 = NULL, breaks2 = 5, shiny_session = NULL, ...) {
  if (is.null(var2)) {
    sd <- simulate_data1(rf, d, class, var1, breaks1, shiny_session = shiny_session, ...)
    attr(sd, "n_vars") <- 1
  } else {
    sd <- simulate_data2(rf, d, class, var1, breaks1, var2, breaks2, shiny_session = shiny_session, ...)
    attr(sd, "n_vars") <- 2
  }
  class(sd) <- c(class(sd), "lumberjackData")
  sd
}

simulate_data1 <- function(rf, d, class, var1, breaks1, shiny_session, ...) {

  sim_var1 <- quantile(d[[var1]], seq(0, 1, length.out = min(dplyr::n_distinct(d[[var1]]), breaks1)))

  if (!is.null(shiny_session)) {
    pb <- Progress$new(shiny_session, min = 1, max = length(sim_var1))
    pb$set(message = "Simulating new data...")
  }

  new_d <- purrr::map_df(sim_var1, function(x) {
    d[[var1]] <- x
    preds <- predict(rf, newdata = d, type = "prob")
    sim_d <- data.frame(var1 = x, preds = mean(preds[, class]))
    names(sim_d)[1] <- var1
    if (exists("pb"))
      pb$inc(amount = 1)
    sim_d
  })

  if (exists("pb"))
    pb$close()
  new_d
}

simulate_data2 <- function(rf, d, class, var1, breaks1, var2, breaks2, shiny_session) {
  sim_var1 <- quantile(d[[var1]], seq(0, 1, length.out = min(dplyr::n_distinct(d[[var1]]), breaks1)))

  if (is.numeric(d[[var2]])) {
    sim_var2 <- quantile(d[[var2]])
  } else {
    sim_var2 <- unique(d[[var2]])
  }

  combos <- purrr::cross2(sim_var1, unique(sim_var2))
  print(length(combos))

  if (!is.null(shiny_session)) {
    pb <- Progress$new(shiny_session, min = 0, max = 1)
    pb$set(message = "Simulating new data...")
  }

  new_d <- purrr::map_df(combos, function(x) {
    d[[var1]] <- x[[1]]
    d[[var2]] <- x[[2]]
    preds <- predict(rf, newdata = d, type = "prob")
    sim_d <- data.frame(var1 = x[[1]], var2 = x[[2]], preds = mean(preds[, class]))
    names(sim_d)[1:2] <- c(var1, var2)
    if (exists("pb"))
      pb$inc(amount = 1/length(combos))

    sim_d
  })

  new_d[[var2]] <- as.factor(new_d[[var2]])

  if (exists("pb"))
    pb$close()

  new_d
}
