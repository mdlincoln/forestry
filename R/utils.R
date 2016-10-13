rf_data <- function(x) {

  if (is.null(x$call$data)) {
    list <- lapply(all.vars(x$call), as.name)
    data <- eval(as.call(list(quote(data.frame),list)), parent.frame())
  } else {
    data <- eval(x$call$data, parent.frame())
  }

  if (is.null(data))
    stop("Original model data cannot be recovered. Please supply origial data when calling survey_forest()")

  return(data)
}
