#' Plot method for accrual_dfs (as created by accrual_create_df)
#'
#' @param x accrual_df object
#' @param which one of \code{"cumulative"}, \code{"absolute"} or \code{"predict"}.
#' Abbreviations are allowed.
#' @param ... options passed to other functions
#'
#' @return a plot
#' @export
#'
#' @examples
plot.accrual_df <- function(x, which = "cum", ...){

  # which <- switch(as.character(which),
  #                 cumulative = "c",
  #                 )

  which <- match.arg(which, c("cumulative", "absolute", "predict"))

  if(which == "cumulative"){
    accrual_plot_cum(x, ...)
  }
  if(which == "absolute"){
    accrual_plot_abs(x, ...)
  }
  if(which == "predict"){
    accrual_plot_predict(x, ...)
  }


}


#' Summary method for accrual_dfs (as created by accrual_create_df)
#'
#' @param object accrual_df object
#' @param ... options passed to other functions
#'
#' @return a table
#' @export
#'
#' @examples
summary.accrual_df <- function(object, ...){

  tab <- accrual_table(object, ...)

  print(tab, row.names = FALSE)
}








