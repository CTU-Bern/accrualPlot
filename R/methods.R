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
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' plot(accrual_df)
#' plot(accrual_df, "abs")
#' plot(accrual_df, "pred", target = 50)

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
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' summary(accrual_df)
summary.accrual_df <- function(object, ...){

  tab <- accrual_table(object, ...)

  print(tab, row.names = FALSE)
}








