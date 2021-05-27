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






#' Print methods for accrual objects
#'
#' @rdname print.accrual_df
#' @param x output from accrual_create_df
#'
#' @return
#' @export
#'
#' @examples
print.accrual_df <- function(x){
  mindate <- min(x$Date)
  maxdate <- max(x$Date)
  maxc <- max(x$Cumulative)

  cat(paste(maxc, "participants recruited between", mindate, "and", maxdate))

}

#' @rdname print.accrual_df
#' @name print.accrual_list
#' @export
print.accrual_list <- function(x){
  w <- which(names(x) == attr(x, "name_overall"))
  if(length(w) > 0) y <- x[-w]
  ncenter <- length(y)
  npart <- sum(sapply(y, function(x) max(x$Cumulative)))
  maxdate <- max(do.call("c", lapply(y, function(x) max(x$Date))))
  mindate <- min(do.call("c", lapply(y, function(x) min(x$Date))))

  cat(paste(npart, "participants recruited between", mindate, "and", maxdate, "from", ncenter, "centers"))
}

