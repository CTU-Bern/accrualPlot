#' Plot method for accrual_dfs (as created by accrual_create_df)
#'
#' @param x accrual_df object
#' @param which one of \code{"cumulative"}, \code{"absolute"} or \code{"predict"}.
#' Abbreviations are allowed.
#' @param engine string to indicate the plotting engine (base/graphics or ggplot2)
#' @param ... options passed to other functions
#'
#' @return a plot
#' @export
#' @seealso \link{accrual_plot_abs} \link{accrual_plot_cum} \link{accrual_plot_predict}
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' plot(accrual_df)
#' plot(accrual_df, "abs")
#' plot(accrual_df, "pred", target = 50)

plot.accrual_df <- function(x
                            , which = "cum"
                            , engine = c("base", "ggplot2")
                            , ...
                            ){

  # which <- switch(as.character(which),
  #                 cumulative = "c",
  #                 )

  which <- match.arg(which, c("cumulative", "absolute", "predict"))
  engine <- match.arg(engine)

  if(engine == "base"){
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
  if(engine == "ggplot2"){
    if(which == "cumulative"){
      out <- gg_accrual_plot_cum(x, ...)
    }
    if(which == "absolute"){
      out <- gg_accrual_plot_abs(x, ...)
    }
    if(which == "predict"){
      out <- gg_accrual_plot_predict(x, ...)
    }
    return(out)
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
#' @param head show header of the accrual data?
#' @param ... arguments passed to head
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' print(accrual_df)
#' # only show text
#' print(accrual_df, head = FALSE)
#' # show first 15 days
#' print(accrual_df, n = 15)
print.accrual_df <- function(x, head = TRUE, ...){
  mindate <- min(x$Date)
  maxdate <- max(x$Date)
  maxc <- max(x$Cumulative)

  cat(paste(maxc, "participants recruited between", mindate, "and", maxdate, "\n"))

  if(head) print(head(as.data.frame(x), ...))

}

#' @rdname print.accrual_df
#' @name print.accrual_list
#' @export
print.accrual_list <- function(x, ...){
  # w <- which(names(x) == attr(x, "name_overall"))
  # if(length(w) > 0) y <- x[-w]
  # ncenter <- length(y)
  # npart <- sum(sapply(y, function(x) max(x$Cumulative)))
  # maxdate <- max(do.call("c", lapply(y, function(x) max(x$Date))))
  # mindate <- min(do.call("c", lapply(y, function(x) min(x$Date))))

  # cat(paste(npart, "participants recruited between", mindate, "and", maxdate, "from", ncenter, "centers\n"))

  lapply(x, function(y) print(y, ...))
  return(invisible(NULL))
}

