#' Plot method for accrual data frames produced by \code{accrual_create_df}
#'
#' @param x object of class 'accrual_df' or 'accrual_list' produced by accrual_create_df.
#' @param which one of \code{"cumulative"}, \code{"absolute"} or \code{"predict"}.
#'  Abbreviations are allowed.
#' @param engine string to indicate the plotting engine (base/graphics or ggplot2)
#' @param ... options passed to other functions
#'
#' @return A plot with cumulative or absolute accrual, or accrual prediction.
#' @export
#' @seealso [accrual_plot_abs()], [accrual_plot_cum()] and [accrual_plot_predict()]
#' @examples
#' data(accrualdemo)
#' accrual_df <- accrual_create_df(accrualdemo$date)
#' plot(accrual_df)
#' plot(accrual_df, "abs", unit="week")
#' plot(accrual_df, "pred", target = 300)
#' plot(accrual_df, "pred", target = 300, engine = "ggplot")

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
#' @param object object of class 'accrual_df' or 'accrual_list' produced by accrual_create_df.
#' @param ... options passed to other functions
#'
#' @return Returns data frame with a header, a row per site and overall and the following columns:
#' \item{name}{name of the site (if accrual_df is a list)}
#' \item{start_date}{accrual start date}
#' \item{time}{time accruing}
#' \item{n}{number of patients accrued}
#' \item{rate}{accrual rate per time unit}
#'
#' @export
#'
#' @examples
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date, accrualdemo$site)
#' summary(accrual_df)
summary.accrual_df <- function(object, ...){

  accrual_table(object, ...)

}


#' Print methods for accrual objects
#'
#' @rdname print.accrual_df
#' @param x object of class 'accrual_df' or 'accrual_list' produced by accrual_create_df.
#' @param head show header of the accrual data?
#' @param ... arguments passed to head
#'
#' @return No return value
#'
#' @export
#'
#' @examples
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
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

  return(invisible(NULL))

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

  lapply(names(x), function(y) {
		cat(paste0(y,":\n"))
		print(x[[y]], ...)
		cat("\n")
	})
  return(invisible(NULL))
}


#' as.data.frame method for accural_list objects
#'
#' @param x accrual_list
#' @param ... for consistency with other \code{as.data.frame} methods (not used)
#'
#' @note methods from within the package will not work on the output from this function.
#'
#' @export
#' @examples
#' data(accrualdemo)
#' x <- accrual_create_df(accrualdemo$date, accrualdemo$site)
#' as.data.frame(x)
#'
as.data.frame.accrual_list <- function(x, ...){

  tmp <- do.call(rbind,
                 mapply(function(d, s){
                   d$site <- s
                   class(d) <- "data.frame"
                   d
                 }, x, names(x), SIMPLIFY = FALSE))
  row.names(tmp) <- NULL
  return(tmp)

}


