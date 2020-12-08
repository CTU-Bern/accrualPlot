#' accrual_predict
#'
#' Prediction of end date based on an accrual data frame produced by accrual_create_df,
#' a fitted regression model produced by accrual_linear_model and a target sample size.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#' @param accrual_fit linear model produced by accrual_linear_model
#' @param target target sample size, single number
#' @param current_date date of the data export or database freeze, single character or date,
#		if not given the latest enrollment date is used
#' @param format_current_date format of the current date, ignored if current_date is a date
#'
#' @return A single date, the predicted end date.
#'
#'
#' @examples
#' \donttest{
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_model<-accrual_linear_model(accrual_df)
#' accrualPlot:::accrual_predict(accrual_df,accrual_model,target=100)
#'
#' #different start and current date
#' accrual_df<-accrual_create_df(enrollment_dates,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' accrual_model<-accrual_linear_model(accrual_df)
#' accrual_plot_predict(accrual_df,accrual_model,target=100)
#' #or
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_model<-accrual_linear_model(accrual_df,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' accrualPlot:::accrual_predict(accrual_df,accrual_model,target=100,
#'     current_date=as.Date("2018-03-01"))
#' }

accrual_predict <- function(accrual_df,
                            accrual_fit,
                            target,
                            current_date=NA,
                            format_current_date="%d%b%Y") {

  if (!is.na(current_date)) {
    if (inherits(current_date,"Date")) {
      end_date<-current_date
    } else {
      end_date<-as.Date(current_date,format=format_current_date)
    }
    if (end_date != max(accrual_df$Date)) {
      stopifnot(end_date > max(accrual_df$Date))
      accrual_df<-rbind(accrual_df,data.frame(Date=end_date,Freq=0,Cumulative=max(accrual_df$Cumulative)))
    }
  }
  nr<-target-max(accrual_df$Cumulative)
  diffdays<-nr/as.numeric(accrual_fit$coef[2])
  max(accrual_df$Date) + ceiling(diffdays)
}
