#' accrual_predict
#'
#' accrual_predict
#'
#' Prediction of end date based on an accrual data frame produced by accrual_create_df,
#' a fitted regression model produced by accrual_linear_model and a target sample size.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df (optionally with by option as a list)
#' @param accrual_fit linear model produced by accrual_linear_model, can be a list with the same length as accrual_df
#' @param target target sample size, can be a vector with the same length as accrual_df
#'
#' @return The predicted end date or a list of the predicted end dates
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_model<-accrual_linear_model(accrual_df)
#' accrual_predict(accrual_df,accrual_model,target=100)
#'
#' #different start and current date
#' accrual_df<-accrual_create_df(enrollment_dates,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' accrual_model<-accrual_linear_model(accrual_df)
#' accrual_predict(accrual_df,accrual_model,target=100)
#'
#'  #accrual_df with by option
#'  set.seed(2020)
#'	centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#'  accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#'	accrual_model<-accrual_linear_model(accrual_df)
#'	accrual_predict(accrual_df,accrual_model,target=c(30,30,30,100))
#' }

accrual_predict <- function(accrual_df, accrual_fit, target) {

  if (is.data.frame(accrual_df)) {
	accrual_df<-list(accrual_df)
  }
  if (!is.null(attr(accrual_fit, "class"))) {
	stopifnot(attr(accrual_fit, "class")=="lm")
	accrual_fit<-list(accrual_fit)
  }
  stopifnot(length(accrual_df)==length(accrual_fit))
   
  if (length(target)==1) {
	target<-rep(target,length(accrual_df))
  }
  stopifnot(length(accrual_df)==length(target))	
	
	
  prend<-numeric(0)	
  for (i in 1:length(accrual_df)) {
    accrual_dfi<-accrual_df[[i]]
	accrual_fiti<-accrual_fit[[i]]
    nr<-target[i]-max(accrual_dfi$Cumulative)
    diffdays<-nr/as.numeric(accrual_fiti$coef[2])
    prend<-append(prend,list(max(accrual_dfi$Date) + ceiling(diffdays)))
  }
	
  if (length(prend)==1) {
	return(prend[[1]])
  } else {
	names(prend)<-names(accrual_df)
	return(prend)
  }	
}
