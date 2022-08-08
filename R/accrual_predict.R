#' accrual_predict
#'
#' accrual_predict
#'
#' Prediction of end date based on an accrual data frame produced by \code{accrual_create_df},
#' a fitted regression model produced by accrual_linear_model and a target sample size.
#'
#' @param accrual_df accrual data frame produced by \code{accrual_create_df} (optionally with by option as a list)
#' @param accrual_fit linear model produced by accrual_linear_model, can be a list with the same length as accrual_df
#' @param target target sample size or date to predict end date or expected sample size, respectively.
#' 	A single number or date, or a named vector with the same length as accrual_df (to add site-specific targets).
#'
#' @return Returns the predicted end date(s) or the predicted sample size(s).
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' accrual_model<-accrual_linear_model(accrual_df)
#' #predict date for a specific n
#' accrual_predict(accrual_df,accrual_model,target=300)
#' #predict n at a specific date
#' accrual_predict(accrual_df,accrual_model,target=as.Date("2020-11-01"))
#'
#' #different start and current date
#' accrual_df<-accrual_create_df(accrualdemo$date,start_date=as.Date("2020-07-09"),
#'     current_date=as.Date("2020-10-15"))
#' accrual_model<-accrual_linear_model(accrual_df)
#' accrual_predict(accrual_df,accrual_model,target=300)
#'
#'  #accrual_df with by option
#'  accrual_df<-accrual_create_df(accrualdemo$date,by=accrualdemo$site)
#'	accrual_model<-accrual_linear_model(accrual_df)
#'	accrual_predict(accrual_df,accrual_model,
#'	  target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300))
#'	accrual_predict(accrual_df,accrual_model,target=as.Date("2020-11-01"))
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

  if (length(target)!=1) {
	if (length(target)!=length(accrual_df)) {
	  stop("length of target has to correspond to length of accrual_df")
	} else {
	  target<-check_name(target, names(accrual_df))
	}
  } else {
	target<-rep(target,length(accrual_df))
  }
	
  preddate<-TRUE
  if (is.Date(target)) {
	preddate<-FALSE
	check_date(target)
  }

  prend<-numeric(0)
  for (i in 1:length(accrual_df)) {
    accrual_dfi<-accrual_df[[i]]
	accrual_fiti<-accrual_fit[[i]]
    if (preddate) {
	  nr<-target[i]-max(accrual_dfi$Cumulative)
	  diffdays<-nr/as.numeric(accrual_fiti$coef[2])
	  predi<-max(accrual_dfi$Date) + ceiling(diffdays)
	} else {
	  nd<-difftime(target[i],max(accrual_dfi$Date),units="day")
	  diffn<-as.numeric(nd)*as.numeric(accrual_fiti$coef[2])
	  predi<-max(accrual_dfi$Cumulative)+diffn
	}
	prend<-append(prend,list(predi))
  }

  if (length(prend)==1) {
	return(prend[[1]])
  } else {
	names(prend)<-names(accrual_df)
	return(prend)
  }
}
