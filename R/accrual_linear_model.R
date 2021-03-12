#' accrual_linear_model
#'
#' Creates weighted linear regression model based on an accrual data frame producd by accrual_create_df.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df (optionally with by option as a list)
#' @param start_date date when recruitment started,
#'		single character/date, or a vector with the same length as accrual_df,
#		if not given the first enrollment date is used
# @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze,
#'		single character/date, or a vector with the same length as accrual_df,
#		if not given the latest enrollment date is used
# @param format_current_date format of the current date, ignored if current_date is a date
#' @param fill_up whether to fill up days where no recruitment was observed,
#		otherwise these points do not contribute to the regression, default is TRUE
#' @param wfun function to calculate the weights based on the accrual data frame, default is
#		wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
#'
#' @return A lm object of a weigthed linear regression of cumulative accrual on dates.
#'
#' @export
#'
#' @importFrom stats lm aggregate
#'
#' @examples
#' \donttest{
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_linear_model(accrual_df)
#'
#' #unweighted
#' accrual_linear_model(accrual_df, wfun=function(x) rep(1,nrow(x)))
#'
#' #different start and current date
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_linear_model(accrual_df,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' #or
#' accrual_df<-accrual_create_df(enrollment_dates,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' accrual_linear_model(accrual_df)
#'
#'  #accrual_df with by option
#'  set.seed(2020)
#'	centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#'  accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#'	accrual_linear_model(accrual_df)
#'	accrual_linear_model(accrual_df, start_date = as.Date("2017-12-25"))
#'	accrual_linear_model(accrual_df, start_date = as.Date("2017-12-25"),
#'	  current_date = as.Date("2018-02-05"))
#' }

accrual_linear_model <- function(accrual_df,
                                 start_date=NA,
                                 # format_start_date="%d%b%Y",
                                 current_date=NA,
                                 # format_current_date="%d%b%Y",
                                 fill_up=TRUE,
                                 wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))) {

  # fill_up<-match.arg(fill_up)
  if (!is.na(start_date)) check_date(start_date)
  if (!is.na(current_date)) check_date(current_date)

  if (is.data.frame(accrual_df)) {
	  accrual_df<-list(accrual_df)
  }

  if (length(start_date)==1) {
	start_date<-rep(start_date,length(accrual_df))
  }
  stopifnot(length(accrual_df)==length(start_date))

  if (length(current_date)==1) {
	current_date<-rep(current_date,length(accrual_df))
  }
  stopifnot(length(accrual_df)==length(current_date))

  lmi<-numeric(0)
  for (i in 1:length(accrual_df)) {
	accrual_dfi<-accrual_df[[i]]

    #add start or current date
    if (!is.na(start_date[i])) {
      if (start_date[i] != min(accrual_dfi$Date)) {
        stopifnot(start_date < min(accrual_dfi$Date))
        accrual_dfi<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),accrual_dfi)
      }
    }

    if (!is.na(current_date[i])) {
      if (current_date[i] != max(accrual_dfi$Date)) {
        stopifnot(current_date > max(accrual_dfi$Date))
        accrual_dfi<-rbind(accrual_dfi,data.frame(Date=current_date,Freq=0,Cumulative=max(accrual_dfi$Cumulative)))
      }
    }

    #fill up days:
    if (fill_up) {
      alldays<-seq(min(accrual_dfi$Date),max(accrual_dfi$Date),by=1)
      alldays<-alldays[!(alldays %in% accrual_dfi$Date)]
      if (!is.null(nrow(alldays))) {
        alldays_df<-data.frame(Date=alldays,Freq=0,Cumulative=NA)
        adf<-rbind(accrual_dfi,alldays_df)
        adf<-adf[order(adf$Date),]
        stopifnot(cumsum(adf$Fre)[!is.na(adf$Cumulative)]==adf$Cumulative[!is.na(adf$Cumulative)])
        adf$Cumulative<-cumsum(adf$Fre)
        accrual_dfi<-adf
      }
    }

    #linear model:
    accrual_dfi<-aggregate(cbind(Freq,Cumulative)~Date,data=accrual_dfi,FUN=sum)
    weivec <- wfun(accrual_dfi)
    stopifnot( length(weivec) == nrow(accrual_dfi) )
    lmi<-append(lmi,list(lm(Cumulative ~ Date, data=accrual_dfi, weights = weivec)))
  }

  if (length(lmi)==1) {
	return(lmi[[1]])
  } else {
	names(lmi)<-names(accrual_df)
	return(lmi)
  }
}
