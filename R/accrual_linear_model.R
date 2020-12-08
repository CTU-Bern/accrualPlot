#' accrual_linear_model
#'
#' Creates weighted linear regression model based on an accrual data frame producd by accrual_create_df.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#' @param start_date date when recruitment started, single character or date,
#		if not given the first enrollment date is used
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date,
#		if not given the latest enrollment date is used
#' @param format_current_date format of the current date, ignored if current_date is a date
#' @param fill_up whether to fill up days where no recruitment was observed,
#		otherwise these points do not contribute to the regression, default is yes
#' @param wfun function to calculate the weights based on the accrual data frame, default is
#		wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
#'
#' @return A lm object of a weigthed linear regression of cumulative accrual on dates.
#'
#' @export
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
#' }

accrual_linear_model <- function(accrual_df,
                                 start_date=NA,
                                 format_start_date="%d%b%Y",
                                 current_date=NA,
                                 format_current_date="%d%b%Y",
                                 fill_up=c("yes","no"),
                                 wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))) {

  fill_up<-match.arg(fill_up)

  #add start or current date
  if (!is.na(start_date)) {
    if (inherits(start_date,"Date")) {
      sdate<-start_date
    } else {
      sdate<-as.Date(start_date,format=format_start_date)
    }
    if (sdate != min(accrual_df$Date)) {
      stopifnot(sdate < min(accrual_df$Date))
      accrual_df<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),accrual_df)
    }
  }

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

  #fill up days:
  if (fill_up=="yes") {
    alldays<-seq(min(accrual_df$Date),max(accrual_df$Date),by=1)
    alldays<-alldays[!(alldays %in% accrual_df$Date)]
    if (!is.null(nrow(alldays))) {
      alldays_df<-data.frame(Date=alldays,Freq=0,Cumulative=NA)
      adf<-rbind(accrual_df,alldays_df)
      adf<-adf[order(adf$Date),]
      stopifnot(cumsum(adf$Fre)[!is.na(adf$Cumulative)]==adf$Cumulative[!is.na(adf$Cumulative)])
      adf$Cumulative<-cumsum(adf$Fre)
      accrual_df<-adf
    }
  }

  #linear model:
  accrual_df<-aggregate(cbind(Freq,Cumulative)~Date,data=accrual_df,FUN=sum)
  weivec <- wfun(accrual_df)
  stopifnot( length(weivec) == nrow(accrual_df) )
  lm(Cumulative ~ Date, data=accrual_df, weights = weivec)
}
