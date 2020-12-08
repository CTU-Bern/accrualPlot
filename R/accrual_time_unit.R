#' accrual_time_unit
#'
#' Generates summary of recruitment per time unit
#'
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#' @param unit time unit for which the bars should be plotted, any of "month","year","week","day"
#' @param start_date start_date: date when recruitment started, single character or date,
#		if not given the first enrollment date is used as start_date
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date
#		if not given the latest enrollment date is used for each site
#' @param format_current_date format of the current date, ignored if current_date is a date
#'
#' @return Data frame with the number of patients accrued for each time unit
#' (given by day and/or week and/or month and year).
#'
#'
#' @import lubridate
#'
#' @examples
#' \donttest{
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrualPlot:::accrual_time_unit(accrual_df,"week")
#' accrualPlot:::accrual_time_unit(accrual_df,"day")
#' accrualPlot:::accrual_time_unit(accrual_df,"week",
#'     start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' }
accrual_time_unit<-function(accrual_df,
                            unit=c("month","year","week","day"),
                            start_date=NA,
                            format_start_date="%d%b%Y",
                            current_date=NA,
                            format_current_date="%d%b%Y") {

  unit<-match.arg(unit)

  #add start or end date
  if (!is.na(start_date)) {
    if (inherits(start_date,"Date")) {
      start_date<-start_date
    } else {
      start_date<-as.Date(start_date,format=format_start_date)
    }
  } else {
    start_date<-min(accrual_df$Date)
  }


  if (!is.na(current_date)) {
    if (inherits(current_date,"Date")) {
      current_date<-current_date
    } else {
      current_date<-as.Date(current_date,format=format_current_date)
    }
  } else {
    current_date<-max(accrual_df$Date)
  }

  if (unit=="year") {
    dfim <- aggregate(cbind(Freq)~year(Date),data=accrual_df,FUN=sum)
    colnames(dfim)<-c("year","Freq")
    alltimes<-seq.Date(from=floor_date(start_date,unit=unit),
                       to=floor_date(current_date,unit=unit),by=unit)
    dfall <-data.frame(year = year(alltimes))
    dfmerge<-merge(dfall,dfim,sort=FALSE,all=TRUE)
    dfmerge<-dfmerge[order(dfmerge$year),]
    dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
    dfmerge<-within(dfmerge,date<-as.Date(paste(year,1,1, sep="-"), "%Y-%m-%d"))
  }

  if (unit=="month") {
    dfim <- aggregate(cbind(Freq)~month(Date) + year(Date),data=accrual_df,FUN=sum)
    colnames(dfim)<-c("month","year","Freq")
    alltimes<-seq.Date(from=floor_date(start_date,unit=unit),
                       to=floor_date(current_date,unit=unit),by=unit)
    dfall <-data.frame(month=month(alltimes),year = year(alltimes))
    dfmerge<-merge(dfall,dfim,sort=FALSE,all=TRUE)
    dfmerge<-dfmerge[order(dfmerge$year,dfmerge$month),]
    dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
    dfmerge<-within(dfmerge,date<-as.Date(paste(year,month,1, sep="-"), "%Y-%m-%d"))
  }

  if (unit=="week") {

    #beginning of the week:
    lastmon <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
    accrual_df$Dates<-lastmon(accrual_df$Date)
    dfim <- aggregate(cbind(Freq)~ week(Dates) + month(Dates) + year(Dates),data=accrual_df,FUN=sum)
    colnames(dfim)<-c("week","month","year","Freq")
    alltimes<-seq.Date(from=floor_date(min(accrual_df$Dates,start_date),unit=unit,week_start = 1),
                       to=floor_date(current_date,unit=unit,week_start = 1),by=unit)
    dfall <-data.frame(week=week(alltimes),month=month(alltimes),year = year(alltimes))
    dfmerge<-merge(dfall,dfim,sort=FALSE,all=TRUE)
    dfmerge<-dfmerge[order(dfmerge$year,dfmerge$month,dfmerge$week),]
    dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
    dfmerge$date<-with(dfmerge,as.Date(paste(year,month,week, 1, sep="-"), "%Y-%m-%U-%u"))
  }

  if (unit=="day") {
    dfim <- aggregate(cbind(Freq)~day(Date) + month(Date) + year(Date),data=accrual_df,FUN=sum)
    colnames(dfim)<-c("day","month","year","Freq")
    alltimes<-seq.Date(from=floor_date(start_date,unit=unit),
                       to=floor_date(current_date,unit=unit),by=unit)
    dfall <-data.frame(day=day(alltimes),month=month(alltimes),year = year(alltimes))
    dfmerge<-merge(dfall,dfim,sort=FALSE,all=TRUE)
    dfmerge<-dfmerge[order(dfmerge$year,dfmerge$month,dfmerge$day),]
    dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
    dfmerge<-within(dfmerge,date<-as.Date(paste(year,month,day, sep="-"), "%Y-%m-%d"))
  }

  return(dfmerge)
}
