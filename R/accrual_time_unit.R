#' accrual_time_unit
#'
#' Generates summary of recruitment per time unit
#'
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#' @param unit time unit for which the bars should be plotted, any of "month","year","week","day"
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
#' }
#'
accrual_time_unit<-function(accrual_df,unit=c("month","year","week","day")) {

  unit<-match.arg(unit)

  #start or end date
  start_date<-min(accrual_df$Date)
  current_date<-max(accrual_df$Date)


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
    #lastmon <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
    #accrual_df$Dates<-lastmon(accrual_df$Date)
	#dfim <- aggregate(cbind(Freq)~ week(Dates) + month(Dates) + year(Dates),data=accrual_df,FUN=sum)
	dfim <- aggregate(cbind(Freq)~ week(Date) + month(Date) + year(Date),data=accrual_df,FUN=sum)
    colnames(dfim)<-c("week","month","year","Freq") 
	alltimes<-seq.Date(from=floor_date(start_date,unit=unit,week_start = 1),
                       to=floor_date(current_date,unit=unit,week_start = 1),by=unit)				   
    dfall <-data.frame(week=week(alltimes),month=month(alltimes),year = year(alltimes))
    dfmerge<-merge(dfall,dfim,sort=FALSE,all=TRUE)
    dfmerge<-dfmerge[order(dfmerge$year,dfmerge$month,dfmerge$week),]
    dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
    dfmerge$date<-with(dfmerge,as.Date(paste(dfmerge$year,dfmerge$week, 1, sep="-"), "%Y-%W-%u"))	
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
