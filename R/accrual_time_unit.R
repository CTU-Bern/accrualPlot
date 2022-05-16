#' accrual_time_unit
#'
#' Generates summary of recruitment per time unit
#'
#' @param accrual_df accrual data frame produced by \code{accrual_create_df} with by=NA.
#' @param unit time unit for which the bars should be plotted,
#'	one of \code{"month"}, \code{"year"}, \code{"week"} or \code{"day"}.
#'
#' @return Returns a data frame with the number of patients accrued for each time unit.
#'
#'
#' @import lubridate
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' accrual_time_unit(accrual_df,"week")
#' accrual_time_unit(accrual_df,"day")
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

	#define weeks as starting on Monday
	alltimes<-seq.Date(from=floor_date(start_date,unit=unit,week_start = 1),
                       to=floor_date(current_date,unit=unit,week_start = 1),by=unit)
	dfa<-data.frame(Date=alltimes,week=1:length(alltimes))

	alltimes_day<-seq.Date(from=floor_date(start_date,unit=unit,week_start = 1),
                       to=floor_date(current_date,unit=unit,week_start = 1),by="day")
	dfaj<-merge(data.frame(Date=alltimes_day),dfa,all=TRUE,by="Date")
	for (i in 1:nrow(dfaj)) {
		dfaj[i,"week"]<-ifelse(is.na(dfaj[i,"week"]),dfaj[i-1,"week"],dfaj[i,"week"])
	}
	dfajm<-merge(accrual_df,dfaj,by="Date",all=TRUE)
	dfim<-aggregate(Freq~week,data=dfajm,FUN=sum)
	dfmerge<-merge(dfa,dfim,by="week",all=TRUE)
	dfmerge[is.na(dfmerge$Freq),"Freq"]<-0
	names(dfmerge)[names(dfmerge)=="Date"]<-"date"

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
