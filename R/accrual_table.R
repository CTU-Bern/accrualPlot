#' accrual_table
#'
#'Table of recruitment overview by site,
#	including name of site, start date, time recruiting, number of patients recruited and
#'	rate of recruitment
#'
#' @param accrual_df object of class 'accrual_df' or 'accrual_list' produced by \code{accrual_create_df}.
#' @param overall logical, indicates that accrual_df contains a summary with all sites (only if by is not NA).
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE).
#' @param pos_overall overall in last or first row (if by is not NA and overall==TRUE).
#' @param unit time unit for time recruiting and the rate,
#'	one of \code{"month"}, \code{"year"}, \code{"week"} or \code{"day"}.
#' @param format_table_date format of start date in table.
#' @param format_time format of time recruiting in table.
#' @param format_rrate format of recruitment rate in table.
#' @param header include header, logical or character vector of length 4 or 5 (if accrual_df is a list).
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
#' accrual_df<-accrual_create_df(accrualdemo$date,by=accrualdemo$site)
#' accrual_table(accrual_df)
#'
#' #format
#' accrual_table(accrual_df,format_time="%1.1f",format_rrate="%1.1f")
#'
#' #unit
#' accrual_table(accrual_df,unit="day")
#'
#' #common start and current dates
#' accrual_df<-accrual_create_df(accrualdemo$date,by=accrualdemo$site,start_date="common",
#'	current_date="common")
#' accrual_table(accrual_df)
#' accrual_df<-accrual_create_df(accrualdemo$date,by=accrualdemo$site,start_date=as.Date("2020-07-09"),
#'     current_date=as.Date("2020-10-15"))
#' accrual_table(accrual_df)
#'
#'
accrual_table<-function(accrual_df,
                        overall=TRUE,
                        name_overall="Overall",
                        pos_overall=c("last","first"),
                        unit=c("month","year","week","day"),
                        format_table_date="%d%b%Y",
                        format_time="%1.0f",
                        format_rrate="%1.2f",
                        header=TRUE) {


  unit<-match.arg(unit)
  pos_overall<-match.arg(pos_overall)

  scales<-data.frame(unit=c("year","month","week","day"),scale=c(365,30,7,1),name=c("Years","Months","Weeks","Days"))
  scale<-scales$scale[scales$unit==unit]
  uname<-scales$name[scales$unit==unit]

  if (is.data.frame(accrual_df)) {
    accrual_df<-list(accrual_df)
    overall<-FALSE
    nhead<-4
  } else {
    nhead<-5
    stopifnot((is.data.frame(accrual_df[[1]])))
  }
  lc<-length(accrual_df)



  tab<-numeric(0)

  for (i in 1:lc) {
    nrec<-max(accrual_df[[i]]$Cumulative)

    #sdate<-min(accrual_df[[i]]$Date[accrual_df[[i]]$Cumulative>0])
    #end_date<-max(accrual_df[[i]]$Date[accrual_df[[i]]$Cumulative==max(accrual_df[[i]]$Cumulative)])
	sdate<-min(accrual_df[[i]]$Date)
    end_date<-max(accrual_df[[i]]$Date)

    trec<-as.numeric(end_date-sdate)/scale
    if (end_date==sdate) {trec<-0.5/scale}

    rrate<-nrec/trec

    tabi<-c(name=names(accrual_df)[[i]],start_date=format(sdate,format_table_date),
            time=sprintf(format_time,trec),n=nrec,rate=sprintf(format_rrate,rrate))
    tab<-data.frame(rbind(tab,tabi))
  }

  if (overall==TRUE) {
    if (pos_overall=="last") {
      tab<-rbind(tab[tab$name!=name_overall,],tab[tab$name==name_overall,])
    } else {
      tab<-rbind(tab[tab$name==name_overall,],tab[tab$name!=name_overall,])
    }
  } else {
    if (!is.null(tab$name)) {
      tab<-tab[tab$name!=name_overall,]
    }
  }

  if (!is.null(header)) {
    if (length(header)==1) {
      if (header==TRUE) {
        head<-c("Center","First participant in",paste0(uname," accruing"),
                "Participants accrued",
                paste0("Accrual rate (per ",unit,")"))
        if (nhead==4) {
          head<-head[-1]
        }
        tab<-rbind(head,tab)
      }
    } else {
		tab<-rbind(header,tab)
	}
  }
  row.names(tab)<-1:nrow(tab)
  return(tab)
}
