#' accrual_table
#'
#'Table of recruitment overview by site,
#'including name of site, start date, time recruiting, number of patients recruited and
#'rate of recruitment
#'
#' @param accrual_df  accrual data frame produced by accrual_create_df potentially with by option (i.e. as a list)
#	  with by option, a line is added for each element in the list
#' @param overall logical, indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#' @param pos_overall overall in last or first row (if by is not NA and overall==TRUE)
#' @param unit time unit for time recruiting and the rate, any of "month","year","week","day"
#' @param format_table_date format of start date in table
#' @param format_time format of time recruiting in table
#' @param format_rrate format of recruitment rate in table
#' @param header header: include header, TRUE, NULL or character vector of length 5
#'
#' @return Data frame with name of the site, accrual start date, time accruing,
#'  number of patients accrued and accrual rate.
#'
#' @export
#'
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' accrual_table(accrual_df)
#'
#' #format
#' accrual_table(accrual_df,format_time="%1.1f",format_rrate="%1.1f")
#'
#' #unit
#' accrual_table(accrual_df,unit="day")
#'
#' #common start and current dates
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers,start_date="common",current_date="common")
#' accrual_table(accrual_df)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers,start_date=as.Date("2017-12-31"),
#'     current_date=as.Date("2018-03-01"))
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
    }
  }

  return(tab)
}
