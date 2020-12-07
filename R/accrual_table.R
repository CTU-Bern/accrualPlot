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
#' @param start_date start_date: date when recruitment started, single character or date,
#	  or "common" if the same date should be used for all sites,
#		if not given the first enrollment date is used as start_date
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date
#	  or "common" if the same date should be used for all sites,
#		if not given the latest enrollment date is used for each site
#' @param format_current_date format of the current date, ignored if current_date is a date
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
#' accrual_table(accrual_df,unit="day",start_date="common",current_date="common")
#' accrual_table(accrual_df,unit="day",start_date=as.Date("2017-12-31"),
#'     current_date=as.Date("2018-03-01"))
#'
#'
accrual_table<-function(accrual_df,
                        overall=TRUE,
                        name_overall="Overall",
                        pos_overall=c("last","first"),
                        start_date=NA,
                        format_start_date="%d%b%Y",
                        current_date=NA,
                        format_current_date="%d%b%Y",
                        unit=c("month","year","week","day"),
                        format_table_date="%d%b%Y",
                        format_time="%1.0f",
                        format_rrate="%1.2f",
                        header=TRUE) {


  unit<-match.arg(unit)
  pos_overall<-match.arg(pos_overall)

  scales<-data.frame(unit=c("year","month","week","day"),scale=c(365,30,7,1),name=c("Years","Months","Weaks","Days"))
  scale<-scales$scale[scales$unit==unit]
  name<-scales$name[scales$unit==unit]

  if (is.data.frame(accrual_df)) {
    accrual_df<-list(accrual_df)
    overall<-FALSE
    nhead<-4
  } else {
    nhead<-5
    stopifnot((is.data.frame(accrual_df[[1]])))
  }
  lc<-length(accrual_df)

  if (!is.na(start_date)) {
    if (inherits(start_date,"Date")) {
      sdate<-start_date
    } else {
      if (start_date=="common") {
        sdate<-min(do.call("c",lapply(accrual_df,function(x) min(x$Date))))
      } else {
        sdate<-as.Date(start_date,format=format_start_date)
      }
    }
  }

  if (!is.na(current_date)) {
    if (inherits(current_date,"Date")) {
      end_date<-current_date
    } else {
      if (current_date=="common") {
        end_date<-max(do.call("c",lapply(accrual_df,function(x) max(x$Date))))
      } else {
        end_date<-as.Date(current_date,format=format_current_date)
      }
    }
  }

  tab<-numeric(0)

  for (i in 1:lc) {
    nrec<-max(accrual_df[[i]]$Cumulative)

    si<-min(accrual_df[[i]]$Date[accrual_df[[i]]$Cumulative>0])
    if (is.na(start_date)) {
      sdate<-si
    }

    ei<-max(accrual_df[[i]]$Date[accrual_df[[i]]$Cumulative==max(accrual_df[[i]]$Cumulative)])
    if (is.na(current_date)) {
      end_date<-ei
    }

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
    tab<-tab[tab$name!=name_overall,]
  }

  if (!is.null(header)) {
    if (length(header)==1) {
      if (header==TRUE) {
        head<-c("Center","First patient in",paste0(name," accruing"),
                "Patients accrued",
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

