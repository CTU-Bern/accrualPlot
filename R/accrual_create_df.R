#' accrual_create_df
#'
#' Creates a data frame that contains the absolute and cumululative number of patients
#' recruited at each date from a vector with enrollment dates.
#'
#' @param enrollment_dates dates on which patients are enrolled as date or character vector
#' @param format_enrollment_dates single character identifying the format of the dates,
#' ignored if enrollment_dates are dates
#' @param start_date date when recruitment started, single character or date, if not given the first enrollment date is used
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date, if not given the latest enrollment date is used
#' @param format_current_date format of the current date, ignored if current_date is a date
#' @param force_start0 adds an extra 0 line to the accrual data frame in cases where a start date is given and
#' corresponds to the earliest enrollment date
#' @param by vector with centers, has to have the same length as enrollment dates,
#' generates a list with accrual data frames for each site
#' @param overall indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#'
#' @return Returns a data frame (or a list of data frames if by is not NA)
#' with three columns "Date", "Freq" and "Cumulative" with each
#' date with an accural and the absolute and cumulative number of patients accrued.
#' @export
#' @examples
#' \donttest{
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_create_df(enrollment_dates)
#' # different start and current date
#' accrual_create_df(enrollment_dates,start_date=as.Date("2017-12-01"),
#' current_date=as.Date("2018-03-01"))
#' #by site
#' set.seed(2020)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_create_df(enrollment_dates,by=centers)
#' }
accrual_create_df <- function(enrollment_dates,
                              format_enrollment_dates="%d%b%Y",
                              start_date=NA,
                              format_start_date="%d%b%Y",
                              current_date=NA,
                              format_current_date="%d%b%Y",
                              force_start0=c("no","yes"),
                              by=NA,
                              overall=TRUE,
                              name_overall="Overall") {

  force_start0<-match.arg(force_start0)

  if (inherits(enrollment_dates,"Date")) {
    format_enrollment_dates<-"%Y-%m-%d"
  }

  if (sum(!is.na(by))==0) {
    nc<-1
    byt<-0
  } else {
    lc<-unique(by)
    nc<-length(unique(by))
    if (overall==TRUE) {
      nc<-nc+1
    }
    byt<-1
  }

  accrual_df<-numeric(0)

  for (i in 1:nc) {

    if (byt==0) {
      ed<-enrollment_dates
    } else {
      if (overall==TRUE & i==nc) {
        ed<-enrollment_dates
      } else {
        ed<-enrollment_dates[by==lc[i]]
      }
    }

    adf <- data.frame(table(ed))
    colnames(adf) <- c("Date", "Freq")
    adf$Date <- as.Date(adf$Date,format=format_enrollment_dates)
    adf<-adf[order(adf$Date),]
    adf$Cumulative <- cumsum(adf$Freq)

    if (!is.na(start_date)) {
      if (inherits(start_date,"Date")) {
        sdate<-start_date
      } else {
        sdate<-as.Date(start_date,format=format_start_date)
      }
      if (sdate != min(adf$Date) | force_start0=="yes")  {
        stopifnot(sdate <= min(adf$Date))
        adf<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),adf)
      }
    }

    if (!is.na(current_date)) {
      if (inherits(current_date,"Date")) {
        end_date<-current_date
      } else {
        end_date<-as.Date(current_date,format=format_current_date)
      }
      if (end_date != max(adf$Date)) {
        stopifnot(end_date > max(adf$Date))
        adf<-rbind(adf,data.frame(Date=end_date,Freq=0,Cumulative=max(adf$Cumulative)))
      }
    }

    if (byt==0) {
      accrual_df<-adf
    } else {
      accrual_df<-append(accrual_df,list(adf))
      if (overall==TRUE & i==nc) {
        names(accrual_df)[i]<-name_overall
      } else {
        names(accrual_df)[i]<-lc[i]
      }
    }
  }
  return(accrual_df)
}