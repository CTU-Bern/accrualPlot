#' accrual_create_df
#'
#' Creates a data frame that contains the absolute and cumululative number of patients
#' recruited at each date from a vector with enrollment dates.
#'
#' @param enrollment_dates dates on which patients are enrolled as date or character vector
#' @param start_date date when recruitment started, single character or date, if not given the first enrollment date is used
#' @param current_date date of the data export or database freeze, single character or date, if not given the latest enrollment date is used
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
                              start_date=NA,
                              current_date=NA,
                              force_start0=TRUE,
                              by=NA,
                              overall=TRUE,
                              name_overall="Overall") {


  check_date(enrollment_dates)
  if (!is.na(start_date)) check_date(start_date)
  if (!is.na(current_date)) check_date(current_date)

  if (sum(!is.na(by))==0) {
    nc<-1
    byt<-0
  } else {
    if (is.factor(by)) {
	  lc<-levels(by)
	} else {
	  lc<-unique(by)
	}

    nc<-length(lc)
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
    adf$Date <- as.Date(as.character(adf$Date))
    adf<-adf[order(adf$Date),]
    adf$Cumulative <- cumsum(adf$Freq)

    if (!is.na(start_date)) {
      if (start_date != min(adf$Date) | force_start0)  {
        stopifnot(start_date <= min(adf$Date))
        adf<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),adf)
      }
    }

    if (!is.na(current_date)) {
      if (current_date != max(adf$Date)) {
        stopifnot(current_date > max(adf$Date))
        adf<-rbind(adf,data.frame(Date=current_date,Freq=0,Cumulative=max(adf$Cumulative)))
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
  class(accrual_df) <- c("accrual_df", class(accrual_df))
  return(accrual_df)
}
