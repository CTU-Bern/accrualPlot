#' accrual_create_df
#'
#' Creates a data frame that contains the absolute and cumululative number of patients
#' recruited at each date from a vector with enrollment dates.
#'
#' @param enrollment_dates dates on which patients are enrolled, date vector
#' @param start_date date when recruitment started. Single date (used for all sites in by), 
#'  date vector (with the same length the number of distinct sites in by), 
#'  "common" (first date overall) or "site" (first date for each site, default).
#' @param current_date date of the data export or database freeze. 
#' 	Single date, date vector (with the same length the number of distinct sites in by),
#'  "common" (last date overall, default) or "site" (first date for each site).
#' @param force_start0 logical, adds an extra 0 line to the accrual data frame in cases 
#'  where a start date is given and corresponds to the earliest enrollment date.
#' @param by vector with sites, has to have the same length as enrollment dates,
#' generates a list with accrual data frames for each site
#' @param overall logical indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#'
#' @return Returns a data frame (or a list of data frames if by is not NA)
#' with three columns "Date", "Freq" and "Cumulative" with each
#' date with an accrual and the absolute and cumulative number of patients accrued.
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
                              start_date="site",
                              current_date="common",
                              force_start0=TRUE,
                              by=NA,
                              overall=TRUE,
                              name_overall="Overall") {


  check_date(enrollment_dates)
  if(any(is.na(enrollment_dates))) stop("'enrollment_dates' contains NA values")

   if (sum(!is.na(by))==0) {
    nc<-1; nct<-1; byt<-0
  } else {
    if (is.factor(by)) {lc<-levels(by)} else {lc<-sort(unique(by))}
	nc<-length(lc)
    nct<-ifelse(overall==TRUE,nc+1,nc) 
    byt<-1
  }	
  
  if (!any(start_date[1] %in% c("site","common"))) {
	check_date(start_date)
	check_length(start_date,by)	
	start_date<-mult(start_date,nc)
	if (nct>nc) {start_date<-c(start_date,min(start_date))}	
  } else {
	if(length(start_date)!=1) {
		stop(paste0(start_date," should be of class Date or a single character 'common' or 'site'"))
	}	
	if (start_date=="common") {
		start_date<-rep(min(enrollment_dates),nct)
	} else {
	   start_date<-rep(NA,nct)
    }
  }	
  
 if (!any(current_date[1] %in% c("site","common"))) {
	check_date(current_date)
	check_length(current_date,by)	
	current_date<-mult(current_date,nc)
	if (nct>nc) {current_date<-c(current_date,max(current_date))}	
  } else {
	if(length(current_date)!=1) {
		stop(paste0(current_date," should be of class Date or a single character 'common' or 'site'"))
	}
	if (current_date=="common") {
		current_date<-rep(max(enrollment_dates),nct)
	} else {
	   current_date<-rep(NA,nct)
    }
  }	
  
 
  accrual_df<-numeric(0)

  for (i in 1:nct) {

    if (byt==0) {
      ed<-enrollment_dates
    } else {
      if (overall==TRUE & i==nct) {
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

    if (!is.na(start_date[i])) {
      if (start_date[i] != min(adf$Date) | force_start0)  {
        stopifnot(start_date[i] <= min(adf$Date))
        adf<-rbind(data.frame(Date=start_date[i],Freq=0,Cumulative=0),adf)
      }
    }

    if (!is.na(current_date[i])) {
      if (current_date[i] != max(adf$Date)) {
        stopifnot(current_date[i] > max(adf$Date))
        adf<-rbind(adf,data.frame(Date=current_date[i],Freq=0,Cumulative=max(adf$Cumulative)))
      }
    }

    if (byt==0) {
      accrual_df<-adf
    } else {
      accrual_df<-append(accrual_df,list(adf))
      if (overall==TRUE & i==nct) {
        names(accrual_df)[i]<-name_overall
      } else {
        names(accrual_df)[i]<-lc[i]
      }
    }
  }
  class(accrual_df) <- c("accrual_df", class(accrual_df))
  return(accrual_df)
}
