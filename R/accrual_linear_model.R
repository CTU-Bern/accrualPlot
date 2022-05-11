#' accrual_linear_model
#'
#' Creates a weighted linear regression model using an accrual data frame produced by \code{accrual_create_df}.
#'
#' @param accrual_df object of class 'accrual_df' or 'accrual_list' produced by \code{accrual_create_df}.
#' @param fill_up whether to fill up days where no recruitment was observed,
#		otherwise these points do not contribute to the regression, default is TRUE.
#' @param wfun function to calculate the weights with accrual data frame as argument, 
#'	default is wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)).
#'
#' @return Returns an object of class 'lm' with a weighted linear regression of cumulative accrual on dates.
#'
#' @export
#'
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
#' accrual_df<-accrual_create_df(enrollment_dates,start_date=as.Date("2017-12-01"),
#'     current_date=as.Date("2018-03-01"))
#' accrual_linear_model(accrual_df)
#'
#' #accrual_df with by option
#' set.seed(2020)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' accrual_linear_model(accrual_df)
#' }

accrual_linear_model <- function(accrual_df,
                                 fill_up=TRUE,
                                 wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))) {

  # fill_up<-match.arg(fill_up)

  if (is.data.frame(accrual_df)) {
	  accrual_df<-list(accrual_df)
  }

  lmi<-numeric(0)
  for (i in 1:length(accrual_df)) {
	accrual_dfi<-accrual_df[[i]]


    #fill up days:
    if (fill_up) {
      alldays<-seq(min(accrual_dfi$Date),max(accrual_dfi$Date),by=1)
      alldays<-alldays[!(alldays %in% accrual_dfi$Date)]
      if (!is.null(nrow(alldays))) {
        alldays_df<-data.frame(Date=alldays,Freq=0,Cumulative=NA)
        adf<-rbind(accrual_dfi,alldays_df)
        adf<-adf[order(adf$Date),]
        stopifnot(cumsum(adf$Fre)[!is.na(adf$Cumulative)]==adf$Cumulative[!is.na(adf$Cumulative)])
        adf$Cumulative<-cumsum(adf$Fre)
        accrual_dfi<-adf
      }
    }

    #linear model:
    accrual_dfi<-aggregate(cbind(Freq,Cumulative)~Date,data=accrual_dfi,FUN=sum)
    weivec <- wfun(accrual_dfi)
    stopifnot( length(weivec) == nrow(accrual_dfi) )
    lmi<-append(lmi,list(lm(Cumulative ~ Date, data=accrual_dfi, weights = weivec)))
  }

  if (length(lmi)==1) {
	return(lmi[[1]])
  } else {
	names(lmi)<-names(accrual_df)
	return(lmi)
  }
}
