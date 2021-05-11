# helper functions


check_date <- function(x){
  var <- as.character(sys.call())[2]
  if(!any(c("Date") %in% class(x))) stop("'", var, "' should be of class Date")
}


check_length <- function(x,y) {
  var1 <- as.character(sys.call())[2]
  var2 <- as.character(sys.call())[3]
  if (all(is.na(y))) { 
    if (length(x)!=1) stop(paste0(x," should be of length 1 if ",var2,"=NA"))
  } else {
    if (length(x)!=1 & length(x)!=length(unique(y))) {
  	  stop(paste0(var1," should be of length 1 or ",length(unique(y)),
		" (the number of distinct centers in ",var2,")."))
  	}
  }
}	
	
genadf<-function(enrollment_dates,start_date,current_date,force_start0) {
	adf <- data.frame(table(enrollment_dates))
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
	return(adf)
}




mult<-function(x,n) {
  var1 <- as.character(sys.call())[2]
  
  if (all(is.na(x)) | length(x)==1) {
  		x<-rep(x,n)
  } else {
  	if (length(x)!=n) warning(paste0(var1," is not of length ",n))
  }
  return(x)
}




ascale<-function(adf,xlim=NA,ylim=NA,ni=5,min.n=ni %/% 2, addxmax = NULL, addymax = NULL) {
   if (is.data.frame(adf)) {
     adf<-list(adf)
   }
   if (sum(!is.na(xlim))==0) {
     xlims<-c(min(do.call("c",lapply(adf,function(x) min(x$Date)))),
              max(do.call("c",lapply(adf,function(x) max(x$Date)))))
	if (!is.null(addxmax)) {
		xlims[2]<-max(xlims[2],addxmax)
	}	
	  xlabs<-pretty(x=xlims,n=ni,min.n=min.n)
     xlims<-c(min(xlims,xlabs),max(xlims,xlabs))
   } else {
     xlims<-xlim
	  xlabs<-pretty(x=xlims,n=ni,min.n=min.n)
     xlabs<-xlabs[xlabs>=xlims[1] & xlabs <=xlims[2]]
   }

   if (sum(!is.na(ylim))==0) {
     ymax<-max(do.call("c",lapply(adf,function(x) max(x$Cumulative))))
     ylims<-c(0,ymax)
	 if (!is.null(addymax)) {
		ylims[2]<-max(ylims[2],addymax)
	 }
   } else {
     ylims<-ylim
   }
   alim<-list(xlim=xlims,ylim=ylims,xlabs=xlabs)
   return(alim)
}
 
 