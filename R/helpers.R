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
	

mult<-function(x,n) {
  var1 <- as.character(sys.call())[2]
  
  if (all(is.na(x)) | length(x)==1) {
  		x<-rep(x,n)
  } else {
  	if (length(x)!=n) warning(paste0(var1," is not of length ",n))
  }
  return(x)
}

