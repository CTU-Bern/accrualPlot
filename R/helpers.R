# helper functions


check_date <- function(x){
  var <- as.character(sys.call())[2]
  if(!any(c("Date") %in% class(x))) stop("'", var, "' should be of class Date")
}




