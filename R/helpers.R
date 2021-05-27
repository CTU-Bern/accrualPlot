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

genadf<-function(enrollment_dates,start_date,current_date,force_start0,name=NULL,warning=TRUE) {

	adf <- data.frame(table(enrollment_dates))
    colnames(adf) <- c("Date", "Freq")
    adf$Date <- as.Date(as.character(adf$Date))
    adf<-adf[order(adf$Date),]
    adf$Cumulative <- cumsum(adf$Freq)

	if (is.null(name)) {
		wtext<-""
	} else {
		wtext<-paste0(" for ",name)
	}
	if (!is.na(start_date) & start_date > min(adf$Date)) {
		if (warning) {
			warning(paste0("Start date is after first recruitment",wtext," and will not be used."))
		}
		start_date<-NA
	}
	if (!is.na(current_date) & current_date < max(adf$Date)) {
		if (warning) {
			warning("Current date is before last recruitment",wtext," and will not be used.")
		}
		current_date<-NA
	}

    if (!is.na(start_date) & start_date!=min(adf$Date)) {
		adf<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),adf)
    } else {
		if (force_start0) {
			adf<-rbind(data.frame(Date=min(adf$Date),Freq=0,Cumulative=0),adf)
		}
	}

    if (!is.na(current_date) & current_date!=max(adf$Date)) {
		adf<-rbind(adf,data.frame(Date=current_date,Freq=0,Cumulative=max(adf$Cumulative)))
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


plot_center<-function(accrual_df,center_start_dates,
	overall,name_overall,
	lc,lct,design,
	center_legend,center_colors,targetc,
	center_label,center_legend_text_size) {

	if (!is.null(center_start_dates)) {
		if (length(accrual_df)>1)	{
			if (lct!=length(center_start_dates)) {
				stop("length of center_start_dates has to correspond to the number of sites (not including overall)")
			}
		}
	} else {
		if (length(accrual_df)>1)	{
			if (overall) {
				center_start_dates<-do.call("c",lapply(accrual_df[names(accrual_df)!=name_overall],
					function(x) min(x$Date)))
			} else {
				center_start_dates<-do.call("c",lapply(accrual_df,function(x) min(x$Date)))
			}
		}
	}

	csk<-accrual_create_df(center_start_dates)

	cdate<-max(do.call("c",lapply(accrual_df,function(x) max(x$Date))))

	stopifnot(design>0 & design<=3)

	if (is.na(targetc)) {
		targetc<-lct
	}

	#colors
	if(is.null(center_colors)) {
		if (center_legend=="number")  {
			center_colors<-rep("grey90",targetc)
		} else {
			center_colors<-gray.colors(targetc)
		}
	} else {
		if (length(center_colors)!=targetc) {
			warning(paste0("center_colors is not of length ",targetc))
		}
	}

	cols<-rev(center_colors)
	centerw<-1

	#dates
	cdates<-c(csk$Date,cdate)


	#coordinates for plotting
	uc<-par("usr")
	lh <- par('cin')[2] * par('cex') * par('lheight')
	x_off <- diff(grconvertX(0:1, 'inches', 'user'))
	y_off <- diff(grconvertY(0:1, 'inches', 'user'))
	bwidth<-centerw*y_off*lh
	ypf<-function(yp1) {c(rep(yp1,2),rep(yp1 + bwidth,2))} #get position for barplot

	if (design==1) {
		yp1<-uc[3] - par("mar")[1] * y_off*lh #at the bottom
		yp1<-uc[3] - (par("mar")[1]-0.4) * y_off*lh #0.4 lines above the bottom
		yp<-ypf(yp1)
		ypl<-mean(yp)
		xpl<-cdates[1]-(uc[2]-uc[1])/50
		xadj<-1
		label<-center_label
	}

	if (design==2) {
		yp1<-0.85*uc[4]
		yp<-ypf(yp1)
		ypl<-1.03*max(yp)
		xpl<-cdates[1]
		xadj<-0
		label<-center_label
	}

	if (design==3) {
		yp1<-0.85*uc[3]
		yp<-ypf(yp1)
		ypl<-mean(yp)
		xpl<-cdates[length(cdates)]+(uc[2]-uc[1])/50
		xadj<-0
		label<-center_label
	}


	for (i in 1:(length(cdates)-1)) {
		nc<-csk$Cumulative[i]
		polygon(x=c(cdates[i],rep(cdates[i+1],2),cdates[i]),y=yp,
			xpd=TRUE,col=cols[nc],border="black")
	}

	#legend
	text(x=xpl,y=ypl,labels=label,adj=xadj,xpd=TRUE)

	if (center_legend=="number") {
		td<-(as.numeric(cdates)[-length(cdates)]+as.numeric(cdates)[-1])/2
		text(x=td,y=mean(yp),labels=csk$Cumulative,xpd=TRUE,cex=center_legend_text_size)

	} else {

		bwidth<-centerw*y_off*lh
		pl<- 0.5 * x_off * lh
		lxp<-par("usr")[2] + pl/2
		ypp<-seq(yp[1],yp[3] + 2*bwidth ,l=targetc+1)
		if (design==2) {
			ypp<-seq(yp[1]- bwidth,yp[3] + bwidth,l=targetc+1)
		}
		atc<-round(seq(1,targetc,l=5))
		ypatc<-(ypp[atc]+ypp[atc+1])/2
		tcks<-pl/5
		xtck<-matrix(rep(c(lxp+pl,lxp+pl+tcks),length(atc)),length(atc),2,byrow=TRUE)
		ytck<-matrix(rep(ypatc,each=2),length(atc),2,byrow=TRUE)

		for (i in 1:targetc) {
			polygon(x=c(lxp,lxp+pl,lxp+pl,lxp),y=c(ypp[i],ypp[i],ypp[i+1],ypp[i+1]),
				xpd=TRUE,col=cols[i],border=NA)
		}
		lines(x=c(lxp,lxp)+pl,y=c(min(ypp),max(ypp)),xpd=TRUE)
		lines(x=c(lxp,lxp),y=c(min(ypp),max(ypp)),xpd=TRUE)
		lines(x=c(lxp,lxp+pl),y=c(min(ypp),min(ypp)),xpd=TRUE)
		lines(x=c(lxp+pl,lxp),y=c(max(ypp),max(ypp)),xpd=TRUE)

		for (i in 1:nrow(xtck)) {
			lines(x=xtck[i,],y=ytck[i,],xpd=TRUE)
		}
		text(x=lxp+pl+2*tcks,y=ytck[,2],label=atc,xpd=TRUE,adj=0,cex=center_legend_text_size)

	}

}






# helpers for prediction

lc_lct <- function(accrual_df){
  overall <- get("overall", envir = parent.frame())
  name_overall <- get("name_overall", envir = parent.frame())

  if (is.data.frame(accrual_df)) {
    accrual_df<-list(accrual_df)
  } else {
    if (!all(unlist(lapply(accrual_df,function(x) is.data.frame(x))))) {
      stop("accrual_df has to be a data frame or a list of data frames")
    }
  }
  lc<-lct<-length(accrual_df)

  if (lc>1 & overall==TRUE) {
    if (is.null(accrual_df[[name_overall]])) {
      print(paste0("'",name_overall,"' not found in accrual_df, overall set to FALSE"))
      overall<-FALSE
    }
  }

  if (overall & lc!=1) {
    lct<-lc-1
  }
  assign("accrual_df", accrual_df, envir = parent.frame())
  assign("lc", lc, envir = parent.frame())
  assign("lct", lct, envir = parent.frame())
  assign("overall", overall, envir = parent.frame())
}

pred_fn <- function(accrual_df,
                    fill_up,
                    wfun,
                    lc,
                    overall,
                    target,
                    name_overall){

  if (lc==1) {
    #only 1:
    adf<-accrual_df[[1]]
    m1<-accrual_linear_model(adf,fill_up=fill_up,wfun=wfun)
    end_date<-accrual_predict(adf,m1,target)
    edate<-end_date
  } else {
    #only 1 target and overall
    if (overall & length(target)==1) {
      adf<-accrual_df[[name_overall]]
      m1<-accrual_linear_model(adf,fill_up=fill_up,wfun=wfun)
      end_date<-accrual_predict(adf,m1,target)
      edate<-end_date
    } else {
      #no overall or several targets: multiple predictions
      adf<-accrual_df
      m1<-accrual_linear_model(adf,fill_up=fill_up,wfun=wfun)
      end_date<-accrual_predict(adf,m1,target)
      edate<-max(do.call("c",end_date))
    }
  }

  assign("end_date", end_date, envir = parent.frame())
  assign("edate", edate, envir = parent.frame())
  assign("adf", adf, envir = parent.frame())

}




