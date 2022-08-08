#' Accrual prediction plots
#'
#' Generates an accrual prediction plot using an accrual data frame produced by \code{accrual_create_df}
#'	and a target sample size. Prediction is based on a weighted linear regression.
#' If the accrual data frame is a list (i.e. using the by option in \code{accrual_create_df}),
#' 	or if center start dates are given, the number of enrolled and targeted sites is included.
#'
#' @rdname accrual_plot_predict
#' @param accrual_df object of class 'accrual_df' or 'accrual_list' produced by \code{accrual_create_df}.
#' @param target target sample size or date to predict end date or expected sample size, respectively.
#' 	A single number or date, or a named vector with the same length as accrual_df.
#'	For the latter, center-specific predictions are shown.
#' @param overall logical, indicates that accrual_df contains a summary with all sites (only if by is not NA).
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE).
#' @param fill_up whether to fill up days where no recruitment was observed,
#'		otherwise these points do not contribute to the regression.
#' @param wfun function to calculate the weights with accrual data frame as argument,
#'	default is wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)).
#' @param col.obs line color of cumulative recruitment, can be a vector with the same length as accrual_df.
#' @param lty.obs line type of cumulative recruitment, can be a vector with the same length as accrual_df.
#' @param col.pred line color of prediction, can be a vector with the same length as accrual_df.
#' @param lty.pred line color of prediction, can be a vector with the same length as accrual_df.
#' @param pch.pred point symbol for end of prediction, can be a vector with the same length as accrual_df.
#' @param pos_prediction position of text with predicted end date or sample size, 
#' either \code{"out"}, \code{"in"} or \code{"none"}.
#' @param label_prediction label for predicted end date or sample size.
#' @param cex_prediction text size for predicted end date or sample size.
#' @param format_prediction date format for predicted end date (only if target is a sample size)
#' @param show_center logical, whether the center info should be shown
#'	(if accrual_df is a list or if center_start_dates are given).
#' @param design design options for the center info
#'		1 (default): below plot, 2: within plot, top, 3: within plot, bottom.
#' @param center_label label for the center info.
#' @param center_legend either "number" to plot numbers in the center strip or "strip" to add a legend strip,
#'		requires specification of center_colors.
#' @param center_colors colors to be used for the strip with the centers, a vector of length targetc.
#' @param targetc target number of centers, to scale the legend if it is "strip".
#' @param center_legend_text_size  size of the text of the center or legend strip, only has a function
#		if center and center_legend is specified.
#' @param ylim limits for y-axis.
#' @param xlim limits for x-axis.
#' @param ylab y-axis label.
#' @param xlabformat format of date on x-axis.
#' @param xlabn integer giving the desired number of intervals for the xlabel, default=5.
#' @param xlabminn integer giving the minimal number of intervals.
#' @param xlabformat format of date on x-axis.
#' @param xlabpos position of the x-label.
#' @param xlabsrt rotation of x-axis labels in degrees.
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment in x- and y-direction.
#' @param xlabcex size of x-axis label.
#' @param mar vector of length 4 (bottom, left, top, right margins), overwrite default margins.
#' @param legend.list named list with options passed to legend(), only if accrual data frame is a list.
#' @param ... further options passed to plot() and axis().
#' @param center_start_dates alternative way to add center info,
#'	vector with dates on which centers are enrolled.
#'
#' @return \code{accrual_plot_predict} returns a plot with the accrual prediction.
#'
#' @export
#'
#' @importFrom grDevices heat.colors
#'
#' @examples
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' ##Predict end date
#' accrual_plot_predict(accrual_df=accrual_df,target=300)
#' ##Predict sample size
#' accrual_plot_predict(accrual_df=accrual_df,as.Date("2020-11-01"))
#'
#' #Include site
#' accrual_df<-accrual_create_df(accrualdemo$date,by=accrualdemo$site)
#' accrual_plot_predict(accrual_df=accrual_df,target=300,center_label="Site")
#' ## with strip and target
#' accrual_plot_predict(accrual_df=accrual_df,target=300,center_label="Site",
#'	 targetc=5,center_colors=heat.colors(5),center_legend="strip")
#'
#' #Design for site
#' accrual_plot_predict(accrual_df=accrual_df,target=300,design=2)
#'
#' #Format prediction end date
#' accrual_plot_predict(accrual_df=accrual_df,target=300,
#'      pos_prediction="in",label_prediction="End of accrual: ",cex_prediction=1.2,
#'      format_prediction="%Y-%m-%d",ylim=c(0,150))
#'
#' #Format plot
#' accrual_plot_predict(accrual_df=accrual_df,target=300,
#'      ylab="No of recruited patients",ylim=c(0,150),
#'      xlabcex=1.2,xlabsrt=30,xlabn=5,xlabmin=5,
#'      mgp=c(3,0.5,0),cex.lab=1.2,cex.axis=1.2)
#'
#' #predictions for all sites
#' accrual_plot_predict(accrual_df=accrual_df,
#'	target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300))
#' ## different colors
#' accrual_plot_predict(accrual_df=accrual_df,
#'	target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300),
#'	col.obs=topo.colors(length(accrual_df)))
#' ##not showing center info
#' accrual_plot_predict(accrual_df=accrual_df,
#'	target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300),
#'	show_center=FALSE)
#'
#' #predictions of sample size for all sites
#' target<-rep(as.Date("2020-11-01"),4)
#' names(target)<-c("Site 1","Site 2","Site 3","Overall")
#' accrual_plot_predict(accrual_df=accrual_df,target=target,col.obs=topo.colors(length(accrual_df)))

accrual_plot_predict<-function(accrual_df,
                               target,
                               overall=TRUE,
                               name_overall=attr(accrual_df, "name_overall"),
                               fill_up=TRUE,
                               wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)),
                               col.obs=NULL,
                               lty.obs=1,
                               col.pred="red",
                               lty.pred=2,
                               pch.pred=8,
                               pos_prediction=c("out","in","none"),
                               label_prediction=NULL,
                               cex_prediction=1,
                               format_prediction="%B %d, %Y",
                               show_center=TRUE,
                               design=1,
                               center_label="Centers",
                               center_legend=c("number","strip"),
                               targetc=NA,
                               center_colors=NULL,
                               center_legend_text_size=0.7,
                               ylim=NA,xlim=NA,
                               ylab="Recruited patients",
                               xlabformat="%d%b%Y",
                               xlabn=5,
                               xlabminn= xlabn %/% 2,
                               xlabpos=NA,
                               xlabsrt=45,
                               xlabadj=c(1,1),
                               xlabcex=1,
                               mar=NA,
                               legend.list=NULL,
                               ...,
                               center_start_dates=NULL) {


	pos_prediction<-match.arg(pos_prediction)
	center_legend<-match.arg(center_legend)

	tmp <- lc_lct(accrual_df,
	              overall,
	              name_overall)
	accrual_df <- tmp$accrual_df
	lc <- tmp$lc
	lct <- tmp$lct
	overall <- tmp$overall

	if (!is.na(sum(mar))) {
		stopifnot(length(mar)==4)
	}

	if (length(target)!=1) {
		#separate prediction
		if (length(target)!=length(accrual_df)) {
			stop("length of target has to correspond to length of accrual_df")
		} else {
			target<-check_name(target, names(accrual_df))
		}
	}
	
	preddate<-TRUE
	if (is.Date(target)) {
		preddate<-FALSE
		check_date(target)
	}
	 
	if (is.null(col.obs)) {
		if (lc==1) {
			col.obs="black"
		} else {
			col.obs<-gray.colors(lc)
		}

	}

	if (is.null(label_prediction)) {
		if (preddate) {
			label_prediction<-"Predicted end date: "
		} else {
			label_prediction<-"Predicted sample size: "
		}
	}
	
	#predictions
	#&&&&&&&&&&

	tmp <- pred_fn(accrual_df,
	               fill_up,
	               wfun,
	               lc,
	               overall,
	               target,
	               name_overall)
				   
	if (preddate) {
		end_date <- tmp$end_date
		edate <- tmp$edate
		targetm <- target
	} else {
		end_date <- target
		edate <- max(target)
		targetm <- unlist(tmp$end_date)
	}
	
	adf <- tmp$adf
	

	#plot scaling
	#&&&&&&&&&&

	alim<-ascale(accrual_df,xlim=xlim,ylim=ylim,ni=xlabn,min.n=xlabminn,addxmax=edate,addymax=targetm)

	# modification of ylim if design==3
	if (show_center) {
		if (lc>1 | !is.null(center_start_dates)) {
		  if (design==3) {
			if (alim[["ylim"]][1]==0) {
				alim[["ylim"]][1]<--max(targetm)/15
			}
		  }
		}
	}


	#margin
	#&&&&&&&&&&

	if (is.na(sum(mar))) {

		#mar<-c(5.1,4.1,2.0,1.0)
		mar<-c(5.1,4.1,4.1,2.1)

		#centers
		if (show_center) {
			if (lc>1 | !is.null(center_start_dates)) {
				  #if (center_legend=="strip") {
					#mar[4]<-2.5
				  #}
				  if (design==1)
					mar[1]<-6.5
			}
		}

	}


	#plot raw data
	#&&&&&&&&&&

	oldpar <- par(mar=mar)
	on.exit(par(oldpar))

	plot(0,type="n",ylim=alim[["ylim"]],xlim=alim[["xlim"]],
		 axes=FALSE,xlab="",ylab=ylab,...)
	box()

	#xlabel:
	xlabsl<-format(alim[["xlabs"]], xlabformat)
	axis(side=1,at=alim[["xlabs"]],labels=rep("",length(alim[["xlabs"]])),...)
	if (is.na(xlabpos)) {
		xlabpos<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
	}
	text(x=alim[["xlabs"]],y=xlabpos,srt=xlabsrt,labels=xlabsl,xpd=TRUE,adj=xlabadj,cex=xlabcex)

	#ylabel:
	axis(side=2,las=1,...)

	#only overall
	if (lc==1 | (overall & length(target)==1)) {
		lines(Cumulative~Date,data=adf,type="s",col=col.obs,lty=lty.obs)
		lp<-adf[which.max(adf$Date),]
		lines(x=c(lp$Date,end_date),y=c(lp$Cumulative,targetm),col=col.pred,lty=lty.pred)
		points(x=end_date,y=targetm,pch=pch.pred,col=col.pred,xpd=TRUE)

		#prediction text
		if (preddate) {
			pred_text<-paste0(label_prediction,format(end_date, format_prediction))
		} else {
			pred_text<-paste0(label_prediction,format(targetm,digits=0))
		}
		
		if (pos_prediction!="none") {
			if (pos_prediction=="in") {
			legend("topleft",pred_text,bty="n",cex=cex_prediction)
			} else {
			text(x=par("usr")[1],y=par("usr")[4],adj=c(0,-1), xpd=TRUE,pred_text,cex=cex_prediction)
			}
		}

	} else {
	#for each site:
		targetm<-mult(targetm,length(adf))
		col.obs<-mult(col.obs,length(adf))
		lty.obs<-mult(lty.obs,length(adf))
		col.pred<-mult(col.pred,length(adf))
		lty.pred<-mult(lty.pred,length(adf))
		pch.pred<-mult(pch.pred,length(adf))

		for (k in 1:length(adf)) {

			lines(Cumulative~Date,data=adf[[k]],type="s",col=col.obs[k],lty=lty.obs[k])
			lp<-adf[[k]][which.max(adf[[k]]$Date),]
			lines(x=c(lp$Date,end_date[[k]]),y=c(lp$Cumulative,targetm[k]),col=col.pred[k],lty=lty.pred[k])
			points(x=end_date[[k]],y=targetm[k],pch=pch.pred[k],col=col.pred[k],xpd=TRUE)
		}
		if (preddate) {
			lna<-paste0(names(adf),": ",format(do.call("c",end_date), format_prediction))
		} else {
			lna<-paste0(names(adf),": ",format(targetm, digits=0))
		}
		
		if(!is.null(legend.list)) {
			ll<-legend.list
			#defaults if not given:
			vlist<-c("x","legend","ncol","col","lty","bty","y.intersp","seg.len")
			obslist<-list("topleft",lna,1,col.obs,lty.obs,"n",0.85,1.5)
			for (d in 1:length(vlist)) {
				if (is.null(ll[[vlist[d]]])) {
					ll[[vlist[d]]]<-obslist[[d]]
				}
			}
		} else {
			ll<-list(x = "topleft",legend = lna,ncol=1,col=col.obs,lty=lty.obs,bty="n",y.intersp=0.85,seg.len=1.5)
		}
		do.call("legend",ll)

	}



	#plot centers info
	#&&&&&&&&&&

	if (show_center) {
		if (lc>1 | !is.null(center_start_dates)) {

			plot_center(accrual_df=accrual_df,
				center_start_dates=center_start_dates,
				overall=overall,name_overall=name_overall,
				lc=lc,lct=lct,design=design,
				center_legend=center_legend,center_colors=center_colors,targetc=targetc,
				center_label=center_label,center_legend_text_size=center_legend_text_size)
		}

	}
}

#**********************************************************************************#

#' Cumulative accrual plots
#'
#' Plot of cumulative recruitment using an accrual data frame produced by \code{accrual_create_df}.
#'
#' @rdname accrual_plot_cum
#' @param accrual_df object of class 'accrual_df' or 'accrual_list' produced by \code{accrual_create_df}.
#' @param ylim limits for y-axis.
#' @param xlim limits for x-axis.
#' @param ylab y-axis label.
#' @param xlabn integer giving the desired number of intervals for the xlabel, default=5.
#' @param xlabminn negative integer giving the minimal number of intervals.
#' @param xlabformat format of date on x-axis.
#' @param xlabpos position of the x-label.
#' @param xlabsrt rotation of x-axis labels in degrees.
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2
#'	for different adjustment in x- and y-direction.
#' @param xlabcex size of x-axis label.
#' @param col color for line(s) in plot
#		if accrual_df is a list and overall is indicated, the first entry is used for the overall.
#' @param lty line type(s) in plot
#		if accrual_df is a list and overall is indicated, the first entry is used for the overall.
#' @param legend.list named list with options passed to legend().
#' @param ... further options passed to plot() and axis().
#'
#' @return \code{accrual_plot_cum} returns a plot of the cumulative accrual (per site if accrual_df is a list).
#'
#' @export
#' @importFrom graphics plot
#'
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_plot_cum(accrual_df)
#' accrual_plot_cum(accrual_df,cex.lab=1.2,cex.axis=1.1,xlabcex=1.1)
#'
#' #several sites
#' set.seed(1)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' accrual_plot_cum(accrual_df)
#'
#' #assuming a common start and current date
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers,start_date="common",current_date="common")
#' accrual_plot_cum(accrual_df)
#'
#' #plot and legend options
#' accrual_plot_cum(accrual_df,col=c("red",rep(1,3)),lty=c(1,1:3),cex.lab=1.2,cex.axis=1.1,xlabcex=1.1)
#' accrual_plot_cum(accrual_df,legend.list=list(ncol=2,bty=TRUE,cex=0.8))
#'
#' #without overall
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers,overall=FALSE)
#' accrual_plot_cum(accrual_df)
#'

accrual_plot_cum<-function(accrual_df,
                           ylim=NA,
                           xlim=NA,
                           ylab="Recruited patients",
                           xlabn=5,
                           xlabminn= xlabn %/% 2,
                           xlabformat="%d%b%Y",
                           xlabpos=NA,
                           xlabsrt=45,
                           xlabadj=c(1,1),
                           xlabcex=1,
                           col=rep(1:8,5),
                           lty=rep(1:5,each=8),
                           legend.list=NULL,
                           ...) {

  if (is.data.frame(accrual_df)) {
    accrual_df<-list(accrual_df)
  } else {
	if (!all(unlist(lapply(accrual_df,function(x) is.data.frame(x))))) {
		stop("accrual_df has to be a data frame or a list of data frames")
	}
  }
  lc<-length(accrual_df)

  if (lc>length(col)) {
    col<-rep(col,lc)
  }
  if (lc>length(lty)) {
    lty<-rep(lty,lc)
  }


  alim<-ascale(accrual_df,xlim=xlim,ylim=ylim,ni=xlabn,min.n=xlabminn)
  lna<-names(accrual_df)

  plot(0,type="n",ylim=alim[["ylim"]],xlim=alim[["xlim"]],
       axes=FALSE,xlab="",ylab=ylab,...)
  box()

  #xlabel
  xlabsl<-format(alim[["xlabs"]], xlabformat)
  axis(side=1,at=alim[["xlabs"]],labels=rep("",length(alim[["xlabs"]])),...)
  if (is.na(xlabpos)) {
    xlabpos<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
  }
  text(x=alim[["xlabs"]],y=xlabpos,srt=xlabsrt,labels=xlabsl,xpd=TRUE,adj=xlabadj,cex=xlabcex)
  axis(side=2,las=1,...)

  for (i in (1:lc)) {
    dfi<-accrual_df[[i]]
    lines(Cumulative~Date,data=dfi,col=col[i],lty=lty[i],type="s")
  }

  if (lc!=1) {
    if(!is.null(legend.list)) {
      ll<-legend.list
      #defaults if not given:
      vlist<-c("x","legend","ncol","col","lty","bty","y.intersp","seg.len")
      obslist<-list("topleft",lna,1,col,lty,"n",0.85,1.5)
      for (d in 1:length(vlist)) {
        if (is.null(ll[[vlist[d]]])) {
          ll[[vlist[d]]]<-obslist[[d]]
        }
      }
    } else {
      ll<-list(x = "topleft",legend = lna,ncol=1,col=col,lty=lty,bty="n",y.intersp=0.85,seg.len=1.5)
    }
    do.call("legend",ll)
  }

}


#**********************************************************************************#

#' Absolute accrual plots
#'
#' Plot of absolute recruitment by time unit using an accrual data frame produced by \code{accrual_create_df}.
#'
#' @rdname accrual_plot_abs
#' @param accrual_df object of class 'accrual_df' or 'accrual_list' produced by \code{accrual_create_df}.
#' @param unit time unit for which the bars should be plotted,
#'	one of \code{"month"}, \code{"year"}, \code{"week"} or \code{"day"}.
#' @param target adds horizontal line for target recruitment per time unit.
#' @param overall logical, indicates that accrual_df contains a summary with all sites
#'		that should be removed from stacked barplot (only if by is not NA).
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE).
#' @param ylim limits for y-axis.
#' @param xlim limits for x-axis.
#' @param ylab y-axis label.
#' @param xlabformat format of date on x-axis.
#' @param xlabsel selection of x-labels if not all should be shown,
#'		 by default all are shown up to 15 bars, with more an automated selection is done,
#'		 either NA (default), NULL (show all), or a numeric vector.
#' @param xlabpos position of the x-label.
#' @param xlabsrt rotation of x-axis labels in degrees.
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment
#' 		in x- and y-direction.
#' @param xlabcex size of x-axis label.
#' @param col colors of bars in barplot, can be a vector if accrual_df is a list, default is grayscale.
#' @param legend.list named list with options passed to legend().
#' @param ... further arguments passed to barplot() and axis().
#'
#' @return \code{accrual_plot_abs} returns a barplot of absolute accrual by time unit (stacked if accrual_df is a list).
#'
#' @export
#'
#' @importFrom graphics abline axis barplot box grconvertX grconvertY legend lines mtext par points polygon text
#' @importFrom grDevices gray.colors
#'
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:100, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_plot_abs(accrual_df,unit="week")
#'
#' #time unit
#' accrual_plot_abs(accrual_df,unit="day")
#'
#' #include target
#' accrual_plot_abs(accrual_df,unit="week",target=5)
#'
#' #further plot options
#' accrual_plot_abs(accrual_df,unit="week",ylab="No of recruited patients",
#'    xlabformat="%Y-%m-%d",xlabsrt=30,xlabpos=-0.8,xlabadj=c(1,0.5),
#'    col="pink",tck=-0.03,mgp=c(3,1.2,0))
#'
#' #accrual_df with by option
#' set.seed(2020)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' centers<-factor(centers,levels=c("Site 1","Site 2","Site 3"))
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' accrual_plot_abs(accrual_df=accrual_df,unit=c("week"))
#'

accrual_plot_abs<-function(accrual_df,
                           unit=c("month","year","week","day"),
                           target=NULL,
						   overall=TRUE,
                           name_overall=attr(accrual_df, "name_overall"),
                           ylim=NULL,
                           xlim=NULL,
                           ylab="Recruited patients",
                           xlabformat=NULL,
                           xlabsel=NA,
                           xlabpos=NULL,
                           xlabsrt=45,
                           xlabadj=c(1,1),
                           xlabcex=1,
                           col=NULL,
						    legend.list=NULL,
							...) {


	if (is.data.frame(accrual_df)) {
		accrual_df<-list(accrual_df)
	} else {
		if (!all(unlist(lapply(accrual_df,function(x) is.data.frame(x))))) {
			stop("accrual_df has to be a data frame or a list of data frames")
		}
	}

	#remove overall if
	if (length(accrual_df)>1 & overall==TRUE) {
		if (is.null(accrual_df[[name_overall]])) {
			print(paste0("'",name_overall,"' not found in accrual_df, overall set to FALSE"))
			overall<-FALSE
		} else {
			accrual_df<-accrual_df[names(accrual_df)!=name_overall]
		}
	}
	lc<-length(accrual_df)

	unit<-match.arg(unit)
	if (length(unit)!=1) {
		stop("unit should be of length 1")
	}
	if (!is.null(ylim) & length(ylim)!=2) {
		stop("ylim should be of length 2")
	}
	if (!is.null(xlim) & length(xlim)!=2) {
		stop("xlim should be of length 2")
	}

	#default colors
	if (is.null(col)) {
		if (lc==1) {
			col="grey"
		} else {
			col<-gray.colors(lc)
		}

	}

	#default xlabformat
	if (is.null(xlabformat)) {
		if (unit=="month") {xlabformat<-"%b %Y"}
		if (unit=="year") {xlabformat<-"%Y"}
		if (unit=="week") {xlabformat<-"%d %b %Y"}
		if (unit=="day") {xlabformat<-"%d %b %Y"}
	}

	for (i in 1:lc) {

	  accrual_dfi<-accrual_df[[i]]

	  #summarize data by time unit
	  dfi<-accrual_time_unit(accrual_dfi,unit=unit)
	  dfi<-dfi[,c("date","Freq")]
	  names(dfi)[names(dfi)=="Freq"]<-paste0("Freq",i)

	  if (i==1) {
		#dfit<-dfi[,names(dfi)!="date"]
		dfit<-dfi
	  } else {
		#dfit<-merge(dfit,dfi[,names(dfi)!="date"],all=TRUE)
		dfit<-merge(dfit,dfi,all=TRUE)
	  }
	}

	dfit<-dfit[order(dfit$date),]
	ma<-as.matrix(dfit[,paste0("Freq",1:lc)])
	ma[is.na(ma)]<-0
	if (ncol(ma)>1) {
		ma<-ma[,ncol(ma):1]
	}
	rownames(ma)<-rep("",nrow(ma))

	if (is.null(ylim)) {
	  ylim<-c(0,max(apply(ma,1,function(x) sum(x,na.rm=TRUE)),target,na.rm=TRUE)+1)
	}

	#xscale
	b<-barplot(t(ma),plot=FALSE)
	if (is.null(xlim)) {
		xlim<-c(min(b),max(b)) + c(-0.5,0.5)
	}

	#x label selection
	if (is.null(xlabsel)) {
	  sel<-1:length(b)
	} else {
	  if (sum(!is.na(xlabsel))==0) {
	    if (length(b)>15) {
	      sel<-round(seq(1,length(b),l=8))
	      sel<-sel[sel>0&sel<=length(b)]
	    } else {
	      sel<-1:length(b)
	    }
	  } else {
	    sel<-xlabsel
	  }
	}

	#plot
	b<-barplot(t(ma),ylab=ylab,ylim=ylim,axes=FALSE,xlim=xlim,col=col,...)

	box()
	axis(side=2,las=2,...)
	axis(side=1,at=b,labels=rep("",length(b)),...)

	#xlabel
	if (is.null(xlabpos)) {
	  xlabpos<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
	}
	sel<-sel[sel<=length(b)]
	bu<-b[sel]
	lab<-format(dfit$date,xlabformat)
	lab<-lab[sel]
	text(x=bu,y=xlabpos,srt=xlabsrt,labels=lab,xpd=TRUE,adj=xlabadj,cex = xlabcex)

	#line for target
	if (!is.null(target)) {
	  abline(h=target,lty=2)
	}

	#legend
	if (lc!=1) {
		if(!is.null(legend.list)) {
			ll<-legend.list
			#defaults if not given:
			vlist<-c("x","legend","ncol","fill","bty","y.intersp","seg.len")
			obslist<-list("topright",names(accrual_df),1,rev(col),"n",0.85,1.5)
			for (d in 1:length(vlist)) {
				if (is.null(ll[[vlist[d]]])) {
					ll[[vlist[d]]]<-obslist[[d]]
				}
			}
		} else {
			ll<-list(x = "topright",legend = names(accrual_df),ncol=1,fill=rev(col),
				bty="n",y.intersp=0.85,seg.len=1.5)
		}
		do.call("legend",ll)
	}
}


# helpers
ascale<-function(adf,xlim=NA,ylim=NA,ni=5,min.n=ni %/% 2) {
  if (is.data.frame(adf)) {
    adf<-list(adf)
  }
  if (sum(!is.na(xlim))==0) {
    xlims<-c(min(do.call("c",lapply(adf,function(x) min(x$Date)))),
             max(do.call("c",lapply(adf,function(x) max(x$Date)))))
  } else {
    xlims<-xlim
  }
  xlabs<-pretty(x=xlims,n=ni,min.n=min.n)
  xlabs<-xlabs[xlabs>=xlims[1] & xlabs <=xlims[2]]

  if (sum(!is.na(ylim))==0) {
    ymax<-max(do.call("c",lapply(adf,function(x) max(x$Cumulative))))
    ylims<-c(0,ymax)
  } else {
    ylims<-ylim
  }
  alim<-list(xlim=xlims,ylim=ylims,xlabs=xlabs)
  return(alim)
}



mult<-function(var, accrual_df) {
  if (length(var)==1) {
    var<-rep(var,length(accrual_df))
    return(var)
  } else {
    stopifnot(length(var)==length(accrual_df))
    return(var)
  }
}


