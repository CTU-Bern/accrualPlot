#' accrual_plot_predict
#'
#' Generates an accrual prediction plot based an accrual data frame (produced by accrual_create_df)  
#'	and a target sample size. 
#' If the accrual data frame is a list (i.e. using the by option in accrual_create_df, 
#' 	or if center start dates are given, the number of enrolled and targeted sites is included.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df (optionally with by option as a list)
#' @param target target sample size, if it is a vector with the same length as accrual_df, center-specific 
#'		predictions are shown
#' @param overall logical, indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#' @param fill_up whether to fill up days where no recruitment was observed,
#'		otherwise these points do not contribute to the regression, default is yes
#' @param wfun function to calculate the weights based on the accrual data frame, default is
#'		wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
#' @param col.obs line color of cumulative recruitment, can be a vector with the same length as accrual_df
#' @param lty.obs line type of cumulative recruitment, can be a vector with the same length as accrual_df
#' @param col.pred line color of prediction, can be a vector with the same length as accrual_df
#' @param lty.pred line color of prediction, can be a vector with the same length as accrual_df
#' @param pch.pred point symbol for end of prediction, can be a vector with the same length as accrual_df
#' @param pos_prediction position of text with predicted end date, out, in or none
#' @param label_prediction label for predicted end date
#' @param cex_prediction text size for predicted end date
#' @param format_prediction date format for predicted end date
#' @param show_center logical, whether the center info should be shown 
#'	(if accrual_df is a list or if center_start_dates are given)	
#' @param design design options for the center info
#'		1 (default): below plot, 2: within plot, top, 3: within plot, bottom
#' @param center_label label for the center info
#' @param center_legend either "number" to plot numbers in the center strip or "strip" to add a legend strip,
#'		requires specification of center_colors
#' @param center_colors colors to be used for the strip with the centers, a vector of length targetc
#' @param targetc target number of centers, to scale the legend if it is "strip"
#' @param center_legend_text_size  size of the text of the center or legend strip, only has a function
#		if center and center_legend is specified
#' @param ylim limits for y-axis, can be a vector with the same length as accrual_df
#' @param xlim limits for x-axis, can be a vector with the same length as accrual_df
#' @param ylab y-axis label
#' @param xlabformat format of date on x-axis
#' @param xlabn integer giving the desired number of intervals for the xlabel, default=5
#' @param xlabminn nonnegative integer giving the minimal number of intervals
#' @param xlabformat format of date on x-axis
#' @param xlabpos position of the x-label, can be a vector with the same length as accrual_df
#' @param xlabsrt rotation of x-axis labels in degrees
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment in x- and y-direction
#' @param xlabcex size of x-axis label
#' @param mar vector of length 4 (bottom, left, top, right margins), overwrite default margins
#' @param legend.list named list with options passed to legend(), only if accrual data frame is a list
#' @param ... further options passed to plot() and axis()
#' @param center_start_dates alternative way to add center info, 
#'	vector with dates on which centers are enrolled
#'
#' @return A plot with cumulative accrual and the prediction to the end date.
#'
#' @export
#'
#' @importFrom grDevices heat.colors
#'
#' @examples
#' #Data
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' center_start_dates<-enrollment_dates[sample(1:length(enrollment_dates),5)]
#'
#' #Default plot
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_plot_predict(accrual_df=accrual_df,target=100)
#'
#' #Include site
#' set.seed(2021)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' accrual_plot_predict(accrual_df=accrual_df,target=100,center_label="Site")
#' ## with strip and target
#' accrual_plot_predict(accrual_df=accrual_df,target=100,center_label="Site",
#'	 targetc=5,center_colors=heat.colors(5),center_legend="strip")
#'
#' #Design for site
#' accrual_plot_predict(accrual_df=accrual_df,target=100,design=2)
#'
#' #Format prediction end date
#' accrual_plot_predict(accrual_df=accrual_df,target=100,
#'      pos_prediction="in",label_prediction="End of accrual: ",cex_prediction=1.2,
#'      format_prediction="%Y-%m-%d",ylim=c(0,150))
#'
#' #Format plot
#' accrual_plot_predict(accrual_df=accrual_df,target=100,
#'      ylab="No of recruited patients",ylim=c(0,150),
#'      xlabcex=1.2,xlabsrt=30,xlabn=5,xlabmin=5,
#'      mgp=c(3,0.5,0),cex.lab=1.2,cex.axis=1.2)
#'
#' #predictions for all sites
#' accrual_plot_predict(accrual_df=accrual_df,target=c(30,30,30,100))
#' ## different colors
#' accrual_plot_predict(accrual_df=accrual_df,target=c(30,30,30,100),
#'	col.obs=topo.colors(length(accrual_df)))
#' ##not showing center info
#' accrual_plot_predict(accrual_df=accrual_df,target=c(30,30,30,100),show_center=FALSE)
#'
accrual_plot_predict<-function(accrual_df,
                               target,
							   overall=TRUE,
							   name_overall="Overall",
                               fill_up=TRUE,
                               wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)),
							   col.obs=NULL,
							   lty.obs=1,
							   col.pred="red",
							   lty.pred=2,
							   pch.pred=8,
							   pos_prediction=c("out","in","none"),
                               label_prediction="Predicted end date: ",
                               cex_prediction=1.1,
                               format_prediction="%B %d, %Y",
                               show_center=TRUE,
							   design=1,
							   center_label="Centers",
							   center_legend=c("number","strip"),
                               targetc=NA,
                               center_colors=NA,
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
	
	
	stopifnot(design>0 & design<=3)
	
	if (center_legend=="strip") {
		stopifnot(!is.na(center_colors[1]))
	}
	
	if (!is.na(sum(mar))) {
		stopifnot(length(mar)==4)
		par(mar=mar)
	}

	if (length(target)==1) {
		#only one prediction
	} else {
		#separate prediction
		if (length(target)!=length(accrual_df)) {
			stop("length of target has to correspond to length of accrual_df")
		}
	}
	
	if (is.null(col.obs)) {
		if (lc==1) {
			col.obs="black"
		} else {
			col.obs<-gray.colors(lc)
		}
	}
	

	#centers
	#&&&&&&&&&&
	
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
	if (!is.null(center_start_dates)) {
		csk<-accrual_create_df(center_start_dates)
	}
	
	if (is.na(targetc)) {
		targetc<-lct
	}

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
	
	alim<-ascale(accrual_df,xlim=xlim,ylim=ylim,ni=xlabn,min.n=xlabminn,addxmax=edate,addymax=target)
	cdate<-max(do.call("c",lapply(accrual_df,function(x) max(x$Date))))
	

	#plot setup
	#&&&&&&&&&&

	if (pos_prediction %in% c("in","none")) {
		margin_top<-1
	} else {
		margin_top<-2
	}
	if (is.na(sum(mar))) {
		par(mar=c(5,4.3,margin_top,1))
	}

	#centers
	if (!is.null(center_start_dates)) {

	  margin_bottom<-5

	  if (center_legend=="number") {
		margin_right<-1
	  } else {
		margin_right<-2.5
	  }

	  if (design==1) {
		margin_bottom<-6.5
	  }

	  if (design==3) {
		if (alim[["ylim"]][1]==0) {
			alim[["ylim"]][1]<--max(target)/15
		}
	  }

	  if (is.na(sum(mar))) {
		par(mar=c(margin_bottom,4.3,margin_top,margin_right))
	  }
	
	}

	#plot raw data
	#&&&&&&&&&&

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
		lines(x=c(lp$Date,end_date),y=c(lp$Cumulative,target),col=col.pred,lty=lty.pred)
		points(x=end_date,y=target,pch=pch.pred,col=col.pred,xpd=TRUE)
		
		#predicted end date
		if (pos_prediction!="none") {
			if (pos_prediction=="in") {
			legend("topleft",paste0(label_prediction,format(end_date, format_prediction)),bty="n",
				cex=cex_prediction)
			} else {
			text(x=par("usr")[1],y=par("usr")[4],adj=c(0,-1), xpd=TRUE,
				paste0(label_prediction,format(end_date, format_prediction)),cex=cex_prediction)
			}
		}
	
	} else {
	#for each site:
		target<-mult(target,length(adf))
		col.obs<-mult(col.obs,length(adf))
		lty.obs<-mult(lty.obs,length(adf))
		col.pred<-mult(col.pred,length(adf))
		lty.pred<-mult(lty.pred,length(adf))
		pch.pred<-mult(pch.pred,length(adf))
			
		for (k in 1:length(adf)) {
			
			lines(Cumulative~Date,data=adf[[k]],type="s",col=col.obs[k],lty=lty.obs[k])
			lp<-adf[[k]][which.max(adf[[k]]$Date),]
			lines(x=c(lp$Date,end_date[[k]]),y=c(lp$Cumulative,target[k]),col=col.pred[k],lty=lty.pred[k])
			points(x=end_date[[k]],y=target[k],pch=pch.pred[k],col=col.pred[k],xpd=TRUE)
		}
		lna<-paste0(names(adf),": ",format(do.call("c",end_date), format_prediction))
		
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
	

	#centers, design
	#&&&&&&&&&&
	if (show_center) {
		if (!is.null(center_start_dates)) { 
		
			cdates<-c(csk$Date,cdate)
			centerw<-1
		
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
		
			if (is.na(center_colors[1])) {
				polygon(x=c(cdates[i],rep(cdates[i+1],2),cdates[i]),y=yp,
						xpd=TRUE,col="grey90",border="gray70")
			} else {
				cols<-rev(center_colors)
				if (length(center_colors)!=targetc) {
					warning(paste0("center_colors is not of length ",lc))
				}
				polygon(x=c(cdates[i],rep(cdates[i+1],2),cdates[i]),y=yp,
						xpd=TRUE,col=cols[nc],border="black")
			}
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
	}
}

#**********************************************************************************#

#' accrual_plot_cum
#'
#' Plot of cumulative recruitment based on accrual data frame produced by accrual_create_df
#'
#' @param accrual_df  accrual data frame produced by accrual_create_df potentially with by option (i.e. as a list)
#	  with by option, a line is added for each element in the list
#' @param ylim  limits for y-axis
#' @param xlim  limits for x-axis
#' @param ylab y-axis label
#' @param xlabn integer giving the desired number of intervals for the xlabel, default=5
#' @param xlabminn nonnegative integer giving the minimal number of intervals
#' @param xlabformat format of date on x-axis
#' @param xlabpos position of the x-label
#' @param xlabsrt rotation of x-axis labels in degrees
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment in x- and y-direction
#' @param xlabcex size of x-axis label
#' @param col color for line(s) in plot
#		if accrual_df is a list and overall is indicated, the first entry is used for the overall
#' @param lty line types in plot
#		if accrual_df is a list and overall is indicated, the first entry is used for the overall
#' @param legend.list named list with options passed to legend()
#' @param ... further options passed to plot() and axis()
#'
#' @return A plot of the cumulative accrual, optionally by site.
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

#' accrual_plot_abs
#'
#' Plot of absolute recruitment by time unit
#'
#' @param accrual_df accrual data frame produced by accrual_create_df (optionally with by option as a list)
#' @param unit time unit for which the bars should be plotted, any of "month","year","week","day"
#' @param target adds horizontal line for target recruitment per time unit
#' @param overall logical, indicates that accrual_df contains a summary with all sites 
#'		that should be removed from stacked barplot (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#' @param ylim limits for y-axis, numeric vector of length 2
#' @param xlim limits for x-axis, in barplot units, numeric vector of length 2
#' @param ylab y-axis label
#' @param xlabformat format of date on x-axis
#' @param xlabsel selection of x-labels if not all should be shown,
#'		 by default all are shown up to 15 bars, with more an automated selection is done,
#'		 either NA (default), NULL (show all), or a numeric vector
#' @param xlabpos position of the x-label
#' @param xlabsrt rotation of x-axis labels in degrees
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment
#' 		in x- and y-direction
#' @param xlabcex size of x-axis label
#' @param col colors of bars in barplot, can be a vector if accrual_df is a list, default is grayscale	
#' @param legend.list named list with options passed to legend()
#' @param ... further arguments passed to barplot() and axis()
#'
#' @return Barplot of absolute recruitment by time unit, stacked if accrual_df is a list.
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
                           name_overall="Overall",
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

