#' accrual_plot_predict
#'
#' Generates an accrual predction plot based on vector with enrollment dates and a target sample size.
#' Optionally the enrolled and targeted sites can be included.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df (optionally with by option as a list)
#'        or dates on which patients are enrolled as date or character vector
#' @param target target sample size, can be a vector with the same length as accrual_df
#' @param start_date date when recruitment started,
#'		single character/date, or a vector with the same length as accrual_df,
#'		if not given the first enrollment date is used as start_date,
#' @param current_date date of the data export or database freeze,
#'		single character/date, or a vector with the same length as accrual_df,
#'		if not given the latest enrollment date is used,
#' @param fill_up whether to fill up days where no recruitment was observed,
#'		otherwise these points do not contribute to the regression, default is yes
#' @param wfun function to calculate the weights based on the accrual data frame, default is
#'		wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
#' @param center_start_dates dates on which centers are enrolled as character or date vector,
#'		required if center information should be shown
#' @param targetc  target number of centers, required if center information should be shown,
#'		can be a vector with the same length as accrual_df
#' @param center_label label for the center strip od axis
#' @param design design options for the addition of the center
#'		1: barplots, 2 (default): strip below plot, 3: strip within plot, above line, 4: strip within plot, below line
#' @param center_colors colors to be used for the strip with the centers, a vector of length targetc
#' @param center_legend either "number" to plot numbers in the center strip or "strip" to add a legend strip,
#'		requires specification of center_colors
#' @param legend_text_size  size of the text of the center or legend strip, only has a function
#		if center and center_legend is specified
#' @param pos_prediction position of text with predicted end date, out, in or none
#' @param label_prediction label for predicted end date
#' @param cex_prediction text size for predicted end date
#' @param format_prediction date format for predicted end date
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
#' @param ... further options passed to plot() and axis()
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
#' accrual_plot_predict(accrual_df=enrollment_dates,target=100)
#'
#' #Include site
#' accrual_plot_predict(accrual_df=accrual_df,target=100,
#'      center_start_dates=center_start_dates,targetc=10,center_label="Site")
#' accrual_plot_predict(accrual_df=accrual_df,target=100,
#'      center_start_dates=center_start_dates,targetc=10,
#'      center_colors=heat.colors(10),center_legend="strip")
#'
#' #Design for site
#' accrual_plot_predict(accrual_df=accrual_df,target=100,
#'      center_start_dates=center_start_dates,targetc=10,design=1)
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
#' #accrual_df with by option
#' set.seed(2020)
#' centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers)
#' par(mfcol=c(2,2))
#' accrual_plot_predict(accrual_df=accrual_df,target=c(30,30,30,100))
#'

accrual_plot_predict<-function(accrual_df,
                               target,
                               start_date=NA,
                               current_date=NA,
                               fill_up=TRUE,
                               wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)),
                               center_start_dates=NA,
                               targetc=NA,
                               center_label="Centers",
                               design=2,
                               center_colors=NA,
                               center_legend=c("number","strip"),
                               legend_text_size=0.7,
                               pos_prediction=c("out","in","none"),
                               label_prediction="Predicted end date: ",
                               cex_prediction=1.1,
                               format_prediction="%B %d, %Y",
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
                               ...) {

  # fill_up<-match.arg(fill_up)
  pos_prediction<-match.arg(pos_prediction)
  center_legend<-match.arg(center_legend)
  if (!is.na(start_date)) check_date(start_date)
  if (!is.na(current_date)) check_date(current_date)

  stopifnot(design>0 & design<=4)

  if (center_legend=="strip") {
    stopifnot(!is.na(center_colors[1]))
  }

  if (!is.na(sum(mar))) {
    stopifnot(length(mar)==4)
    par(mar=mar)
  }


  #via enrollment dates
  #&&&&&&&&&&
  if (mode(accrual_df) %in% c("logical","numeric","complex","character")) {

    #current date
    if (is.na(current_date)) {
      current_date<-max(accrual_df)
    }

    #start date
    if (is.na(start_date)) {
      start_date<-min(accrual_df)
    }

    #accrual data frame
    accrual_df<-accrual_create_df(accrual_df,
                                  start_date=start_date,
                                  current_date=current_date
                                  )

    accrual_df0<-accrual_df
    accrual_df0<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),accrual_df0)

	accrual_df<-list(accrual_df)
	accrual_df0<-list(accrual_df0)

  } else {

    #via accrual_df
    #&&&&&&&&&&

    if (is.data.frame(accrual_df)) {
	  accrual_df<-list(accrual_df)
    }

	mult<-function(var) {
		if (length(var)==1) {
			var<-rep(var,length(accrual_df))
			return(var)
		} else {
			stopifnot(length(var)==length(accrual_df))
			return(var)
		}
	}
	target<-mult(target)
	targetc<-mult(targetc)
	current_date<-mult(current_date)
	start_date<-mult(start_date)
	xlabpos<-mult(xlabpos)

	if (mode(xlim) %in% c("logical","numeric","complex","character")) {
		xlim<-rep(list(xlim),length(accrual_df))
	} else {
		stopifnot(length(xlim)==length(accrual_df))
	}
	if (mode(ylim) %in% c("logical","numeric","complex","character")) {
		ylim<-rep(list(ylim),length(accrual_df))
	} else {
		stopifnot(length(ylim)==length(accrual_df))
	}

	accrual_df0<-vector(mode = "list", length = length(accrual_df))

	for (i in 1:length(accrual_df)) {

	  accrual_dfi<-accrual_df[[i]]

	  if (!is.na(current_date[i])) {
	    cdate<-current_date[i]
	    stopifnot(cdate >= max(accrual_dfi$Date))

	    if (cdate != max(accrual_dfi$Date)) {
	      accrual_dfi<-rbind(accrual_dfi,
	                         data.frame(Date=cdate,Freq=0,Cumulative=max(accrual_dfi$Cumulative)))
	    }
	  } else {
	    cdate<-max(accrual_dfi$Date)
	  }

	  if (!is.na(start_date[i])) {
	    sdate<-start_date[i]

	    stopifnot(sdate <= min(accrual_dfi$Date))

	    if (sdate != min(accrual_dfi$Date))  {
	      accrual_dfi<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),accrual_dfi)
	      adf0<-accrual_dfi
	    } else {
	      adf0<-accrual_dfi
	      adf0<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),adf0)
	    }
	  } else {
	    sdate<-min(accrual_dfi$Date)
	    adf0<-accrual_dfi
	    adf0<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),adf0)
	  }
	  accrual_df[[i]]<-accrual_dfi
	  accrual_df0[[i]]<-adf0
	}
  }

  #centers
#&&&&&&&&&&

  cs<-vector(mode = "list", length = length(accrual_df))
  for (i in 1:length(accrual_df)) {
	  if (mode(center_start_dates) %in% c("logical","numeric","complex","character")) {
	  	center_start_datesi<-center_start_dates
	  } else {
	  	stopifnot(length(accrual_df)==length(center_start_dates))
	  	center_start_datesi<-center_start_dates[[i]]
	  }

    if (!is.na(max(center_start_datesi))) {

  		csi<-accrual_create_df(center_start_datesi)
  		cs[[i]]<-csi

  		if (is.na(targetc[i])) {
  		  targetc[i]<-max(csi$Cumulative)
  		}
  	}
  }

  for (k in 1:length(accrual_df)) {

	  accrual_dfi<-accrual_df[[k]]
	  accrual_df0i<-accrual_df0[[k]]
	  csk<-cs[[k]]

	  if (is.na(start_date[k])) {
		sdate<-min(accrual_dfi$Date)
	  } else {
		sdate<-start_date[k]
	  }
	  if (!is.null(cs[[k]])) {
		  #use start center if earlier than recruitment
		  if (min(csk$Date) < sdate) {
			sdate<-min(csk$Date)
		  }
	  }

	  if (is.na(current_date[k])) {
		cdate<-max(accrual_dfi$Date)
	  } else {
		cdate<-current_date[k]
	  }

	  #model and prediction
	  #&&&&&&&&&&

	  m1<-accrual_linear_model(accrual_dfi,
	                           start_date=sdate,
	                           current_date=cdate,
	                           fill_up=fill_up,
	                           wfun=wfun)

	  #from last recruited patient:
	  end_date<-accrual_predict(accrual_dfi,m1,target[k],current_date=cdate)

	  if (sum(!is.na(xlim[[k]]))==0) {
		xlimk<-c(min(accrual_dfi$Date),end_date)
		xlabs<-pretty(x=xlimk,n=xlabn,min.n=xlabminn)
		xlimk<-c(min(xlimk,xlabs),max(xlimk,xlabs))
	  } else {
		xlimk<-xlim[[k]]
		xlabs<-pretty(x=xlimk,n=xlabn,min.n=xlabminn)
	    xlabs<-xlabs[xlabs>=xlimk[1] & xlabs <=xlimk[2]]
	  }

	  #plot setup
	  #&&&&&&&&&&

	  ymin<-0

	  if (pos_prediction %in% c("in","none")) {
		margin_top<-1
	  } else {
		margin_top<-2
	  }
	  if (is.na(sum(mar))) {
		par(mar=c(5,4.3,margin_top,1))
	  }

	  #centers
	  if (!is.null(cs[[k]])) {

		if (design==1) {
		  diffs<-c(csk$Date[-1],cdate)-csk$Date

		  if (is.na(sum(mar))) {
			par(mar=c(5,4.3,margin_top,4))
		  }

		  b<-barplot(csk$Cumulative,space=0,width=as.numeric(diffs),
					 ylim=c(0,targetc[k]),col="gray90",border="gray90",axes=FALSE,
					 xlim=c(0,max(end_date-csk$Date)))
		  axis(side=4,las=1,col="gray60",col.axis="gray60")
		  mtext(center_label,side=4,cex=1.2,line=2.2,col="gray70")

		  par(new=T)
		} else {

		  margin_bottom<-5

		  if (center_legend=="number") {
			margin_right<-1
		  } else {
			margin_right<-2.5
		  }

		  if (design==2) {
			margin_bottom<-6.5
		  }

		  if (design==4) {
			ymin<--target[k]/15
		  }

		  if (is.na(sum(mar))) {
			par(mar=c(margin_bottom,4.3,margin_top,margin_right))
		  }
		}
	  }

	  if (sum(!is.na(ylim[[k]]))==0) {
		ylimk<-c(ymin,target[k])
	  } else {
		ylimk<-ylim[[k]]
	  }

	  #plot raw data
	  #&&&&&&&&&&

	  if (!is.null(cs[[k]]) & design==1) {
		plot(0,type="n",ylim=ylimk,xlim=xlimk,
			 axes=FALSE,xlab="",ylab=ylab,
			 yaxs = "i",...)
		box(bty="c")
	  } else {
		plot(0,type="n",ylim=ylimk,xlim=xlimk,
			 axes=FALSE,xlab="",ylab=ylab,...)
		box()
	  }

	  #xlabel:
	  xlabsl<-format(xlabs, xlabformat)
	  axis(side=1,at=xlabs,labels=rep("",length(xlabs)),...)
	  if (is.na(xlabpos[k])) {
		xlabposk<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
	  } else {
		xlabposk<-xlabpos[k]
	  }
	  text(x=xlabs,y=xlabposk,srt=xlabsrt,labels=xlabsl,xpd=TRUE,adj=xlabadj,cex=xlabcex)

	  #ylabel:
	  axis(side=2,las=1,...)

	  #lines
	  #points(Cumulative~Date,data=accrual_dfi,cex=0.8)
	  #lines(Cumulative~Date,data=accrual_dfi,type="s")
	  lines(Cumulative~Date,data=accrual_df0i,type="s")


	  #plot model fit
	  #&&&&&&&&&&

	  lp<-accrual_df0i[which.max(accrual_df0i$Date),]
	  lines(x=c(lp$Date,end_date),y=c(lp$Cumulative,target[k]),col="red",lty=2)
	  points(x=end_date,y=target[k],pch=8,col="red",xpd=TRUE)


	  #predicted end date
	  #&&&&&&&&&&
	  if (pos_prediction!="none") {
		if (pos_prediction=="in") {
		  legend("topleft",paste0(label_prediction,format(end_date, format_prediction)),bty="n",
				 cex=cex_prediction)
		} else {
		  text(x=par("usr")[1],y=par("usr")[4],adj=c(0,-1), xpd=TRUE,
			   paste0(label_prediction,format(end_date, format_prediction)),cex=cex_prediction)
		}
	  }

	  #centers, design 2-4
	  #&&&&&&&&&&

	  if (!is.null(cs[[k]]) & design!=1) {

		cdates<-c(csk$Date,cdate)
		centerw<-1

		#coordinates for plotting
		uc<-par("usr")
		lh <- par('cin')[2] * par('cex') * par('lheight')
		x_off <- diff(grconvertX(0:1, 'inches', 'user'))
		y_off <- diff(grconvertY(0:1, 'inches', 'user'))
		bwidth<-centerw*y_off*lh
		ypf<-function(yp1) {c(rep(yp1,2),rep(yp1 + bwidth,2))} #get position for barplot

		if (design==2) {

		  yp1<-uc[3] - par("mar")[1] * y_off*lh #at the bottom
		  yp1<-uc[3] - (par("mar")[1]-0.4) * y_off*lh #0.4 lines above the bottom
		  yp<-ypf(yp1)
		  ypl<-mean(yp)
		  xpl<-cdates[1]-(uc[2]-uc[1])/50
		  xadj<-1
		  label<-center_label
		}

		if (design==3) {
		  yp1<-0.85*uc[4]
		  yp<-ypf(yp1)
		  ypl<-1.03*max(yp)
		  xpl<-cdates[1]
		  xadj<-0
		  label<-center_label
		}

		if (design==4) {
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
			if (length(center_colors)==targetc[k]) {
			  cols<-rev(center_colors)
			} else {
			  cols<-rev(heat.colors(targetc[k]))
			  print("Length of center_colors does not correpsong to targetc, default scheme used")
			}
			polygon(x=c(cdates[i],rep(cdates[i+1],2),cdates[i]),y=yp,
					xpd=TRUE,col=cols[nc],border="black")
		  }
		}

		#legend
		text(x=xpl,y=ypl,labels=label,adj=xadj,xpd=TRUE)

		if (center_legend=="number") {
		  td<-(as.numeric(cdates)[-length(cdates)]+as.numeric(cdates)[-1])/2
		  text(x=td,y=mean(yp),labels=csk$Cumulative,xpd=TRUE,cex=legend_text_size)

		} else {

		  bwidth<-centerw*y_off*lh
		  pl<- 0.5 * x_off * lh
		  lxp<-par("usr")[2] + pl/2
		  ypp<-seq(yp[1],yp[3] + 2*bwidth ,l=targetc[k]+1)
		  if (design==3) {
			ypp<-seq(yp[1]- bwidth,yp[3] + bwidth,l=targetc[k]+1)
		  }
		  atc<-round(seq(1,targetc[k],l=5))
		  ypatc<-(ypp[atc]+ypp[atc+1])/2
		  tcks<-pl/5
		  xtck<-matrix(rep(c(lxp+pl,lxp+pl+tcks),length(atc)),length(atc),2,byrow=TRUE)
		  ytck<-matrix(rep(ypatc,each=2),length(atc),2,byrow=TRUE)

		  for (i in 1:targetc[k]) {
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
		  text(x=lxp+pl+2*tcks,y=ytck[,2],label=atc,xpd=TRUE,adj=0,cex=legend_text_size)

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
#' @param overall logical, indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
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
#' accrual_plot_cum(accrual_df,overall=FALSE)
#'

accrual_plot_cum<-function(accrual_df,
                           overall=TRUE,
                           name_overall="Overall",
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
    overall<-FALSE
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

  if (lc>1 & overall==TRUE) {
    if (is.null(accrual_df[[name_overall]])) {
      print(paste0("'",name_overall,"' not found in accrual_df, overall set to FALSE"))
      overall<-FALSE
    }
  }

  if (overall) {
    lc<-length(accrual_df)-1
    adf<-accrual_df[[name_overall]]
    alim<-ascale(adf,xlim=xlim,ylim=ylim,ni=xlabn,min.n=xlabminn)
    lna<-names(accrual_df)
    lna<-c(name_overall,lna[lna!=name_overall])
  } else {
    alim<-ascale(accrual_df,xlim=xlim,ylim=ylim,ni=xlabn,min.n=xlabminn)
    lna<-names(accrual_df)
  }


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
    if (!overall) {
      lines(Cumulative~Date,data=dfi,col=col[i],lty=lty[i],type="s")
    } else {
      if (names(accrual_df)[[i]]!=name_overall) {
        lines(Cumulative~Date,data=dfi,col=col[i+1],lty=lty[i+1],type="s")
      }
    }
  }
  if (overall) {
    lines(Cumulative~Date,data=accrual_df[[name_overall]],col=col[1],lty=lty[1],type="s")
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
		accrual_df<-accrual_df[names(accrual_df)!=name_overall]
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

