#' accrual_plot_predict
#'
#' Generates an accrual prediction plot based on vector with enrollment dates and a target sample size.
#' Optionally the enrolled and targeted sites can be included.
#'
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#'        or dates on which patients are enrolled as date or character vector
#' @param format_enrollment_dates single character identifying the format of the dates,
#'        ignored if accrual_df is a data frame or a date vector
#' @param target target sample size
#' @param start_date date when recruitment started, single character or date,
#'		if not given the first enrolment date is used as start_date
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date,
#'		if not given the latest enrolment date is used
#' @param format_current_date format of the current date, ignored if current_date is a date
#' @param fill_up whether to fill up days where no recruitment was observed,
#'		otherwise these points do not contribute to the regression, default is yes
#' @param wfun function to calculate the weights based on the accrual data frame, default is
#'		wfun<-function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
#' @param center_start_dates dates on which centers are enrolled as character or date vector,
#'		required if center information should be shown
#' @param format_center_start_dates format of the center start date, ignored if enrollment_dates are dates
#' @param targetc  target number of centers, required if center information should be shown
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
#' @param ylim limits for y-axis
#' @param xlim limits for x-axis
#' @param ylab y-axis label
#' @param xlabformat format of date on x-axis
#' @param xlabn integer giving the desired number of intervals for the xlabel, default=5
#' @param xlabminn nonnegative integer giving the minimal number of intervals
#' @param xlabformat format of date on x-axis
#' @param xlabpos position of the x-label
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

accrual_plot_predict<-function(accrual_df,
                               format_enrollment_dates="%d%b%Y",
                               target,
                               start_date=NA,
                               format_start_date="%d%b%Y",
                               current_date=NA,
                               format_current_date="%d%b%Y",
                               fill_up=c("yes","no"),
                               wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x)),
                               center_start_dates=NA,
                               format_center_start_dates="%d%b%Y",
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
                               ylim=NA,
                               xlim=NA,
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

  fill_up<-match.arg(fill_up)
  pos_prediction<-match.arg(pos_prediction)
  center_legend<-match.arg(center_legend)

  stopifnot(design>0 & design<=4)

  if (center_legend=="strip") {
    stopifnot(!is.na(center_colors[1]))
  }

  if (!is.na(sum(mar))) {
    stopifnot(length(mar)==4)
    par(mar=mar)
  }


  #via enrollment dates
  #....................

  if (!is.data.frame(accrual_df)) {

    if (!inherits(accrual_df,"Date")) {
      accrual_df<-as.Date(accrual_df,format=format_enrollment_dates)
    }


    #current date
    if (!is.na(current_date)) {
      if (!inherits(current_date,"Date")) {
        current_date<-as.Date(current_date,format=format_current_date)
      }
    } else {
      current_date<-max(accrual_df)
    }

    #start date

    if (!is.na(start_date)) {
      if (!inherits(start_date,"Date")) {
        start_date<-as.Date(start_date,format=format_start_date)
      }
    } else {
      start_date<-min(accrual_df)
    }

    #accrual data frame

    accrual_df<-accrual_create_df(accrual_df,
                                  format_enrollment_dates=format_enrollment_dates,
                                  start_date=start_date,format_start_date=format_start_date,
                                  current_date=current_date,format_current_date=format_current_date)

    accrual_df0<-accrual_df
    accrual_df0<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),accrual_df0)
    #accrual_df0<-accrual_create_df(enrollment_dates,
    #	format_enrollment_dates=format_enrollment_dates,
    #	start_date=start_date,format_start_date=format_start_date,
    #	current_date=current_date,format_current_date=format_current_date,
    #	force_start0="yes")

  } else {

    #via accrual_df
    #..............

    if (!is.na(current_date)) {
      if (!inherits(current_date,"Date")) {
        current_date<-as.Date(current_date,format=format_current_date)
      }
      stopifnot(current_date >= max(accrual_df$Date))

      if (current_date != max(accrual_df$Date)) {
        accrual_df<-rbind(accrual_df,
                          data.frame(Date=current_date,Freq=0,Cumulative=max(accrual_df$Cumulative)))
      }
    } else {
      current_date<-max(accrual_df$Date)
    }

    if (!is.na(start_date)) {
      if (!inherits(start_date,"Date")) {
        start_date<-as.Date(start_date,format=format_start_date)
      }
      stopifnot(start_date <= min(accrual_df$Date))

      if (start_date != min(accrual_df$Date))  {
        accrual_df<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),accrual_df)
        accrual_df0<-accrual_df
      }
    } else {
      start_date<-min(accrual_df$Date)
      accrual_df0<-accrual_df
      accrual_df0<-rbind(data.frame(Date=start_date,Freq=0,Cumulative=0),accrual_df0)
    }
  }

  #centers
  #.......

  if (!is.na(max(center_start_dates))) {

    if (inherits(center_start_dates,"Date")) {
      format_center_start_dates<-"%Y-%m-%d"
    }

    cs<-accrual_create_df(center_start_dates,format_enrollment_dates=format_center_start_dates)

    if (is.na(targetc)) {
      targetc<-max(cs$Cumulative)
    }

    #use start center if earlier than recruitment
    if (min(cs$Date)<start_date) {
      start_date<-min(cs$Date)
    }

  }



  #model and prediction
  #....................

  m1<-accrual_linear_model(accrual_df,
                           start_date=start_date,format_start_date=format_start_date,
                           current_date=current_date,format_current_date=format_current_date,
                           fill_up=fill_up,wfun=wfun)

  #from last recruited patient:
  end_date<-accrual_predict(accrual_df,m1,target,current_date=current_date,format_current_date="%d%b%Y")

  if (sum(!is.na(xlim))==0) {
    xlim<-c(min(accrual_df$Date),end_date)
  }
  xlabs<-pretty(x=xlim,n=xlabn,min.n=xlabminn)
  xlabs<-xlabs[xlabs>=xlim[1] & xlabs <=xlim[2]]

  #plot setup
  #..........

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

  if (!is.na(max(center_start_dates))) {

    if (design==1) {
      diffs<-c(cs$Date[-1],current_date)-cs$Date

      if (is.na(sum(mar))) {
        par(mar=c(5,4.3,margin_top,4))
      }

      b<-barplot(cs$Cumulative,space=0,width=as.numeric(diffs),
                 ylim=c(0,targetc),col="gray90",border="gray90",axes=FALSE,
                 xlim=c(0,max(end_date-cs$Date)))
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
        ymin<--target/15
      }

      if (is.na(sum(mar))) {
        par(mar=c(margin_bottom,4.3,margin_top,margin_right))
      }
    }
  }

  if (sum(!is.na(ylim))==0) {
    ylim<-c(ymin,target)
  }

  #plot raw data
  #.............

  if (!is.na(max(center_start_dates)) & design==1) {
    plot(0,type="n",ylim=ylim,xlim=xlim,
         axes=FALSE,xlab="",ylab=ylab,
         yaxs = "i",...)
    box(bty="c")
  } else {
    plot(0,type="n",ylim=ylim,xlim=xlim,
         axes=FALSE,xlab="",ylab=ylab,...)
    box()
  }

  #xlabel:
  xlabsl<-format(xlabs, xlabformat)
  axis(side=1,at=xlabs,labels=rep("",length(xlabs)),...)
  if (is.na(xlabpos)) {
    xlabpos<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
  }
  text(x=xlabs,y=xlabpos,srt=xlabsrt,labels=xlabsl,xpd=TRUE,adj=xlabadj,cex=xlabcex)

  #ylabel:
  axis(side=2,las=1,...)

  #lines
  #points(Cumulative~Date,data=accrual_df,cex=0.8)
  #lines(Cumulative~Date,data=accrual_df,type="s")
  lines(Cumulative~Date,data=accrual_df0,type="s")


  #plot model fit
  #..............

  lp<-accrual_df0[which.max(accrual_df0$Date),]
  lines(x=c(lp$Date,end_date),y=c(lp$Cumulative,target),col="red",lty=2)
  points(x=end_date,y=target,pch=8,col="red",xpd=TRUE)


  #predicted end date
  #..................
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
  #...................

  if (!is.na(max(center_start_dates)) & design!=1) {

    cdates<-c(cs$Date,current_date)
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
      nc<-cs$Cumulative[i]

      if (is.na(center_colors[1])) {
        polygon(x=c(cdates[i],rep(cdates[i+1],2),cdates[i]),y=yp,
                xpd=TRUE,col="grey90",border="gray70")
      } else {
        if (length(center_colors)==targetc) {
          cols<-rev(center_colors)
        } else {
          cols<-rev(heat.colors(targetc))
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
      text(x=td,y=mean(yp),labels=cs$Cumulative,xpd=TRUE,cex=legend_text_size)

    } else {

      bwidth<-centerw*y_off*lh
      pl<- 0.5 * x_off * lh
      lxp<-par("usr")[2] + pl/2
      ypp<-seq(yp[1],yp[3] + 2*bwidth ,l=targetc+1)
      if (design==3) {
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
      text(x=lxp+pl+2*tcks,y=ytck[,2],label=atc,xpd=TRUE,adj=0,cex=legend_text_size)

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
#' @param overall indicates that accrual_df contains a summary with all sites (only if by is not NA)
#' @param name_overall name of the summary with all sites (if by is not NA and overall==TRUE)
#' @param start_date start_date: date when recruitment started, single character or date,
#	  or "common" if the same date should be used for all sites,
#		if not given the first enrollment date is used as start_date
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date
#	  or "common" if the same date should be used for all sites,
#		if not given the latest enrollment date is used for each site
#' @param format_current_date format of the current date, ignored if current_date is a date
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
#' accrual_plot_cum(accrual_df,start_date="common",current_date="common")
#'
#' #plot and legend options
#' accrual_plot_cum(accrual_df,start_date="common",current_date="common",
#'      col=c("red",rep(1,3)),lty=c(1,1:3),cex.lab=1.2,cex.axis=1.1,xlabcex=1.1)
#' accrual_plot_cum(accrual_df,legend.list=list(ncol=2,bty=TRUE,cex=0.8))
#'
#' #without overall
#' accrual_df<-accrual_create_df(enrollment_dates,by=centers,overall=FALSE)
#' accrual_plot_cum(accrual_df,start_date="common",current_date="common",overall=FALSE)
#'

accrual_plot_cum<-function(accrual_df,
                           overall=TRUE,
                           name_overall="Overall",
                           start_date=NA,
                           format_start_date="%d%b%Y",
                           current_date=NA,
                           format_current_date="%d%b%Y",
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
    stopifnot((is.data.frame(accrual_df[[1]])))
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

  if (!is.na(start_date)) {
    if (inherits(start_date,"Date")) {
      sdate<-start_date-0.00001
    } else {
      if (start_date=="common") {
        sdate<-min(do.call("c",lapply(accrual_df,function(x) min(x$Date))))-0.00001
      } else {
        sdate<-as.Date(start_date,format=format_start_date)-0.00001
      }
    }
    for (i in 1:lc) {
      if (sdate != min(accrual_df[[i]]$Date))  {
        stopifnot(sdate <= min(accrual_df[[i]]$Date))
        accrual_df[[i]]<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),accrual_df[[i]])
      }
    }
  } else {
    for (i in 1:lc) {
      sdate<-min(accrual_df[[i]]$Date)-0.00001
      accrual_df[[i]]<-rbind(data.frame(Date=sdate,Freq=0,Cumulative=0),accrual_df[[i]])
    }
  }

  if (!is.na(current_date)) {
    if (inherits(current_date,"Date")) {
      end_date<-current_date
    } else {
      if (current_date=="common") {
        end_date<-max(do.call("c",lapply(accrual_df,function(x) max(x$Date))))
      } else {
        end_date<-as.Date(current_date,format=format_current_date)
      }
    }
    for (i in 1:lc) {
      if (end_date != max(accrual_df[[i]]$Date)) {
        stopifnot(end_date > max(accrual_df[[i]]$Date))
        accrual_df[[i]]<-rbind(accrual_df[[i]],
                               data.frame(Date=end_date,Freq=0,Cumulative=max(accrual_df[[i]]$Cumulative)))
      }
    }
  }


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
#' @param accrual_df accrual data frame produced by accrual_create_df with by=NA
#' @param unit time unit for which the bars should be plotted, any of "month","year","week","day"
#' @param target adds horizontal line for target recruitment per time unit
#' @param start_date start_date: date when recruitment started, single character or date,
#		if not given the first enrollment date is used as start_date
#' @param format_start_date format of the start date, ignored if start_date is a date
#' @param current_date date of the data export or database freeze, single character or date
#		if not given the latest enrollment date is used for each site
#' @param format_current_date format of the current date, ignored if current_date is a date
#' @param ylim limits for y-axis
#' @param xlim limits for x-axis, in barplot units
#' @param ylab y-axis label
#' @param xlabformat format of date on x-axis
#' @param xlabsel selection of x-labels if not all should be shown,
#		 by default all are shown up to 15 bars, with more an automated selection is done,
#		 either NA (default), NULL (show all), or a numeric vector
#' @param xlabpos position of the x-label
#' @param xlabsrt rotation of x-axis labels in degrees
#' @param xlabadj adjustment of x-label, numeric vector with length 1 or 2 for different adjustment
#'   in x- and y-direction
#' @param xlabcex size of x-axis label
#' @param col colors of bars in barplot
#' @param ... further arguments passed to barplot() and axis()
#'
#' @return Barplot of absolute recruitment by time unit.
#'
#' @export
#'
#' @importFrom graphics abline axis barplot box grconvertX grconvertY legend lines mtext par points polygon text
#'
#' @examples
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#' accrual_df<-accrual_create_df(enrollment_dates)
#' accrual_plot_abs(accrual_df,unit="week",xlabformat="%d%b%Y")
#'
#' #time unit
#' accrual_plot_abs(accrual_df,unit="day",xlabformat="%d%b%Y")
#'
#' #include target
#' accrual_plot_abs(accrual_df,unit="week",xlabformat="%d%b%Y",target=10)
#'
#' #different start and current dates
#' accrual_plot_abs(accrual_df,unit="week",xlabformat="%d%b%Y",target=10,
#'    start_date=as.Date("2017-12-01"),current_date=as.Date("2018-03-01"))
#'
#' #further plot options
#' accrual_plot_abs(accrual_df,unit="week",ylab="No of recruited patients",
#'    xlabformat="%Y-%m-%d",xlabsrt=30,xlabpos=-0.8,xlabadj=c(1,0.5),
#'    col="pink",tck=-0.03,mgp=c(3,1.2,0))
#'
#'
accrual_plot_abs<-function(accrual_df,
                           unit=c("month","year","week","day"),
                           target=NA,
                           start_date=NA,
                           format_start_date="%d%b%Y",
                           current_date=NA,
                           format_current_date="%d%b%Y",
                           ylim=NA,
                           xlim=NA,
                           ylab="Recruited patients",
                           xlabformat="%b %Y",
                           xlabsel=NA,
                           xlabpos=NA,
                           xlabsrt=45,
                           xlabadj=c(1,1),
                           xlabcex=1,
                           col="grey",...) {

  unit<-match.arg(unit)

  #add start or end date
  if (!is.na(start_date)) {
    if (inherits(start_date,"Date")) {
      sdate<-start_date
    } else {
      sdate<-as.Date(start_date,format=format_start_date)
    }
    if (sdate != min(accrual_df$Date)) {
      stopifnot(sdate < min(accrual_df$Date))
    }
  } else {
    sdate<-min(accrual_df$Date)
  }


  if (!is.na(current_date)) {
    if (inherits(current_date,"Date")) {
      edate<-current_date
    } else {
      edate<-as.Date(current_date,format=format_current_date)
    }
    if (edate != max(accrual_df$Date)) {
      stopifnot(edate > max(accrual_df$Date))
    }
  } else {
    edate<-max(accrual_df$Date)
  }

  #summarize data by time unit
  dfit<-accrual_time_unit(accrual_df,unit=unit,start_date=sdate,current_date=edate)

  if (sum(!is.na(ylim))==0) {
    ylim<-c(0,max(dfit$Freq,target,na.rm=TRUE)+1)
  }

  #xscale
  if (sum(!is.na(xlim))==0) {
    b<-barplot(dfit$Freq,plot=FALSE)
    xlim<-c(min(b),max(b)) + c(-0.5,0.5)
  }

  #x label selection
  if (is.null(xlabsel)) {
    sel<-1:nrow(b)
  } else {
    if (sum(!is.na(xlabsel))==0) {
      if (nrow(b)>15) {
        sel<-round(seq(1,nrow(b),l=8))
        sel<-sel[sel>0&sel<=nrow(b)]
      } else {
        sel<-1:nrow(b)
      }
    } else {
      sel<-xlabsel
    }
  }

  #plot
  b<-barplot(dfit$Freq,ylab=ylab,
             ylim=ylim,axes=FALSE,xlim=xlim,col=col,...)
  box()
  axis(side=2,las=2,...)
  axis(side=1,at=b,labels=rep("",length(b)),...)

  #xlabel
  if (is.na(xlabpos)) {
    xlabpos<-par("usr")[3]-(par("usr")[4]-par("usr")[3])/30
  }
  sel<-sel[sel<=nrow(b)]
  bu<-b[sel,]
  lab<-format(dfit$date,xlabformat)
  lab<-lab[sel]
  text(x=bu,y=xlabpos,srt=xlabsrt,labels=lab,xpd=TRUE,adj=xlabadj,cex = xlabcex)

  #line for target
  if (!is.na(target)) {
    abline(h=target,lty=2)
  }

}
