#' ggplot2 method for cumulative accrual plots
#' @rdname accrual_plot_cum
#'
#' @return ggplot2 object
#' @details When the \code{accrual_df} includes multiple sites, the dataframe
#' passed to \code{ggplot} includes a \code{site} variable
#' which can be used for faceting
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_step vars scale_x_date labs
#' @importFrom rlang !! sym
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows rename mutate
#' @examples
#' ### ggplot2 approach
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' gg_accrual_plot_cum(accrual_df)
#' gg_accrual_plot_cum(accrual_df) +
#'   ggplot2::theme_classic()
#'
#' #several sites
#' accrual_df <- accrual_create_df(accrualdemo$date, by = accrualdemo$site)
#' gg_accrual_plot_cum(accrual_df)
#'
#' #assuming a common start and current date
#' accrual_df <-
#'   accrual_create_df(
#'     accrualdemo$date,
#'     by = accrualdemo$site,
#'     start_date = "common",
#'     current_date = "common"
#'   )
#' gg_accrual_plot_cum(accrual_df)
#'
#' #without overall
#' accrual_df <-
#'   accrual_create_df(accrualdemo$date, by = accrualdemo$site, overall = FALSE)
#' gg_accrual_plot_cum(accrual_df)


gg_accrual_plot_cum <- function(accrual_df, xlabformat="%d%b%Y"){

  Date <- site <- Cumulative <- NULL

  if("data.frame" %in% class(accrual_df)){

    out <- accrual_df %>%
      rename('Recruited participants' = Cumulative) %>%
      ggplot(aes(x = Date, y = !!sym('Recruited participants'))) +
      geom_step()

  }

  if("list" %in% class(accrual_df)){

    x <- accrual_df
    class(x) <- class(x)[2]
    out <- x %>% map2(names(x), function(.x, .y){
      .x$site <- .y
      .x
    }) %>%
      bind_rows() %>%
      rename('Recruited participants' = Cumulative) %>%
      mutate(site = factor(site, unique(site), unique(site))) %>%
      ggplot(aes(x = Date, y = !!sym('Recruited participants'), col = site)) +
      geom_step() +
      labs(col = NULL)


  }

  out <- out +
    scale_x_date(labels = function(x)format(x, format = xlabformat))

  return(out)

}



#' ggplot2 method for absolute accrual plots
#' @rdname accrual_plot_abs
#'
#' @return ggplot object
#' @details When the \code{accrual_df} includes multiple sites, the dataframe
#' passed to \code{ggplot} includes a \code{site} variable
#' which can be used for facetting
#' @export
#' @importFrom ggplot2 geom_bar
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @examples
#' ### ggplot2 approach
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' gg_accrual_plot_abs(accrual_df, unit = "week")
#' gg_accrual_plot_abs(accrual_df, unit = "week") +
#'   ggplot2::theme_classic()
#'
#' #time unit
#' gg_accrual_plot_abs(accrual_df, unit = "day")
#'
#' #accrual_df with by option
#' accrual_df <- accrual_create_df(accrualdemo$date, by = accrualdemo$site)
#' gg_accrual_plot_abs(accrual_df = accrual_df, unit = "week")
#' gg_accrual_plot_abs(accrual_df = accrual_df, unit = "week") +
#'   ggplot2::scale_fill_discrete(type = c("black", "red", "blue", "green"))
#'
gg_accrual_plot_abs <- function(accrual_df
                                , unit = c("month","year","week","day")
                                , xlabformat = NULL
                                ){

  unit <- match.arg(unit)
  Date <- site <- Freq <- date <- NULL

  #default xlabformat
  if (is.null(xlabformat)) {
    if (unit=="month") {xlabformat<-"%b %Y"}
    if (unit=="year") {xlabformat<-"%Y"}
    if (unit=="week") {xlabformat<-"%d %b %Y"}
    if (unit=="day") {xlabformat<-"%d %b %Y"}
  }

  if("data.frame" %in% class(accrual_df)){
    x <- accrual_df

    out <- accrual_time_unit(accrual_df, unit=unit)%>%
      rename('Recruited participants' = Freq) %>%
      ggplot(aes(x = date, y = !!sym('Recruited participants'))) +
      geom_bar(stat = "identity")

  }

  if("list" %in% class(accrual_df)){

    x <- accrual_df
    class(x) <- class(x)[2]
    x <- x %>%
      map(accrual_time_unit, unit = unit) %>%
      map2(names(x), function(.x, .y){
          .x$site <- .y
          .x
        }) %>%
      bind_rows() %>%
      filter(site != "Overall") %>%
      mutate(site = factor(site, names(accrual_df), names(accrual_df))) %>%
      rename('Recruited participants' = Freq)
    out <- ggplot(x, aes(x = date,
                         y = !!sym('Recruited participants'),
                         fill = site)) +
      geom_bar(stat = "identity") +
      labs(fill = NULL)

  }

  out <- out +
    scale_x_date(labels = function(x)format(x, format = xlabformat))

  return(out)
}

#' ggplot2 method for accrual prediction plots
#' @rdname accrual_plot_predict
#'
#' @return ggplot object
#' @details When the \code{accrual_df} includes multiple sites, the dataframe
#' passed to \code{ggplot} includes a \code{site} variable
#' which can be used for facetting
#'
#' @export
#' @importFrom ggplot2 geom_point geom_line annotation_custom ggtitle
#' @importFrom purrr pmap
#' @importFrom grid grobTree textGrob
#' @examples
#' ### ggplot2 approach
#' data(accrualdemo)
#' accrual_df<-accrual_create_df(accrualdemo$date)
#' gg_accrual_plot_predict(accrual_df = accrual_df, target = 300)
#' gg_accrual_plot_predict(accrual_df = accrual_df, target = 300) +
#'   ggplot2::theme_classic()
#'
#' #Include site
#' accrual_df<-accrual_create_df(accrualdemo$date, by=accrualdemo$site)
#' gg_accrual_plot_predict(accrual_df=accrual_df, target=300)
#'
#'
#' #Format prediction end date
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'	target=300,
#'	pos_prediction="in",
#'	format_prediction="%Y-%m-%d")
#'
#'
#' #predictions for all sites
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'	target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300))
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'  target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300)) +
#' 	ggplot2::theme(legend.position = c(0.15,.9)) +
#' 	ggplot2::labs(col = "Site")

gg_accrual_plot_predict <- function(accrual_df
                                    , target
                                    , overall = TRUE
                                    , name_overall = attr(accrual_df, "name_overall")
                                    , col.pred = "red"
                                    , lty.pred = 2
                                    , pch.pred = 8
                                    , fill_up = TRUE
                                    , wfun = function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
                                    , pos_prediction = c("out", "in", "none")
									, label_prediction=NULL
                                    , format_prediction = "%B %d, %Y"
                                    , xlabformat = "%d%b%Y"
                                    ){

  pos_prediction <- match.arg(pos_prediction)

  date <- cum <- NULL

  if(is_accrual_list(accrual_df)){
    if(length(target) > 1){
      if(length(target) != length(accrual_df)) stop("target should have the same length as accrual_df")

    } else {
      accrual_df <- accrual_df[[1]]
    }
  }
  preddate<-TRUE
  if (is.Date(target)) {
    preddate<-FALSE
	check_date(target)
  }
	
  if (is.null(label_prediction)) {
	if (preddate) {
	  label_prediction<-"Predicted end date: "
	} else {
	  label_prediction<-"Predicted sample size: "
	}
  }
	
  accrual_df_o <- accrual_df
  tmp <- lc_lct(accrual_df,
                overall,
                name_overall)
  accrual_df <- tmp$accrual_df
  lc <- tmp$lc
  lct <- tmp$lct
  overall <- tmp$overall

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

  if(is_accrual_list(accrual_df) & length(target) > 1){

    pdat <- pmap(list(accrual_df, end_date, targetm), function(x, y, z){
      data.frame(date = c(max(x$Date), y),
                         cum = c(max(x$Cumulative, na.rm = TRUE), z))
    })

    n <- 1
    l <- list()
    while(n <= length(target)){
      tmp <- pdat[[n]]
	  if (is.finite(as.numeric(tmp$date[2]))) {
		  
		  l[[length(l)+1]] <- geom_line(data = tmp,
										mapping = aes(x = date, y = cum),
										col = col.pred,
										lty = lty.pred)
		  l[[length(l)+1]] <- geom_point(data = tmp[2, ],
										 aes(x = date, y = cum),
										 col = col.pred,
										 pch = pch.pred)
		  if (preddate) {
		    names(accrual_df_o)[n] <- paste0(names(accrual_df_o)[n], ": ",
										  format(end_date[[n]], format = format_prediction))
		  } else {
		    names(accrual_df_o)[n] <- paste0(names(accrual_df_o)[n], ": ",
										  round(targetm[n], digits = 0))
			}		  
		 							  
		}
	  n <- n + 1			
    }

    pgeom <- function() l


  } else {

    pdat <- data.frame(date = c(max(adf$Date), edate),
                       cum = c(max(adf$Cumulative), targetm))
    pgeom <- function() {
      list(
        geom_line(data = pdat,
                  mapping = aes(x = date, y = cum),
                  col = col.pred,
                  lty = lty.pred),
        geom_point(aes(x = edate, y = targetm),
                   col = col.pred,
                   pch = pch.pred)
      )
    }
  }

  if (preddate) {
	pred_text <- paste0(label_prediction,format(edate, format = format_prediction))
  } else {
	pred_text <- paste0(label_prediction,round(targetm, digits = 0))
  }
  
  out <- gg_accrual_plot_cum(accrual_df_o, xlabformat)

  out <- out + pgeom()

  if(length(target) == 1){
    if(pos_prediction == "out") out <- out + ggtitle(pred_text)
    if(pos_prediction == "in"){
      grob <- grobTree(textGrob(pred_text, x=0.025,  y=0.95, hjust=0))
      out <- out + annotation_custom(grob)
    }
  }

  return(out)
}



