#' ggplot2 method for cumulative accrual plots
#' @rdname accrual_plot_cum
#'
#' @return \code{gg_accrual_plot_cum} returns an object of class 'ggplot' with the cumulative accrual.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_step vars scale_x_date labs
#' @importFrom rlang !! sym
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows rename mutate
#'
#' @examples
#' ### ggplot2 approach
#'
#' set.seed(2020)
#' enrollment_dates <-
#'   as.Date("2018-01-01") + sort(sample(1:30, 50, replace = TRUE))
#' accrual_df <- accrual_create_df(enrollment_dates)
#' gg_accrual_plot_cum(accrual_df)
#' gg_accrual_plot_cum(accrual_df) +
#'   ggplot2::theme_classic()
#'
#' #several sites
#' set.seed(1)
#' centers <-
#'   sample(c("Site 1", "Site 2", "Site 3"),
#'          length(enrollment_dates),
#'          replace = TRUE)
#' accrual_df <- accrual_create_df(enrollment_dates, by = centers)
#' gg_accrual_plot_cum(accrual_df)
#'
#' #assuming a common start and current date
#' accrual_df <-
#'   accrual_create_df(
#'     enrollment_dates,
#'     by = centers,
#'     start_date = "common",
#'     current_date = "common"
#'   )
#' gg_accrual_plot_cum(accrual_df)
#'
#' #without overall
#' accrual_df <-
#'   accrual_create_df(enrollment_dates, by = centers, overall = FALSE)
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
#' @return \code{gg_accrual_plot_abs} returns an object of class 'ggplot' with the absolute accrual by time unit.
#'
#' @export
#'
#' @importFrom ggplot2 geom_bar
#' @importFrom purrr map
#' @importFrom dplyr filter
#'
#' @examples
#' ### ggplot2 approach
#'
#' set.seed(2020)
#' enrollment_dates <-
#'   as.Date("2018-01-01") + sort(sample(1:100, 50, replace = TRUE))
#' accrual_df <- accrual_create_df(enrollment_dates)
#' gg_accrual_plot_abs(accrual_df, unit = "week")
#' gg_accrual_plot_abs(accrual_df, unit = "week") +
#'   ggplot2::theme_classic()
#'
#' #time unit
#' gg_accrual_plot_abs(accrual_df, unit = "day")
#'
#' #accrual_df with by option
#' set.seed(2020)
#' centers <-
#'   sample(c("Site 1", "Site 2", "Site 3"),
#'          length(enrollment_dates),
#'          replace = TRUE)
#' centers <- factor(centers, levels = c("Site 1", "Site 2", "Site 3"))
#' accrual_df <- accrual_create_df(enrollment_dates, by = centers)
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
	   rename('Date' = date) %>%
      ggplot(aes(x = Date, y = !!sym('Recruited participants'))) +
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
      rename('Recruited participants' = Freq)  %>%
	  rename('Date' = date)
	  
    out <- ggplot(x, aes(x = Date,
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
#' @return \code{gg_accrual_plot_predict} an object of class 'ggplot' with the accrual prediction.
#'
#' @export
#'
#' @importFrom ggplot2 geom_point geom_line annotation_custom ggtitle
#' @importFrom purrr pmap
#' @importFrom grid grobTree textGrob
#'
#' @examples
#' ### ggplot2 approach
#'
#' #Data
#' set.seed(2020)
#' enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
#'
#' #Default plot
#' accrual_df <- accrual_create_df(enrollment_dates)
#' gg_accrual_plot_predict(accrual_df = accrual_df, target = 100)
#' gg_accrual_plot_predict(accrual_df = accrual_df, target = 100) +
#'   ggplot2::theme_classic()
#'
#' #Include site
#' set.seed(2021)
#' centers<-sample(c("Site 1","Site 2","Site 3"),
#'                 length(enrollment_dates), replace=TRUE)
#' accrual_df<-accrual_create_df(enrollment_dates, by=centers)
#' gg_accrual_plot_predict(accrual_df=accrual_df, target=100)
#'
#'
#' #Format prediction end date
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'                         target=100,
#'                         pos_prediction="in",
#'                         format_prediction="%Y-%m-%d")
#'
#'
#' #predictions for all sites
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'                         target=c("Site 1"=30,"Site 2"=30,"Site 3"=30,"Overall"=100))
#' gg_accrual_plot_predict(accrual_df = accrual_df,
#'                         target=c("Site 1"=30,"Site 2"=30,"Site 3"=30,"Overall"=100)) +
#'    ggplot2::theme(legend.position = c(0.15,.9)) +
#'    ggplot2::labs(col = "Site")

gg_accrual_plot_predict <- function(accrual_df
                                    , target
                                    , overall = TRUE
                                    , name_overall = attr(accrual_df, "name_overall")
									, fill_up = TRUE
                                    , wfun = function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
                                    , col.pred = "red"
                                    , lty.pred = 2
                                    , pch.pred = 8
                                    , pos_prediction = c("out", "in", "none")
                                    , format_prediction = "%B %d, %Y"
                                    , xlabformat = "%d%b%Y"
                                    ){

  pos_prediction <- match.arg(pos_prediction)

  date <- cum <- NULL

  if(is_accrual_list(accrual_df)){
    if(length(target) > 1){
      if(length(target) != length(accrual_df)) {
		stop("target should have the same length as accrual_df")
	  } else {	
		target<-check_name(target, names(accrual_df))
	  }

    } else {
      accrual_df <- accrual_df[[name_overall]]
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
  end_date <- tmp$end_date
  edate <- tmp$edate
  adf <- tmp$adf

  if(is_accrual_list(accrual_df) & length(target) > 1){

    pdat <- pmap(list(accrual_df, end_date, target), function(x, y, z){
      data.frame(date = c(max(x$Date), y),
                         cum = c(max(x$Cumulative, na.rm = TRUE), z))
    })

    n <- 1
    l <- list()
    while(n <= length(target)){
      tmp <- pdat[[n]]
      l[[length(l)+1]] <- geom_line(data = tmp,
                                    mapping = aes(x = date, y = cum),
                                    col = col.pred,
                                    lty = lty.pred)
      l[[length(l)+1]] <- geom_point(data = tmp[2, ],
                                     aes(x = date, y = cum),
                                     col = col.pred,
                                     pch = pch.pred)

      names(accrual_df_o)[n] <- paste0(names(accrual_df_o)[n], ": ",
                                      format(end_date[[n]], format = format_prediction))

      n <- n + 1
    }

    pgeom <- function() l


  } else {

    pdat <- data.frame(date = c(max(adf$Date), edate),
                       cum = c(max(adf$Cumulative), target))
    pgeom <- function() {
      list(
        geom_line(data = pdat,
                  mapping = aes(x = date, y = cum),
                  col = col.pred,
                  lty = lty.pred),
        geom_point(aes(x = edate, y = target),
                   col = col.pred,
                   pch = pch.pred)
      )
    }
  }


  pred_text <- paste0("Predicted end date: ",
                      format(edate, format = format_prediction))

  out <- gg_accrual_plot_cum(accrual_df_o)

  out <- out + pgeom()

  if(length(target) == 1){
    if(pos_prediction == "out") out <- out + ggtitle(pred_text)
    if(pos_prediction == "in"){
      grob <- grobTree(textGrob(pred_text, x=0.025,  y=0.95, hjust=0))
      out <- out + annotation_custom(grob)
    }
  }

  suppressMessages(
    out <- out +
      scale_x_date(labels = function(x)format(x, format = xlabformat))
  )

  return(out)
}



