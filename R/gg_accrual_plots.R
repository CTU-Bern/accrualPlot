
library(ggplot2)
library(magrittr)
library(dplyr)
library(purrr)
library(ggrepel)

#' Title
#'
#' @param accrual_df
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot aes geom_step +.gg
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#'
#' @examples
gg_accrual_plot_cum <- function(accrual_df){

  if("data.frame" %in% class(accrual_df)){

    out <- ggplot(accrual_df, aes(x = Date, y = Cumulative)) +
      geom_step()

  }

  if("list" %in% class(accrual_df)){

    x <- accrual_df
    class(x) <- class(x)[2]
    x <- x %>% map2(names(x), function(.x, .y){
      .x$site <- .y
      .x
    }) %>% bind_rows()

    out <- ggplot(x, aes(x = Date, y = Cumulative, col = site)) +
      geom_step()


  }


  return(out)

}



#' Title
#'
#' @param accrual_df
#' @param unit
#'
#' @return
#' @export
#' @importFrom ggplot2 geom_bar
#' @importFrom purrr map
#' @importFrom dplyr filter
#'
#' @examples
gg_accrual_plot_abs <- function(accrual_df
                                , unit = c("month","year","week","day")
                                ){

  if("data.frame" %in% class(accrual_df)){

    x <- accrual_time_unit(accrual_df, unit=unit)

    out <- ggplot(x, aes(x = date, y = Freq)) +
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
      filter(site != "Overall")

    out <- ggplot(x, aes(x = date, y = Freq, fill = site)) +
      geom_bar(stat = "identity")

  }

  return(out)
}


#' Title
#'
#' @param accrual_df
#' @param overall
#' @param name_overall
#' @param fill_up
#' @param wfun
#'
#' @return
#' @export
#' @importFrom ggplot2 geom_point geom_line annotation_custom ggtitle
#'
#' @examples
gg_accrual_plot_predict <- function(accrual_df
                                    , target
                                    , overall=TRUE
                                    , name_overall = attr(accrual_df, "name_overall")
                                    , col.pred="red"
                                    , lty.pred=2
                                    , pch.pred=8
                                    , fill_up=TRUE
                                    , wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
                                    , pos_prediction = c("out", "in", "none")
                                    , format_prediction="%B %d, %Y"
                                    ){

  pos_prediction <- match.arg(pos_prediction)

  if(is_accrual_list(accrual_df)){
    if(length(target) > 1){
      if(length(target) != length(accrual_df)) stop("target should have the same length as accrual_df")

    } else {
      accrual_df <- accrual_df[[name_overall]]
    }
  }

  accrual_df_o <- accrual_df
  lc_lct(accrual_df)

  pred_fn(accrual_df,
          fill_up,
          wfun,
          lc,
          overall,
          target,
          name_overall)

  if(is_accrual_list(accrual_df) & length(target) > 1){

    pdat <- pmap(list(accrual_df, end_date, target), function(x, y, z){
      data.frame(date = c(max(x$Date), y),
                         cum = c(max(x$Cumulative, na.rm = TRUE), z))
    })
    str(pdat)

    n <- 1
    l <- list()
    while(n <= length(target)){
      print(n)
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



  # if("list" %in% class(accrual_df)) x <- accrual_df[[1]]
  # if("data.frame" %in% class(accrual_df)) x <- accrual_df

  pred_text <- paste0("Predicted end date: ",
                      format(edate, format = format_prediction))

  out <- gg_accrual_plot_cum(accrual_df_o)

  out <- out + pgeom()

  print(str(pgeom))

  if(length(target) == 1){
    if(pos_prediction == "out") out <- out + ggtitle(pred_text)
    if(pos_prediction == "in"){
      library(grid)
      grob <- grid::grobTree(grid::textGrob(pred_text, x=0.025,  y=0.95, hjust=0))
      out <- out + annotation_custom(grob)
    }
  }
  return(out)
}




# set.seed(2020)
# enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
# target <- 75
# accrual_df<-accrual_create_df(enrollment_dates)
# gg_accrual_plot_cum(accrual_df) +
#   theme_classic()
# gg_accrual_plot_abs(accrual_df, unit = "week")
# gg_accrual_plot_abs(accrual_df, unit = "day")
# gg_accrual_plot_predict(accrual_df, target = target)
# accrual_plot_predict(accrual_df, target = target)
#
# #several sites
# set.seed(1)
# centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
# accrual_df<-accrual_create_df(enrollment_dates,by=centers, start_date = "common", name_overall = "Foo")
# gg_accrual_plot_cum(accrual_df)
# gg_accrual_plot_abs(accrual_df)
# gg_accrual_plot_abs(accrual_df, unit = "week")
# gg_accrual_plot_abs(accrual_df, unit = "day")
# gg_accrual_plot_predict(accrual_df, target = target)
# accrual_plot_predict(accrual_df,
#                      target = target,
#                      name_overall = "Overall")
#
# gg_accrual_plot_predict(accrual_df, target = target)
# gg_accrual_plot_predict(accrual_df[[1]], target = 75)
# gg_accrual_plot_predict(accrual_df[[4]], target = 75)
# gg_accrual_plot_predict(accrual_df, target = 75)
# gg_accrual_plot_predict(accrual_df, target = c(30,30,30,90))
# accrual_plot_predict(accrual_df[[1]],
#                      target = target,
#                      name_overall = "Overall")
#
# accrual_plot_predict(accrual_df,
#                      target = c(30, 30, 30, 90),
#                      name_overall = "Overall")
#
# accrual_plot_cum(accrual_df["Overall"])

# accrual_plot_predict(accrual_df, pos_prediction="out", target = 75)
# accrual_plot_predict(accrual_df, pos_prediction="in", target = 75)
# accrual_plot_predict(accrual_df, pos_prediction="none", target = 75)
