
library(ggplot2)
library(magrittr)
library(dplyr)
library(purrr)

gg_accrual_plot_cum <- function(accrual_df){

  if(class(accrual_df)[2] == "data.frame"){

    out <- ggplot(accrual_df, aes(x = Date, y = Cumulative)) +
      geom_step()

  }

  if(class(accrual_df)[2] == "list"){

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



gg_accrual_plot_abs <- function(accrual_df
                                , unit = c("month","year","week","day")
                                ){

  if(class(accrual_df)[2] == "data.frame"){

    x <- accrual_time_unit(accrual_df, unit=unit)

    out <- ggplot(x, aes(x = date, y = Freq)) +
      geom_bar(stat = "identity")

  }

  if(class(accrual_df)[2] == "list"){

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


# target <- 75
gg_accrual_plot_predict <- function(accrual_df
                                    target,
                                    overall=TRUE,
                                    name_overall="Overall",
                                    fill_up=TRUE,
                                    wfun=function(x) seq(1 / nrow(x), 1, by = 1/nrow(x))
                                    ){

  # START COPIED FROM accrual_plot_predict
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

  #predictions
  #&&&&&&&&&&

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
  # END COPIED FROM accrual_plot_predict


  out <- gg_accrual_plot_cum(accrual_df[[1]]) +
    geom_point(aes(x = edate, y = target)) +
    geom_line(aes(x = c(max())))

  return(out)
}




# set.seed(2020)
# enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
# accrual_df<-accrual_create_df(enrollment_dates)
# gg_accrual_plot_cum(accrual_df) +
#   theme_classic()
# gg_accrual_plot_abs(accrual_df, unit = "week")
# gg_accrual_plot_abs(accrual_df, unit = "day")
#
# #several sites
# set.seed(1)
# centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates),replace=TRUE)
# accrual_df<-accrual_create_df(enrollment_dates,by=centers, start_date = "common")
# gg_accrual_plot_abs(accrual_df)
# gg_accrual_plot_abs(accrual_df, unit = "week")
# gg_accrual_plot_abs(accrual_df, unit = "day")




