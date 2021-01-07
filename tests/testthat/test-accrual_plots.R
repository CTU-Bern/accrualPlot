
# generate test data
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)
site <- sample(1:3, 50, replace = TRUE)

# requires english locale!!
# Sys.setlocale("LC_ALL","English")

df <- accrual_create_df(x)
df2 <- accrual_create_df(x, by = site)

# vdiffr::manage_cases()

test_that("cumulative plots", {

  fn <- function() accrual_plot_cum(df)
  expect_doppelganger("cumulative", fn)

  fn <- function() accrual_plot_cum(df, start_date = as.Date("2020-10-31"))
  expect_doppelganger("cumulative, early start", fn)

  expect_error(accrual_plot_cum(df, start_date = as.Date("2020-11-20")))

  fn <- function() accrual_plot_cum(df, current_date = as.Date("2020-12-31"))
  expect_doppelganger("cumulative, end date", fn)

  expect_error(accrual_plot_cum(df, current_date = as.Date("2020-12-20")))

  fn <- function() accrual_plot_cum(df, xlabn = 8)
  expect_doppelganger("cumulative, xlabn", fn)
  # doesnt seem to work

  fn <- function() accrual_plot_cum(df, xlabsrt = 90)
  expect_doppelganger("cumulative, xlabsrt", fn)


  fn <- function() accrual_plot_cum(df2)
  expect_doppelganger("cumulative site", fn)

  fn <- function() accrual_plot_cum(df2, start_date = as.Date("2020-10-31"))
  expect_doppelganger("cumulative site, early start", fn)

  expect_error(accrual_plot_cum(df, start_date = as.Date("2020-11-20")))

  fn <- function() accrual_plot_cum(df2, current_date = as.Date("2020-12-31"))
  expect_doppelganger("cumulative site, end date", fn)

  expect_error(accrual_plot_cum(df, current_date = as.Date("2020-12-20")))

  fn <- function() accrual_plot_cum(df2, xlabn = 8)
  expect_doppelganger("cumulative site, xlabn", fn)
  # doesnt seem to work

  fn <- function() accrual_plot_cum(df2, xlabsrt = 90)
  expect_doppelganger("cumulative site, xlabsrt", fn)

})




