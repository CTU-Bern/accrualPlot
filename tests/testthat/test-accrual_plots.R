
# generate test data
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)
site <- sample(1:3, 50, replace = TRUE)

# requires english locale!!
# Sys.setlocale("LC_ALL","English")

df <- accrual_create_df(x)
df2 <- accrual_create_df(x, by = site)



test_that("cumulative plots", {
  expect_error(accrual_plot_cum(df), NA)
  expect_error(gg_accrual_plot_cum(df), NA)
  expect_warning(accrual_plot_cum(df2), NA)
  expect_warning(gg_accrual_plot_cum(df2), NA)
})



# accrual_plot_abs(df, unit = "week")

test_that("accrual_plot_abs", {
  expect_error(accrual_plot_abs(df, unit = "weeks"))
  expect_error(accrual_plot_abs(df, unit = "week"), NA)
  expect_error(gg_accrual_plot_abs(df, unit = "weeks"))
  expect_error(gg_accrual_plot_abs(df, unit = "week"), NA)
})




# vdiffr tests

# to check validate figures
# Sys.setlocale("LC_ALL","English")
# vdiffr::manage_cases()

skip_if(getRversion() < package_version("4.1.0"))

test_that("vdiff cumulative plots", {

  fn <- function() accrual_plot_cum(df)
  expect_doppelganger("cumulative", fn)

  fn <- function() accrual_plot_cum(df)
  expect_doppelganger("cumulative, early start", fn)

  fn <- function() accrual_plot_cum(df)
  expect_doppelganger("cumulative, end date", fn)

  fn <- function() accrual_plot_cum(df, xlabn = 8)
  expect_doppelganger("cumulative, xlabn", fn)
  # doesnt seem to work

  fn <- function() accrual_plot_cum(df, xlabsrt = 90)
  expect_doppelganger("cumulative, xlabsrt", fn)

  fn <- function() accrual_plot_cum(df2)
  expect_doppelganger("cumulative site", fn)

  fn <- function() accrual_plot_cum(df2)
  expect_doppelganger("cumulative site, early start", fn)

  fn <- function() accrual_plot_cum(df2)
  expect_doppelganger("cumulative site, end date", fn)

  fn <- function() accrual_plot_cum(df2, xlabn = 8)
  expect_doppelganger("cumulative site, xlabn", fn)
  # doesnt seem to work

  fn <- function() accrual_plot_cum(df2, xlabsrt = 90)
  expect_doppelganger("cumulative site, xlabsrt", fn)

  fn <- function() accrual_plot_cum(df2, col = c("red1", "blue", "orange", "black"))
  expect_doppelganger("cumulative site, col", fn)

  # ggplot

  fn <- gg_accrual_plot_cum(df)
  expect_doppelganger("gg cumulative", fn)

  fn <- gg_accrual_plot_abs(df)
  expect_doppelganger("gg abs", fn)

  fn <- gg_accrual_plot_predict(df, target = 70)
  expect_doppelganger("gg pred", fn)

  fn <- gg_accrual_plot_cum(df2)
  expect_doppelganger("gg cumulative site", fn)

  fn <- gg_accrual_plot_abs(df2)
  expect_doppelganger("gg abs site", fn)

  fn <- gg_accrual_plot_predict(df2, target = 70)
  expect_doppelganger("gg pred site", fn)

  fn <- gg_accrual_plot_cum(df2[[1]])
  expect_doppelganger("gg cumulative site1", fn)

  fn <- gg_accrual_plot_abs(df2[[1]])
  expect_doppelganger("gg abs site1", fn)

  # fn <- gg_accrual_plot_predict(df2[[1]], target = 70)
  # expect_doppelganger("gg pred site 1", fn)

  fn <- gg_accrual_plot_predict(df2[[2]], target = 70)
  expect_doppelganger("gg pred site 2", fn)

  fn <- gg_accrual_plot_predict(df2, target = c(30, 30, 30, 70))
  expect_doppelganger("gg pred site n", fn)



})





# accrual_plot_abs(df, unit = "week")

test_that("vdiff accrual_plot_abs", {
  fn <- function() accrual_plot_abs(df)
  expect_doppelganger("abs default", fn)
  fn <- function() accrual_plot_abs(df, unit = "week")
  expect_doppelganger("abs week", fn)
  fn <- function() accrual_plot_abs(df, unit = "day")
  expect_doppelganger("abs day", fn)
})






