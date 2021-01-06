
# generate test data
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)
site <- sample(1:3, 50, replace = TRUE)

# requires english locale!!
# Sys.setlocale("LC_ALL","English")


test_that("works with dates", {
  expect_warning(accrual_create_df(x), NA)
  expect_warning(accrual_create_df(x, by = site), NA)
})


df <- accrual_create_df(x)
df2 <- accrual_create_df(x, by = site)
dfs <- accrual_create_df(format(x, "%d%b%Y"))

test_that("works with string '%d%b%Y'", {
  df <- accrual_create_df(x)
  df2 <- accrual_create_df(x, by = site)

  dfs <- accrual_create_df(format(x, "%d%b%Y"))
  df2s <- accrual_create_df(format(x, "%d%b%Y"), by = site)
  expect_equivalent(df, dfs)
  expect_equivalent(df2, df2s)

})

test_that("works with string %Y-%m-%d", {
  df <- accrual_create_df(x)
  df2 <- accrual_create_df(x, by = site)

  dfs <- accrual_create_df(format(x, "%Y-%m-%d"),
                           format_enrollment_dates = "%Y-%m-%d")
  df2s <- accrual_create_df(format(x, "%Y-%m-%d"),
                            format_enrollment_dates = "%Y-%m-%d",
                            by = site)
  expect_equivalent(df2, df2s)
  expect_equivalent(df, dfs)

})


test_that("correct class", {
          expect_equal(class(df), c("accrual_df", "data.frame"))
          expect_equal(class(df2), c("accrual_df", "list"))
          expect_equal(class(df2[[1]]), c("accrual_df", "data.frame"))
})

test_that("start date parsing", {
  expect_warning(accrual_create_df(x, start_date = as.Date("2020-10-31")), NA)
  expect_warning(accrual_create_df(x, start_date = "31Oct2020"), NA)
  expect_warning(accrual_create_df(x, start_date = "2020-10-31",
                                   format_start_date = "%Y-%M-%d"), NA)
  expect_error(accrual_create_df(x, start_date = "2020-10-31"), "error parsing")

  df <- accrual_create_df(x, start_date = as.Date("2020-10-31"))
  expect_equal(min(df$Date), as.Date("2020-10-31"))
  expect_error(accrual_create_df(x, start_date = as.Date("2020-11-21")), "after earliest")


})

test_that("current date parsing", {
  expect_warning(accrual_create_df(x, current_date = as.Date("2020-12-31")), NA)
  expect_warning(accrual_create_df(x, current_date = "31Dec2020"), NA)
  expect_warning(accrual_create_df(x, current_date = "2020-12-31",
                                   format_current_date = "%Y-%m-%d"), NA)
  expect_error(accrual_create_df(x, current_date = "2020-12-31"), "error parsing")

  df <- accrual_create_df(x, current_date = as.Date("2020-12-31"))
  expect_equal(max(df$Date), as.Date("2020-12-31"))
  expect_error(accrual_create_df(x, current_date = as.Date("2020-12-24")), "before last")

})



test_that("overall column", {
  df2 <- accrual_create_df(x, by = site)
  expect_equal(names(df2)[length(unique(site)) + 1], "Overall")
  df2 <- accrual_create_df(x, by = site, name_overall = "All")
  expect_equal(names(df2)[length(unique(site)) + 1], "All")
  df2 <- accrual_create_df(x, by = site, overall = FALSE)
  expect_equal(length(df2), length(unique(site)))


})



