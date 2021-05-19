
# generate test data
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)
site <- sample(1:3, 50, replace = TRUE)


test_that("works with dates", {
  expect_warning(accrual_create_df(x), NA)
  expect_warning(accrual_create_df(x, by = site), NA)
})


df <- accrual_create_df(x)
df2 <- accrual_create_df(x, by = site)

test_that("does not work with string %d%b%Y", {
  expect_error(accrual_create_df(format(x, "%d%b%Y")))
})

test_that("does not work string %Y-%m-%d", {
  expect_error(accrual_create_df(format(x, "%Y-%m-%d")))
})


test_that("correct class", {
          expect_equal(class(df), c("accrual_df", "data.frame"))
          expect_equal(class(df2), c("accrual_df", "list"))
          expect_equal(class(df2[[1]]), c("accrual_df", "data.frame"))
})

test_that("start date parsing", {
  expect_warning(accrual_create_df(x, start_date = as.Date("2020-10-31")), NA)
  expect_error(accrual_create_df(x, start_date = "31Oct2020"))
  expect_error(accrual_create_df(x, start_date = "2020-10-31"))

  df <- accrual_create_df(x, start_date = as.Date("2020-10-31"))
  expect_equal(min(df$Date), as.Date("2020-10-31"))
  expect_error(accrual_create_df(x, start_date = as.Date("2020-11-21")), "after earliest")


})

test_that("current date parsing", {
  expect_error(accrual_create_df(x, current_date = as.Date("2020-12-31")), NA)
  expect_error(accrual_create_df(x, current_date = "31Dec2020"))

  df <- accrual_create_df(x, current_date = as.Date("2020-12-31"))
  expect_equal(max(df$Date), as.Date("2020-12-31"))
  expect_error(accrual_create_df(x, current_date = as.Date("2020-12-24")), "before last")
})

set.seed(2020)
enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
set.seed(2020)
centers<-sample(c("Site 1","Site 2","Site 3"),length(enrollment_dates), replace=TRUE)


test_that("accrual_create_df class, errors, length", {
  expect_error(accrual_create_df(enrollment_dates), NA)
  expect_error(accrual_create_df(enrollment_dates, by=centers), NA)
  expect_error(accrual_create_df(format(enrollment_dates, "%Y-%m-%d")))
  expect_error(accrual_create_df(format(enrollment_dates, "%Y-%m-%d"), by=centers))

  x <- accrual_create_df(enrollment_dates)
  expect_equal(class(x), c("accrual_df", "data.frame"))
  expect_equal(length(x), 3)
  expect_equal(nrow(x), length(unique(enrollment_dates)))

  x <- accrual_create_df(enrollment_dates,by=centers)
  expect_equal(class(x), c("accrual_df", "list"))
  expect_equal(length(x), length(unique(centers))+1)

})



test_that("start_date", {
  expect_error(accrual_create_df(enrollment_dates,
                                 start_date = as.Date("2017-12-15")), NA)
  expect_error(accrual_create_df(enrollment_dates, by=centers,
                                 start_date = as.Date("2017-12-15")), NA)
  expect_error(accrual_create_df(enrollment_dates,
                                 start_date = "2017-12-15"))
  expect_error(accrual_create_df(enrollment_dates, by=centers,
                                 start_date = "2017-12-15"))

  x <- accrual_create_df(enrollment_dates,
                         start_date = as.Date("2017-12-15"))
  expect_equal(min(x$Date), as.Date("2017-12-15"))

  x <- accrual_create_df(enrollment_dates,by=centers,
                         start_date = as.Date("2017-12-15"))
  expect_true(all(unlist(lapply(x, function(x) min(x$Date) == as.Date("2017-12-15")))))

})



test_that("current_date", {
  expect_error(accrual_create_df(enrollment_dates,
                                 current_date = as.Date("2018-02-15")), NA)
  expect_error(accrual_create_df(enrollment_dates, by=centers,
                                 current_date = as.Date("2018-02-15")), NA)
  expect_error(accrual_create_df(enrollment_dates,
                                 current_date = "2018-02-15"))
  expect_error(accrual_create_df(enrollment_dates, by=centers,
                                 current_date = "2018-02-15"))

  x <- accrual_create_df(enrollment_dates,
                         current_date = as.Date("2018-02-15"))
  expect_equal(max(x$Date), as.Date("2018-02-15"))

  x <- accrual_create_df(enrollment_dates,by=centers,
                         current_date = as.Date("2018-02-15"))
  expect_true(all(unlist(lapply(x, function(x) max(x$Date) == as.Date("2018-02-15")))))

})



test_that("error on NA dates", {
  x <- enrollment_dates
  x[10] <- NA
  expect_error(accrual_create_df(x), "contains NA")
})



test_that("overall column", {
  df2 <- accrual_create_df(x, by = site)
  expect_equal(names(df2)[length(unique(site)) + 1], "Overall")
  df2 <- accrual_create_df(x, by = site, name_overall = "All")
  expect_equal(names(df2)[length(unique(site)) + 1], "All")
  df2 <- accrual_create_df(x, by = site, overall = FALSE)
  expect_equal(length(df2), length(unique(site)))


})





