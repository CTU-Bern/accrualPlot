




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


















