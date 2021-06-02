

enrollment_dates<-as.Date("2022-01-01") + 0:10

test_that("accrual_predict result", {

	x<-accrual_create_df(enrollment_dates)
	y<-accrual_linear_model(x)
	z<-accrual_predict(x,y,target=365)
	expect_equal(z, as.Date("2022-12-31"), tolerance = .1)

	x<-accrual_create_df(enrollment_dates,force_start0=FALSE)
	y<-accrual_linear_model(x)
	z<-accrual_predict(x,y,target=365)
	expect_equal(z, as.Date("2022-12-31"), tolerance = .1)

	x<-accrual_create_df(enrollment_dates,start_date=as.Date("2021-12-30"))
	y<-accrual_linear_model(x,fill_up=FALSE)
	z<-accrual_predict(x,y,target=365)
	expect_true(z>as.Date("2022-12-31"), tolerance = .1)

	x<-accrual_create_df(enrollment_dates,current_date=as.Date("2022-02-01"))
	y<-accrual_linear_model(x,fill_up=FALSE)
	z<-accrual_predict(x,y,target=365)
	expect_true(z>as.Date("2022-12-31"), tolerance = .1)

})


