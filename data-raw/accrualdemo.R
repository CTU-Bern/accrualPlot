# generate demo data

set.seed(123)
N <- 250
dat <- data.frame(date = as.Date("2020-07-01") + sample(1:100, N,
                                                        prob = seq(from = 0.001,
                                                                   to = 0.95,
                                                                   length.out = 100),
                                                        replace = TRUE),
                  site = sample(c("Site 1", "Site 2", "Site 3"), N,
                                replace = TRUE, prob = c(.4, .35, .15)))
dat <- dat[order(dat$date), ]
dat$site[dat$site == "Site 3" & 1:nrow(dat) <= 100] <- "Site 1"
dat$site[dat$site == "Site 2" & 1:nrow(dat) <= 5] <- "Site 1"

accrualdemo <- dat

usethis::use_data(accrualdemo, overwrite = TRUE)

# library(accrualPlot)
# acc <- accrual_create_df(dat$date, dat$site)
# plot(acc)
# plot(acc, "pred", target = 500, wfun = function(x) ifelse(x$Date > (max(x$Date)-30), 1, 0))
# plot(acc, "pred", target = 500)
