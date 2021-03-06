% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary.accrual_df}
\alias{summary.accrual_df}
\title{Summary method for accrual_dfs (as created by accrual_create_df)}
\usage{
\method{summary}{accrual_df}(object, ...)
}
\arguments{
\item{object}{accrual_df object}

\item{...}{options passed to other functions}
}
\value{
a table
}
\description{
Summary method for accrual_dfs (as created by accrual_create_df)
}
\examples{
set.seed(2020)
enrollment_dates <- as.Date("2018-01-01") + sort(sample(1:30, 50, replace=TRUE))
accrual_df<-accrual_create_df(enrollment_dates)
summary(accrual_df)
}
