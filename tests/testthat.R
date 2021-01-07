library(testthat)
library(vdiffr)
library(accrualPlot)

# requires english locale!!
Sys.setlocale("LC_ALL","English")

test_check("accrualPlot")
