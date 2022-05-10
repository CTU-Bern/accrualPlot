---
title: '`accrualPlot`: Easy recruitment tracking for clinical trials'
tags:
  - R software
  - clinical research
  - risk based monitoring
authors:
  - name: Alan G Haynes
    orcid: 0000-0003-1374-081X
    affiliation: "1"
  - name: Lukas Bütikofer
    orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
  - name: CTU Bern, University of Bern, Bern, Switzerland
    index: 1
date: 10 May 2022
year: 2022
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---


<!-- A list of the authors of the software and their affiliations, using the correct format (see the example below). -->
<!-- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience. -->
<!-- A Statement of Need section that clearly illustrates the research purpose of the software. -->
<!-- A list of key references, including to other software addressing related needs. Note that the references should include full names of venues, e.g., journals and conferences, not abbreviations only understood in the context of a specific discipline. -->
<!-- Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it. -->
<!-- Acknowledgement of any financial support. -->


# Summary

Clinical researchers are often overly optimistic with regards to their institutions capacity to enroll participants into their trials. A common failing point in clinical trials is thus poor recruitment, which leads to extended trial duration, with concomitant costs, and potentially to shutting a trial down before enough participants have been recruited to show an effect (@fogel2018). The International Conference on Harmonization (ICH) Good Clinical Practice (GCP) guideline addendum E6 (R2) highlights the need for risk based monitoring. `accrualPlot` provides tools to summarize recruitment and predict the end date for the target sample size, thereby assisting with monitoring the risk of poor recruitment. `accrualPlot` is available from [CRAN](https://CRAN.R-project.org/package=accrualPlot) and additional information is available at the [package site](https://ctu-bern.github.io/accrualPlot/).


<!-- # Citations -->

<!-- Citations to entries in paper.bib should be in -->
<!-- [rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html) -->
<!-- format. -->

<!-- For a quick reference, the following citation commands can be used: -->
<!-- - `@author:2001`  ->  "Author et al. (2001)" -->
<!-- - `[@author:2001]` -> "(Author et al., 2001)" -->
<!-- - `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)" -->

# Example usage
`accrualPlot` is installed via `install.packages("accrualPlot")`, and loaded into the R session via `library(accrualPlot)`. 

First, we generate some data to demonstrate the package's functionality. We will generate 50 participants from 3 sites, where one site performs worse than the others. In normal use, the data is derived from the trial database.




```r
dat <- data.frame(date = as.Date("2020-07-01") + sample(1:100, 50),
                  site = sample(c("Site 1", "Site 2", "Site 3"), 50, 
                                replace = TRUE, prob = c(.4, .35, .15)))
```

The first step to using `accrualPlot` is to create an `accrual_df` as follows. At this point, if we know when each site opened, we can pass a vector of dates to the `start_date` argument. We can also indicate the current date (via the `current_date` argument), which can be relevant when there have been no new participants enrolled recently. The `by`, `start_date` and `current_date` arguments are all optional.


```r
acc <- accrual_create_df(enrollment_dates = dat$date, 
                         by = dat$site, 
                         start_date = c("Site 1" = as.Date("2020-07-01"), 
                                        "Site 2" = as.Date("2020-07-13"), 
                                        "Site 3" = as.Date("2020-06-20")))
```

A short summary table of recruitment at the sites (if entered, only overall otherwise) is accessed via `summary`.


```r
summary(acc)
```

```
##      name           start_date            time                    n
## 1  Center First participant in Months accruing Participants accrued
## 2  Site 1            01Jul2020               3                   24
## 3  Site 2            13Jul2020               3                   17
## 4  Site 3            20Jun2020               4                    9
## 5 Overall            20Jun2020               4                   50
##                       rate
## 1 Accrual rate (per month)
## 2                     7.42
## 3                     6.00
## 4                     2.50
## 5                    13.89
```

We can also plot the cumulative (by default for individual sites and overall) and absolute recruitment (per day, week, month, etc).


```r
par(mfrow = c(1,2))
plot(acc)
plot(acc, which = "abs")
```

![](paper_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

It is also possible predict the date at which the recruitment target would be achieved by setting `which = "pred"` and the `target` argument.


```r
plot(acc, which = "pred", target = 75)
```

![](paper_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 


# Acknowledgements

We acknowledge contributions from Nick Fankhauser, who wrote an early version of the code, and other statisticians from CTU Bern for testing and suggestions while developing this software.

# References
