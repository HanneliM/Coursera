---
title: "Building an R Package"
author: "Hanneli M"
date: "2016-11-08"
output: rmarkdown::html_vignette
vignette: >
%\VignetteIndexEntry{Model Details for example_package}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}
---

## Introduction
The `US_crash` package provides tools to explore and visualise data from the
National Highway Traffic Safety Administration (NHTSA) Fatality Analysis
Reporting System (FARS). This data is from thec Nationwide census conducted by
US National Highway Traffic Satefy Administrations Fatality Analysis Reporting
System. This dataset Provides the American public with yearly data reagrding
fatal injuries due to motor vechie traffic crashes.

```{r, eval = FALSE}
library(US_crash)
filename <- make_filename(2013)
data <- fars_read(filename)


###  Test
Testing that `make_filename` is the "path of least resistance" because it doesnt require you to
bundle large `.csv.bz2` files in your test folder.

```R
test_that("Filename generation is consistent", {
  # Test standard numeric input
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")

  # Test character input
  expect_equal(make_filename("2014"), "accident_2014.csv.bz2")

  # Test that it returns a character string
  expect_type(make_filename(2015), "character")
})

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("STATE", "MONTH", "year", "n", "LONGITUD", "LATITUDE"))
}
