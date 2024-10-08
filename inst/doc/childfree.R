## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_knit$set(global.par = TRUE)

## ----setup--------------------------------------------------------------------
library(childfree)

## ----results='hide'-----------------------------------------------------------
dat <- dhs(file = "ZZIR62FL.SAV", extra.vars = c("v201", "v602", "v613"))

## -----------------------------------------------------------------------------
if (!is.null(dat)) {t(dat[2368,c(3:9,19:21)])}

## -----------------------------------------------------------------------------
dat <- nsfg(years = 2017)

## -----------------------------------------------------------------------------
if (!is.null(dat)) {t(dat[14,2:12])}

## -----------------------------------------------------------------------------
dat <- soss(waves = 84, extra.vars = (c("neal1", "neal2", "neal3")))

## -----------------------------------------------------------------------------
if (!is.null(dat)) {t(dat[2,c(2:9,12:13,21:24)])}

