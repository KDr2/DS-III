#!/usr/bin/env Rscript

library(pryr)
library(testthat)

## in pkg `stats` sd calls var, we'll change the called var to another function
data = rep(3, 27)
expect_that(var(data) == 0, is_true())
expect_that(sd(data) == 0, is_true())

var <- function(...) pi
expect_that(var(data) == pi, is_true()) # changed
expect_that(sd(data) == 0, is_true())   # not changed
rm(var)

## where("var") returns the package env, not the namespace env
## so we should use loadNamespace to get the namespace env
stats_ns_env <- loadNamespace("stats")
var_origin <- stats_ns_env$var
unlockBinding("var", stats_ns_env)
stats_ns_env$var <- function(...) pi
expect_that(var(data) == pi, is_false()) # call in global env, not changed
expect_that(sd(data) == sqrt(pi), is_true())   # changed

stats_pkg_env <- where("var")
unlockBinding("var", stats_pkg_env)
stats_pkg_env$var <- stats_ns_env$var
expect_that(var(data) == pi, is_true()) # call in global env, changed

## change var in the package env doesn't work for sd, due to the order of these env in the env chain:
## function-in-pkg: -> namespace:pkg env -> imports:pkg env -> namespace:base env ->
##                     globalenv() -> package:pkg env -> ...search path ... -> baseenv() -> emptyenv()
