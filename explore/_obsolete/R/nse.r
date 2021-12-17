#!/usr/bin/env Rscript

library(pryr)
library(testthat)

address_bad_wrapper <- function(x) address(x)

address_good_wrapper <- function(x) {
    eval(substitute(address(y), list(y=substitute(x))), parent.frame())
}

address_escape_hatch <- function(x) {
    eval(substitute(address(y), list(y=x)), parent.frame())
}

l=list()

expect_that(address(l) == address(l), is_true())
expect_that(address_bad_wrapper(l) == address(l), is_false())
expect_that(address_bad_wrapper(l) == address_bad_wrapper(l), is_false())
expect_that(address_good_wrapper(l) == address(l), is_true())
expect_that(address_good_wrapper(l) == address_good_wrapper(l), is_true())
expect_that(address_escape_hatch(quote(l)) == address(l), is_true())
expect_that(address_escape_hatch(quote(l)) == address_escape_hatch(quote(l)), is_true())
