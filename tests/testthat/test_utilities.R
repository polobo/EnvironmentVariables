library(testthat)
library(envvar)
context("Utility Tests")

# We need to have a known environment variable with which to test affirmative lookups
Sys.setenv("KNOWN_VAR" = "VALUE")
Sys.setenv("EMPTY_VAR" = "")

test_that("env_or_default can resolve single envvars", {
  expect_equal("VALUE", env_or_default("KNOWN_VAR", "DEFAULT"))
  expect_equal("DEFAULT", env_or_default("UNKNOWN", "DEFAULT"))
})

test_that("nv_or_default can resolve multiple envvars", {
  expect_equal(c("KNOWN_VAR" = "VALUE", "UNKNOWN" = "DEFAULT"),
               env_or_default(c("KNOWN_VAR", "UNKNOWN"), c("DEFAULT", "DEFAULT")))
})

test_that("env_or_default requires matching lengths, no repetition allowed", {
  expect_error(env_or_default(c("KNOWN_VAR", "UNKNOWN"), c("DEFAULT")))
})

test_that("env_or_default distinguishs between empty and missing", {
  expect_equal("", env_or_default("EMPTY_VAR", "DEFAULT"))
})

test_that("env_or_default requires at least one pair", {
 expect_error(env_or_default())
 expect_error(env_or_default(c(), c()))
})

test_that("env_or_default result is not a named vector for single-pair", {
  result <- env_or_default("KNOWN_VAR", "DEFAULT")
  expect_named(result, NULL)
})

test_that("env_or_default result is a named vector for multi-pair", {
  result <- env_or_default(c("KNOWN_VAR","UNKNOWN"), c("DEFAULT","DEFAULT"))
  expect_named(result, c("KNOWN_VAR", "UNKNOWN"))
  expect_equal("DEFAULT", result[["UNKNOWN"]])
})
