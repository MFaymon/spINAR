test_that("spinar output size", {
  expect_equal(length(package1::spinar(c(2,3,1,1),2)),6)
  expect_equal(length(package1::spinar(c(2,3,1,1,0),2)), 6)
  expect_equal(length(package1::spinar(c(2,3,1,1),1)),5)
  expect_equal(length(package1::spinar(c(2,3,1,1,0),1)),5)
  expect_equal(length(package1::spinar(c(1,1,1,0), 1)), 3)
})

test_that("Non-interger p or non-vector dat or missing inputs should error",{
  expect_error(package1::spinar(c(1,2), 2), "Data should be a vector with at least two entries")
  expect_error(package1::spinar(c(1,1), 1), "Data should be a vector with at least two different entries")
  expect_warning(package1::spinar(c(1), 1), "Data should be a vector with at least two entries")
  expect_warning(length(package1::spinar(c(2,3,1,1,0), "1")), "Lag p for INAR(p) must be integer")
  expect_warning(length(package1::spinar(c(2,3,1,1,0))), 'argument "p" is missing, with no default')
})

test_that("Non big enough inputs should be an error", {
  expect_equal(length(package1::spinar(c(2,3),2)),6)
  expect_equal(length(package1::spinar(c(2,3,1,1,0),2)), 6)
})

#devtools::check()
#usethis::use_git_remote(name = "origin", url = "https://github.com/jariffo/spINAR.git", overwrite = TRUE)
#usethis::use_git_remote(name = "upstream")


