test_that("spinar output size", {
  expect_equal(length(package1::spinar(c(2,3,1,1),2)),6)
})

test_that("Non-interger or missing inputs should error",{
  expect_error(package1::spinar(c(1,2,2,2,1), "1"), "Lag p for INAR(p) must be integer")
})
