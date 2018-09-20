context("Premier groupe de test")

test_that("premier test",{
  expect_equal(1,1)
})

test_that("second test",{
  expect_match("Testing is fun", "x")
})
