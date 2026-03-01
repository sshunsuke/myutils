test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("col32", {
  expect_equal(p_col32(c("#123456", "red"), c(0,128)), c("#12345600", "#FF000080"))
})
