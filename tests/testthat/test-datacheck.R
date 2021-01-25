test_that("error when not dataframe", {
  a <- list(letters[1:3], 1:3)
  expect_error(pie_datacheck(a), "dataframe")
})

test_that("error when not bidimensional dataframe", {
  a <- data.frame(letters[1:3], 1:3, 1:3)
  b <- data.frame(letters[1:3])
  expect_error(pie_datacheck(a), "only two")
  expect_error(pie_datacheck(b), "only two")
})

test_that("error when labels in second column", {
  a <- data.frame(1:3, letters[1:3])
  expect_error(pie_datacheck(a), "numerical values")
})

test_that("error when negative or zero values", {
  a <- data.frame(letters[1:3], c(1, 2, -3))
  b <- data.frame(letters[1:3], 0:2)
  expect_error(pie_datacheck(a), "all positive")
  expect_error(pie_datacheck(b), "different from zero")
})

test_that("message when check = T and everything ok", {
  a <- data.frame(letters[1:3], 1:3)
  expect_message(pie_datacheck(a, check = TRUE), ":)")
})

test_that("message when too many groups", {
  a <- data.frame(letters[1:10], 1:10)
  expect_message(pie_datacheck(a), "too high")
})
