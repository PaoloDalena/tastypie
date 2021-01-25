test_that("error when wrong template", {
  a <- data.frame(letters[1:3], 1:3)
  expect_error(pie_bake(a, template = "forzainter"), "exist")
  expect_error(pie_bake(a, template = "dart4"), "pie_bake_pro")
})

test_that("data_check is properly linked", {
  a <- data.frame(1:3, letters[1:3])
  expect_error(pie_bake(a, template = "basic1", perc = TRUE), "second variable")
  expect_error(pie_bake(a, template = "basic2"), "second variable")
})

test_that("output has 'list' type", {
  a <- data.frame(letters[1:3], 1:3)
  expect_type(pie_bake(a, template = "basic3"), "list")
  expect_type(pie_bake(a, template = "basic4", title = "tt"), "list")
})

test_that("output is a 'ggplot' object", {
  a <- data.frame(letters[1:3], 1:3)
  expect_match(class(pie_bake(a, template = "basic5", perc = T)), "gg")
  expect_match(class(pie_bake(a, template = "bw1", title = "gg")), "gg")
})

test_that("title specification works", {
  a <- data.frame(letters[1:3], 1:3)
  out1 <- pie_bake(a, template = "bw2", perc = TRUE)
  out2 <- pie_bake(a, template = "bw3", title = "ciaociao")
  expect_match(out1$labels$title, "")
  expect_match(out2$labels$title, "ciaociao")
})

test_that("for donut is different", {
  a <- data.frame(letters[1:3], 1:3)
  out1 <- pie_bake(a, template = "bw4")
  out2 <- pie_bake(a, template = "red5")
  out3 <- pie_bake(a, template = "donut1")
  expect_equal(length(out1$mapping), 3)
  expect_equal(length(out2$mapping), 3)
  expect_equal(length(out1$mapping), length(out2$mapping))
  expect_equal(length(out3$mapping), 5)
})

test_that("percentages are properly computed", {
  a <- data.frame(letters[1:3], 1:3)
  b <- data.frame(letters[1:5], sample(1:100,5))
  out1 <- pie_bake(a, template = "red1", perc = TRUE)
  out2 <- pie_bake(b, template = "blue4", title = "ciaociao")
  expect_equal(sum(out1$data[,2]), 100, tolerance = 3)
  expect_equal(sum(out2$data[,2]), 100, tolerance = 3)
})
