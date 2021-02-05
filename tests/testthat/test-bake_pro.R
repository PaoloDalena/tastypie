test_that("error when wrong template", {
  a <- data.frame(letters[1:3], 1:3)
  expect_error(pie_bake_pro(a, template = "abbassomilan"), "exist")
  expect_error(pie_bake_pro(a, template = "basic3"), "pie_bake")
})

test_that("data_check is properly linked", {
  a <- data.frame(1:3, letters[1:3])
  expect_error(pie_bake_pro(a, template = "eye1"), "second variable")
  expect_error(pie_bake_pro(a, template = "eye2"), "second variable")
})

test_that("output has 'list' type", {
  a <- data.frame(letters[1:3], 1:3)
  expect_type(pie_bake_pro(a, template = "dart1"), "list")
  expect_type(pie_bake_pro(a, template = "dart3", title = "tt"), "list")
})

test_that("output is a 'ggplot' object", {
  a <- data.frame(letters[1:3], 1:3)
  expect_match(class(pie_bake_pro(a, template = "eaten3")), "gg")
  expect_match(class(pie_bake_pro(a, template = "eaten1", title = "gg")), "gg")
})

test_that("title specification works", {
  a <- data.frame(letters[1:3], 1:3)
  out1 <- pie_bake_pro(a, template = "watermelon1")
  out2 <- pie_bake_pro(a, template = "watermelon3", title = "ciaociao")
  expect_match(out1$labels$title, "")
  expect_match(out2$labels$title, "ciaociao")
})

test_that("percentages are properly computed", {
  a <- data.frame(letters[1:3], 1:3)
  b <- data.frame(letters[1:5], sample(1:100,5))
  out1 <- pie_bake_pro(a, template = "watermelon4")
  out2 <- pie_bake_pro(b, template = "eye1", title = "ciaociao")
  expect_equal(sum(out1$data[,2]), 100, tolerance = 3)
  expect_equal(sum(out2$data[,2]), 100, tolerance = 3)
})