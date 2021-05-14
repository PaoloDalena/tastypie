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
  expect_match(class(pie_bake_pro(a, template = "cirbar1", title = "prova")), "gg")
  expect_match(class(pie_bake_pro(a, template = "cirbar2")), "gg")
  expect_match(class(pie_bake_pro(a, template = "cirbar3", title = "ciao")), "gg")
  expect_match(class(pie_bake_pro(a, template = "cirbar4", title = "nop")), "gg")
  expect_match(class(pie_bake_pro(a, template = "cirbar5")), "gg")
})

test_that("output is NULL when the template is a spider chart", {
  a <- data.frame(letters[1:3], 1:3)
  expect_match(class(pie_bake_pro(a, template = "spider1")), "NULL")
  expect_match(class(pie_bake_pro(a, template = "spider2")), "NULL")
  expect_match(class(pie_bake_pro(a, template = "spider3")), "NULL")
  expect_match(class(pie_bake_pro(a, template = "spider4")), "NULL")
  expect_match(class(pie_bake_pro(a, template = "spider5")), "NULL")
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

test_that("also tibbles can be baked", {
  a <- data.frame(letters[1:3], 1:3)
  b <- tibble::tibble(letters[1:5], 1:5)
  c <- tibble::tibble(letters[1:5], c(4.3, 2.2, 1, 2, 3.0))
  expect_match(class(pie_bake_pro(tibble::as_tibble(a), template = "dart2")), "gg")
  expect_match(class(pie_bake_pro(b, template = "dart4")), "gg")
  expect_match(class(pie_bake_pro(c, template = "dart5")), "gg")
})


