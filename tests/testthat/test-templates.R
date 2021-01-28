test_that("choice of number of groups is respected", {
  a <- pie_templates("blue2", 9)
  b <- pie_templates("watermelon5", n_groups = 8)
  c <- pie_templates("blue4", n_groups = 7, perc = T)
  d <- pie_templates("eaten2", n_groups = 6)
  e <- pie_templates("eaten4", n_groups = 5)
  f <- pie_templates("eaten5", n_groups = 4)
  expect_equal(length(a$data[,1]), 9)
  expect_equal(length(b$data[,2]), 8)
  expect_equal(length(c$data[,1]), 7)
  expect_equal(length(d$data[,1]), 6)
  expect_equal(length(e$data[,1]), 5)
  expect_equal(length(f$data[,1]), 4)
})

test_that("error when wrong template", {
  expect_error(pie_templates("eye6"), "exist")
  expect_error(pie_templates("basic"), "exist")
})

test_that("title reminds function", {
  out1 <- pie_templates(template = "bw4")
  out2 <- pie_templates(template = "watermelon3")
  out3 <- pie_templates(template = "basic1", perc = TRUE)
  expect_match(out1$labels$title, "pie_bake")
  expect_match(out2$labels$title, "pie_bake_pro")
  expect_match(out3$labels$title, "perc = TRUE")
})

test_that("ex_groups gives an error when the number of groups is wrong", {
  expect_error(pie_templates("eye1", n_groups = 1), "number")
  expect_error(pie_templates("eye5", n_groups = 10), "number")
})

test_that("pie discover works properly", {
  a <- pie_discover()
  b <- pie_discover("bake")
  c <- pie_discover("pro")
  expect_match(class(a), "gg")
  expect_match(class(b), "gg")
  expect_match(class(c), "gg")
})
