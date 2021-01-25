test_that("choice of number of groups is respected", {
  a <- pie_templates("blue2", 9)
  b <- pie_templates("watermelon5", n_groups = 8)
  c <- pie_templates("blue4", n_groups = 6, perc = T)
  expect_equal(length(a$data[,1]), 9)
  expect_equal(length(b$data[,2]), 8)
  expect_equal(length(c$data[,1]), 6)
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
