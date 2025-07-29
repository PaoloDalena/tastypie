
test_that("correct error when wrong template", {
  a <- data.frame(letters[1:3], 1:3)
  expect_error(bubble_blow(a, template = "forzainter"), "exist")
  expect_error(bubble_blow(a, template = "basic1"), "pie_bake")
  expect_error(bubble_blow(a, template = "dart4"), "pie_bake_pro")
})

test_that("data_check is properly linked", {
  a <- data.frame(1:3, letters[1:3])
  expect_error(bubble_blow(a, template = "bub1", perc = TRUE), "second variable")
  expect_error(bubble_blow(a, template = "bub2"), "second variable")
})

test_that("output is a 'ggplot' object", {
  a <- data.frame(letters[1:3], 1:3)
  expect_true(is_ggplot(bubble_blow(a, template = "bub1")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub2")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub3")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub4")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub5")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub1", title = "gg")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub2", title = "gg")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub3", title = "gg")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub4", title = "gg")))
  expect_true(is_ggplot(bubble_blow(a, template = "bub5", title = "gg")))
})

test_that("title specification works", {
  a <- data.frame(letters[1:3], 1:3)
  out1 <- bubble_blow(a, template = "bub1", perc = "below")
  out2 <- bubble_blow(a, template = "bub2", perc = "below", title = "ciaociao")
  out3 <- bubble_blow(a, template = "bub3", perc = "below", title = "test")
  out4 <- bubble_blow(a, template = "bub4", perc = "below", title = "test")
  out5 <- bubble_blow(a, template = "bub5", perc = "below", title = "test")
  out6 <- bubble_blow(a, template = "bub1", perc = "right", title = "test")
  out7 <- bubble_blow(a, template = "bub2", perc = "right", title = "testone")
  expect_match(out1$labels$title, "")
  expect_match(out2$labels$title, "ciaociao")
  expect_match(out3$labels$title, "test")
  expect_match(out4$labels$title, "test")
  expect_match(out5$labels$title, "test")
  expect_match(out6$labels$title, "test")
  expect_match(out7$labels$title, "testone")
})

test_that("also tibbles can be blown", {
  a <- data.frame(letters[1:3], 1:3)
  b <- tibble::tibble(letters[1:5], 1:5)
  c <- tibble::tibble(letters[1:5], c(4.3, 2.2, 1, 2, 3.0))

  expect_true(is_ggplot(bubble_blow(tibble::as_tibble(a), template = "bub3", perc = "right")))
  expect_true(is_ggplot(bubble_blow(b, template = "bub4", perc = "right")))
  expect_true(is_ggplot(bubble_blow(b, template = "bub5", perc = "right")))
})

test_that("message when wrong input for perc", {
  a <- data.frame(letters[1:3], 1:3)
  expect_message(bubble_blow(a, template = "bub1", perc = TRUE), "not valid")
  expect_message(bubble_blow(a, template = "bub1", perc = FALSE), "not valid")
  expect_message(bubble_blow(a, template = "bub1", perc = "left"), "not valid")
  expect_message(bubble_blow(a, template = "bub1", perc = "coseacaso"), "not valid")
})
