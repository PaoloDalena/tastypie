test_that("output is a 'ggplot' object and title specification", {
  img <- jpeg::readJPEG(system.file("img", "pie.jpeg", package = "tastypie"))
  imgs <- list(img, img, img)
  df <- data.frame(
    c("A", "B", "C"),
    c(300, 250, 600)
  )
  mypie <- pie_bake(df, template = "rainbow1")
  imgpie <-   pie_addimages(
    mypie = mypie,
    imglist = imgs,
    perc = "right",
    lbl_col = "darkcyan",
    border_col = "orangered",
    title = "Example"
  )

  expect_true(is_ggplot(imgpie))
  expect_match(imgpie$labels$title, "Example")
})
