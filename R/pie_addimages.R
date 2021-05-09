#' Add images to pie charts
#'
#' This function allows to easily add some figures to you pie chart, taking information
#'  from the output of the \code{\link{pie_bake}} function.
#'  You can also customize the chart by specifying the colors of borders and labels
#'  and where (and if) you want to print the percentages for each group.
#'
#' @param mypie A ggplot object. The output from \code{\link{pie_bake}},
#' \strong{NOT from \code{\link{pie_bake_pro}}}. \cr
#' Don't focus on the template, only lables and values will be inherited.
#' @param imglist A list of objects returned by and \code{\link[jpeg]{readJPEG}}
#'  used to fill slices.
#' @param perc You can choose among:\cr
#' - \code{'no'}    : the percentages won't be displayed (default choice);
#' - \code{'below'} : the percentages will be displayed below the group labels;
#' - \code{'right'} : the percentages will be displayed next to the group labels.
#' @param lbl_col A string containing the chosen color for the labels, default is
#'  \code{"black"}.
#' @param border_col A string containing the chosen color for the border of the pie
#'  chart, default is \code{"black"}.
#'
#' @return
#' A ggplot object.
#'
#' @examples
#' img1 <- jpeg::readJPEG(system.file("img", "logo.jpeg", package="tastypie"))
#' img2 <- jpeg::readJPEG(system.file("img", "brownie.jpeg", package="tastypie"))
#' img3 <- jpeg::readJPEG(system.file("img", "mark.jpeg", package="tastypie"))
#' imgs <- list(img1,img2,img3)
#'
#'  df <- data.frame(
#'    c("Mark fan", "My brownie", "Tastypie logo"),
#'    c(500, 200, 300)
#'  )
#'
#'  mypie <- pie_bake(df, template = "basic1")
#'
#'  pie_addimages(
#'    mypie = mypie,
#'    imglist = imgs,
#'    perc = "below",
#'    lbl_col = "red",
#'    border_col = "grey"
#'  )
#'
#' @export
pie_addimages <- function(mypie, imglist, perc = "no", lbl_col = "black", border_col = "black"){
  if(perc == "below"){
    lbls = paste(mypie$data$group,
                 scales::label_percent(accuracy = 1)(mypie$data$value/sum(mypie$data$value)),
                 sep = " \n ")
  }
  else if(perc == "right"){
    lbls = paste(mypie$data$group,
                 scales::label_percent(accuracy = 1)(mypie$data$value/sum(mypie$data$value)),
                 sep = " : ")
  }
  else if(perc == "no"){
    lbls = mypie$data$group
  }else{
    message("The selected value for perc is not valid. You can choose between 'no', 'below' and 'right'.")
    }
  patternplot::imagepie(group = mypie$data$group, pct = mypie$data$value,
                        pattern.type = imglist, label = lbls, label.distance=1.3,
                        frame.color=border_col, frame.size=3, label.size=5,
                        label.color=lbl_col)
}
