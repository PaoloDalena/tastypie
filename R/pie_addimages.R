#' Add images to pie charts
#'
#' This function allows to easily add some figures to you pie chart, taking information
#'  from the output of the \code{\link{pie_bake}} function.
#'  You can also customize the chart by specifying the colors of borders and labels, the
#'  title of the pie chart and where (and if) you want to print the percentages
#'  for each group.
#'
#' @param mypie A ggplot object. The output from \code{\link{pie_bake}},
#' \strong{NOT from \code{\link{pie_bake_pro}}}. \cr
#' Don't focus on the template, only labels and values will be inherited.
#' @param imglist A list of objects returned by and \code{\link[jpeg]{readJPEG}}
#'  used to fill slices. For optimal and fast results, we recommend the use of
#'  small files (5-10 kB).
#' @param perc You can choose among:\cr
#' - \code{'no'}    : the percentages won't be displayed (default choice);
#' - \code{'below'} : the percentages will be displayed below the group labels;
#' - \code{'right'} : the percentages will be displayed next to the group labels.
#' @param lbl_col A string containing the chosen color for the labels, default is
#'  \code{"black"}.
#' @param border_col A string containing the chosen color for the border of the pie
#'  chart, default is \code{"black"}.
#' @param title A string. If you want, you can specify the title of the graph.
#'
#' @return
#' A ggplot object.
#'
#' @examples
#' img5 <- jpeg::readJPEG(system.file("img", "pie.jpeg", package = "tastypie"))
#' imgs2 <- list(img5, img5, img5)
#'
#' df2 <- data.frame(
#'   c("A", "B", "C"),
#'   c(300, 250, 600)
#' )
#'
#' mypie2 <- pie_bake(df2, template = "rainbow1")
#'
#' pie_addimages(
#'   mypie = mypie2,
#'   imglist = imgs2,
#'   perc = "right",
#'   lbl_col = "darkcyan",
#'   border_col = "orangered",
#'   title = "Example"
#' )
#'
#' @seealso
#' Please note that this function is based on the \code{\link[patternplot]{imagepie}}
#' function.
#' @export
pie_addimages <- function(
  mypie,
  imglist,
  perc = "no",
  lbl_col = "black",
  border_col = "black",
  title = ""
  ){
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
                        label.color=lbl_col) + ggtitle(title)
}
