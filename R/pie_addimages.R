#' Add images to pie charts
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
