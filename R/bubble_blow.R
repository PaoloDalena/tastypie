#' Easily create circular packing charts
#'
#' This function allows you to create cool circular packing charts easily by providing just
#' the data and a template among the available ones. Moreover, you can choose whether and
#' where to display percentages and also optionally specify a title for the chart.
#'
#' @param data A data frame (or a tibble) with two variables (columns):\cr
#' - in the first one there must be the vector of labels;\cr
#' - in the second one there must be the vector of values.\cr
#' You can use \code{\link{pie_datacheck}} to understand if the data is suitable.
#' @param template The chosen template.\cr
#' The available templates for this function are 'bub1', 'bub2', 'bub3', 'bub4', 'bub5'.
#' @param perc You can choose among:\cr
#' - 'no'    : the percentages won't be displayed (default choice);
#' - 'below' : the percentages will be displayed below the group labels;
#' - 'right' : the percentages will be displayed next to the group labels.
#' @param title A string. If you want, you can specify the title of the graph.
#'
#' @return
#' A ggplot object.
#' In particular, this function returns a circular packing chart according to the
#' data, the choice of the template, and the other specifications provided.
#'
#'@examples
#' example <- data.frame(
#'   c("This", "Is", "Just", "An", "Example"),
#'   c(2.9, 6.9, 4.20, 13.12, 6.66)
#' )
#' bubble_blow(
#'   data = example,
#'   template = "bub1",
#'   perc = "below",
#'   title = "Example1"
#' )
#'
#'example2 <- tibble::tibble(
#'   c("cat 1", "cat2", "cat3", "cat4", "cat5", "cat6", "cat7", "cat8", "cat9"),
#'   c(324, 432, 499, 291, 750, 836, 314, 133, 372)
#' )
#' bubble_blow(
#'   data = example2,
#'   template = "bub2",
#'   perc = "right",
#'   title = "Ex2"
#' )
#'
#'@seealso
#' See all the available templates displayed
#' \href{https://paolodalena.github.io/tastypie/articles/available_templates.html}{here}!
#'
#' @importFrom packcircles circleProgressiveLayout circleLayoutVertices
#' @export

bubble_blow <- function(
  data,
  template,
  perc = "no",
  title = ""
){

  # useful checks:
  pie_datacheck(data)

  data <- as.data.frame(data)

  names(data) <- c("group", "value")

  if(template %in% pie_template_list_pro){
    stop("\n The selected template must be used with the function pie_bake_pro().
 The available templates for this function are 'bub1', 'bub2', 'bub3', 'bub4', 'bub5'.")
  }
  else if(template %in% pie_template_list){
    stop("\n The selected template must be used with the function pie_bake().
 The available templates for this function are 'bub1', 'bub2', 'bub3', 'bub4', 'bub5'.")
  }
  else if(!template %in% c("bub1", "bub2", "bub3", "bub4", "bub5")){
    stop("\n The selected template does not exist.
 The available templates for this function are 'bub1', 'bub2', 'bub3', 'bub4', 'bub5'.")
  }

  # add useful info
  usf <- circleProgressiveLayout(data$value, sizetype='area')
  data <- cbind(data, usf)
  dat.gg <- circleLayoutVertices(usf, npoints=50)
  dat.gg$value <- rep(data$value, each=51)
  dat.gg$perc <- rep(data$value/sum(data$value), each=51)

  # check for percentage label
  if(perc == "below"){
    data$group <- paste(
      data$group,
      scales::label_percent(accuracy = 1)(data$value/sum(data$value)),
      sep = "\n")
  }
  else if(perc == "right"){
    data$group <- paste(
      data$group,
      scales::label_percent(accuracy = 1)(data$value/sum(data$value)),
      sep = " : ")
  }
  else if(perc == "no"){}
  else{
    message("The selected value for perc is not valid. You can choose between 'no', 'below' and 'right'.")
  }

  if(template == "bub1"){
    ggplot() +
       geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "black", alpha = 1, linewidth =1) +
       scale_fill_distiller(palette = "Set3", direction = 1 ) +
       geom_label(data = data, aes(x, y, size=value, label = group), family = "mono") +
       scale_size_continuous(range = c(2, 5)) +
       theme_void() +
       theme(
         legend.position="none",
         plot.title = element_text(
           family = "mono", color="black", size=16, face="bold", hjust = 0.5)
       ) +
       coord_equal()+
       ggtitle(title)
  }
  else if(template == "bub2"){
    ggplot() +
    # Make the bubbles
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "white", alpha = 2, linewidth = 2) +
      scale_fill_distiller(palette = "Spectral", direction = 1 ) +
      # Add text in the center of each bubble + control its size
      geom_label(data = data, aes(x, y, size=value, label = group, fill = value), family = "mono") +
      scale_size_continuous(range = c(2, 5)) +
      theme_void() +
      theme(
        legend.position="none",
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5)
      ) +
      coord_equal()+
      ggtitle(title)
  }
  else if(template == "bub3"){
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "darkred", alpha = 1, linewidth = 1) +
      scale_fill_distiller(palette = "Oranges", direction = 1 ) +
      geom_label(data = data, aes(x, y, size=value, label = group), colour = "darkred", family = "mono") +
      scale_size_continuous(range = c(2, 5)) +
      theme_void() +
      theme(
        legend.position="none",
        plot.title = element_text(
          family = "mono", color="darkred", size=16, face="bold", hjust = 0.5)
      ) +
      coord_equal()+
      ggtitle(title)
  }

  else if(template == "bub4"){
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "black", alpha = 2, linewidth = 2) +
      scale_fill_distiller(palette = "Greys", direction = 1 ) +
      geom_label(data = data, aes(x, y, size=value, label = group), family = "mono") +
      scale_size_continuous(range = c(2, 5)) +
      theme_void() +
      theme(
        legend.position="none",
        plot.background = element_rect(fill="black"),
        plot.title = element_text(
          family = "mono", color="white", size=16, face="bold", hjust = 0.5)
      ) +
      coord_equal()+
      ggtitle(title)
  }
  else if(template == "bub5"){
    ggplot() +
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "white", alpha = 1, linewidth = 1) +
      scale_fill_distiller(palette = "Paired", direction = 1 ) +
      geom_label(data = data, aes(x, y, size=value, label = group), family = "mono") +
      scale_size_continuous(range = c(2, 5)) +
      theme_void() +
      theme(
        legend.position="none",
        plot.background = element_rect(fill="black"),
        plot.title = element_text(
          family = "mono", color="white", size=16, face="bold", hjust = 0.5)
      ) +
      coord_equal()+
      ggtitle(title)
  }
}
