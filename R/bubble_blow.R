# to be tested and documented

bubble_blow <- function(
  data,
  template,
  perc = "no",
  title = ""
){

  # useful checks:
  pie_datacheck(data)

  if(tibble::is_tibble(data)){
    data <- as.data.frame(data)
  }

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
       geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "black", alpha = 1, size=1) +
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
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "white", alpha = 2, size=2) +
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
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "darkred", alpha = 1, size=1) +
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
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "black", alpha = 2, size = 2) +
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
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=value), colour = "white", alpha = 1, size = 1) +
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
