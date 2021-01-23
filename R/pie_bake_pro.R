#' Easily create (more complex) pie charts
#'
#' This function allows you to create cool pie charts easily by providing just
#' the data and a template among the available ones. Moreover, you can optionally
#' specify a title for the chart and a name for the categories.\cr \cr
#' Using this function you can create some *complex and extravagant* pie charts.
#' If you are looking for something more *classical* (and probably more understandable),
#'  check out the \code{\link{pie_bake}} function.
#'
#' @param data A data frame with two variables (columns):\cr
#' - in the first one there must be the vector of labels;\cr
#' - in the second one there must be the vector of values.
#'
#' You can use \code{\link{pie_datacheck}} to understand if the data is suitable.
#' @param template The chosen template.\cr
#' Type \code{pie_template_list_pro} to display all the available ones for this function.
#' @param group_name A string. If you want, you can specify a name for the categories.
#' @param title A string. If you want, you can specify the title of the graph.
#'
#' @export
#'
#' @seealso
#' See all the available templates displayed
#' \href{https://paolodalena.github.io/tastypie/articles/available_templates.html}{here}!
#'
#' @examples
#' example <- data.frame(
#'   c("a. This", "b. Is", "c. Just", "d. An", "e. Example"),
#'   c(2.9, 6.9, 4.20, 13.12, 6.66)
#' )
#' pie_bake_pro(
#'   data = example,
#'   template = "eaten3",
#'   group_name = "cat:",
#'   title = "Example1"
#' )
#'
#' pie_bake_pro(
#'   data = example,
#'   template = "dart1",
#'   title = "Example2!"
#' )
#'
#' pie_bake_pro(
#'   data = example,
#'   template = "eye5",
#'   group_name = "GROUPS:"
#' )
#'
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shadowtext geom_shadowtext
pie_bake_pro <- function(
  data,
  template,
  group_name = "group",
  title = ""
){

  # useful checks:
  pie_datacheck(data)
  if(template %in% pie_template_list){
    stop("\n The selected template must be used with the function pie_bake().
 Type pie_template_list_pro to see all the available templates for this function.
 Type pie_template_list to see all the available templates for the function pie_bake().")
  }
  else if(!template %in% pie_template_list_pro){
    stop("\n The selected template does NOT exist.
 Type pie_template_list_pro to see all the available templates for this function.
 Type pie_template_list to see all the available templates for the function pie_bake().")
  }

  # initial setup
  names(data) <- c("group", "value")
  data_n <- arrange(data, desc(group))
  data_n[,2] <- round(data_n[,2]/sum(data_n[,2])*100, 0)

  # eaten ------
  if(template == "eaten1"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none")+
      scale_fill_brewer(palette = "Set3", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eaten2"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none")+
      scale_fill_brewer(palette = "YlOrRd", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eaten3"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Spectral", name = group_name)+
      geom_text(aes(y = value+ 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = "black", family = "mono", fontface = "bold", size=5)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eaten4"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none")+
      scale_fill_brewer(palette = "Spectral", name = group_name)+
      geom_shadowtext(aes(y = value+ 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = rev(brewer.pal(length(data_n[,1]), "Spectral")), family = "mono", fontface = "bold", size=5)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eaten5"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Greys", name = group_name)+
      geom_text(aes(y = value+ 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = "black", family = "mono", fontface = "bold", size=5)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }

  # dart -------
  else if(template == "dart1"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar(theta = "y")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Set3", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "dart2"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar(theta = "y")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "YlOrRd", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "dart3"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar(theta = "y")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Spectral", name = group_name)+
      geom_shadowtext(aes(y = value - 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                    color = rev(brewer.pal(length(data_n[,1]), "Spectral")), family = "mono", fontface = "bold", size=5)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "dart4"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar(theta = "y")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Greys", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "dart5"){
    ggplot(data_n, aes(fill = data_n[,1], x = data_n[,1], y = data_n[,2]))+
      geom_bar(width = 1, color = "black", stat = "identity")+
      coord_polar(theta = "y")+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Accent", name = group_name)+
      geom_shadowtext(aes(y = value - 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = rev(brewer.pal(length(data_n[,1]), "Accent")), family = "mono", fontface = "bold", size=5)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }

  # eye ------
  else if(template == "eye1"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Set3", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eye2"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "YlOrRd", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eye3"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Greens", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eye4"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Greys", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "eye5"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar()+
      xlab("")+
      ylab("")+
      ggtitle(title)+
      theme_void()+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_brewer(palette = "Blues", name = group_name)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }

  # watermelon -----
  else if(template == "watermelon1"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "Set3", name = group_name)+
      ggtitle(title)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "watermelon2"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "Spectral", name = group_name)+
      ggtitle(title)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "watermelon3"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "orange")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "YlOrRd", name = group_name)+
      ggtitle(title)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "watermelon4"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "white")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "BrBG", name = group_name)+
      ggtitle(title)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
  else if(template == "watermelon5"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "RdPu", name = group_name)+
      ggtitle(title)+
      theme(plot.title = element_text(
        family = "mono", color="black", size=16, face="bold", hjust = 0.5))
  }
}
