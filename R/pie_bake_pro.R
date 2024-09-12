#' Easily create (more complex) pie charts
#'
#' This function allows you to create cool pie charts easily by providing just
#' the data and a template among the available ones. Moreover, you can optionally
#' specify a title for the chart and a name for the categories.\cr \cr
#' Using this function you can create some *complex and extravagant* pie charts.
#' If you are looking for something more *classical* (and probably more understandable),
#'  check out the \code{\link{pie_bake}} function.
#'
#' @param data A data frame (or a tibble) with two variables (columns):\cr
#' - in the first one there must be the vector of labels;\cr
#' - in the second one there must be the vector of values.
#'
#' Please note that the labels are automatically sorted in alphabetical order.
#' If you want to specify a particular order, it is recommended to type numbers or
#' letters before the category names (e.g. "a. category1", "b. category2", ...).\cr
#' You can use \code{\link{pie_datacheck}} to understand if the data is suitable.
#' @param template The chosen template.\cr
#' Type \code{pie_template_list_pro} to display all the available ones for this function.
#' @param group_name A string. If you want, you can specify a name for the categories.
#' @param title A string. If you want, you can specify the title of the graph.
#'
#' @export
#'
#' @return
#' A ggplot object (or no value if the chosen template is among the spider chart ones).
#' In particular, this function returns a pie (or similar) chart according to the
#' data, the choice of template, and the other specifications provided.
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
#' pie_bake_pro(
#'   data = example,
#'   template = "spider2"
#'   )
#'
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shadowtext geom_shadowtext
#' @importFrom fmsb radarchart
pie_bake_pro <- function(
  data,
  template,
  group_name = "group",
  title = ""
){

  # useful checks:
  pie_datacheck(data)

  data <- as.data.frame(data)

  if(template %in% pie_template_list){
    stop("\n The selected template must be used with the function pie_bake().
 Type pie_template_list_pro to see all the available templates for this function.
 Type pie_template_list to see all the available templates for the function pie_bake().")
  }
  else if(!template %in% c(pie_template_list_pro)){
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
  # circular barplot ----
  else if(template == "cirbar1"){
    data <- data.frame(
      id = seq(1, length(data$value)),
      individual = data$group,
      value = round(data$value/sum(data$value)*100, 0)
    )

    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(data, aes(x=as.factor(id), y=value)) +
      geom_bar(stat="identity", fill=alpha("darkolivegreen", 5)) +
      ylim(-10, max(data$value) + 10) +
      theme_void() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5)
      ) +
      coord_polar(start = 0) +
      geom_shadowtext(
        data=label_data,
        aes(x=id, y=value+10, label=individual, hjust=hjust),
        family = "mono", color="black", fontface="bold", bg.color = "orange",
        alpha = 1, size = 4,
        angle= label_data$angle, inherit.aes = FALSE) +
      ggtitle(title)
  }
  else if(template == "cirbar2"){
    data <- data.frame(
      id = seq(1, length(data$value)),
      individual = data$group,
      value = round(data$value/sum(data$value)*100, 0)
    )
    data$individual <- paste(
      data$individual,
      scales::label_percent(accuracy = 1)(data$value/sum(data$value)),
      sep = "\n")

    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(data, aes(x=as.factor(id), y=value, fill = as.factor(id))) +
      geom_bar(stat="identity") +
      ylim(-10, max(data$value) + 10) +
      theme_void() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5),
        legend.position="none"
      ) +
      coord_polar(start = 0) +
      geom_label(
        data=label_data,
        aes(x=id, y=value+10, label=individual, hjust=hjust),
        family = "mono", color="black", fontface="bold",
        alpha = 1, size = 4,
        angle= label_data$angle, inherit.aes = FALSE) +
      ggtitle(title)
  }
  else if(template == "cirbar3"){
    data <- data.frame(
      id = seq(1, length(data$value)),
      individual = data$group,
      value = round(data$value/sum(data$value)*100, 0)
    )

    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(data, aes(x=as.factor(id), y=value, color = as.factor(id))) +
      geom_bar(stat="identity", fill= "white") +
      ylim(-10, max(data$value) + 10) +
      theme_void() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5),
        legend.position="none"
      ) +
      coord_polar(start = 0) +
      geom_shadowtext(
        data=label_data,
        aes(x=id, y=value+10, label=individual, hjust=hjust),
        family = "mono", color="black", fontface="bold", bg.color = "yellow",
        alpha = 1, size = 4,
        angle= label_data$angle, inherit.aes = FALSE) +
      ggtitle(title)
  }
  else if(template == "cirbar4"){
    data <- data.frame(
      id = seq(1, length(data$value)),
      individual = data$group,
      value = round(data$value/sum(data$value)*100, 0)
    )

    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(data, aes(x=as.factor(id), y=value, color = as.factor(id))) +
      geom_bar(stat="identity", fill= "black") +
      ylim(-10, max(data$value) + 10) +
      theme_void() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5),
        legend.position="none"
      ) +
      coord_polar(start = 0) +
      geom_label(
        data=label_data,
        aes(x=id, y=value+10, label=individual, hjust=hjust),
        family = "mono", color="white", fontface="bold", bg = "black",
        alpha = 1, size = 4,
        angle= label_data$angle, inherit.aes = FALSE) +
      ggtitle(title)
  }
  else if(template == "cirbar5"){
    data <- data.frame(
      id = seq(1, length(data$value)),
      individual = data$group,
      value = round(data$value/sum(data$value)*100, 0)
    )

    data$individual <- paste(
      data$individual,
      scales::label_percent(accuracy = 1)(data$value/sum(data$value)),
      sep = "\n")

    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    label_data$angle<-ifelse(angle < -90, angle+180, angle)

    ggplot(data, aes(x=as.factor(id), y=value, color = 3)) +
      geom_bar(stat="identity", fill = alpha("lightblue", 0.8)) +
      ylim(-10, max(data$value) + 10) +
      theme_void() +
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5),
        legend.position="none"
      ) +
      coord_polar(start = 0) +
      geom_shadowtext(
        data=label_data,
        aes(x=id, y=value+10, label=individual, hjust=hjust),
        family = "mono", color="lightblue", fontface="bold", bg.color = "darkcyan",
        alpha = 1, size = 4,
        angle= label_data$angle, inherit.aes = FALSE) +
      ggtitle(title)
  }
  # spider chart ----
  else if(template == "spider1"){
    lbls <- paste(data_n$group, data_n$value, sep = " : ")
    lbls <- paste(lbls, "%", sep = "")

    b <- matrix(nrow = 3,
                ncol = length(data$value),
                dimnames = list(c("min", "max", "value"), lbls)
    )
    b[1,] <- rep(max(data_n$value), length(data_n$value))
    b[2,] <- rep(0, length(data_n$value))
    b[3,] <- data_n$value
    b <- as.data.frame(b)

    radarchart(b,
               axistype = 1,
               pcol = "darkred",
               pfcol = alpha("orangered", 0.7),
               plwd = 3,
               cglcol = "darkred",
               cglty = 1,
               axislabcol = "darkred",
               cglwd = 0.5,
               caxislabels=seq(0,max(b[3,]), length.out = 5))
  }
  else if(template == "spider2"){
    lbls <- paste(data_n$group, data_n$value, sep = " : ")
    lbls <- paste(lbls, "%", sep = "")

    b <- matrix(nrow = 3,
                ncol = length(data$value),
                dimnames = list(c("min", "max", "value"), lbls)
    )
    b[1,] <- rep(max(data_n$value), length(data_n$value))
    b[2,] <- rep(0, length(data_n$value))
    b[3,] <- data_n$value
    b <- as.data.frame(b)

    radarchart(b,
               axistype = 1,
               pcol = "darkolivegreen",
               pfcol = alpha("darkcyan", 0.3),
               plwd = 2,
               cglcol = "grey",
               cglty = 4,
               axislabcol = "black",
               cglwd = 2,
               caxislabels=seq(0,max(b[3,]), length.out = 5))
  }
  else if(template == "spider3"){
    lbls <- paste(data_n$group, data_n$value, sep = " : ")
    lbls <- paste(lbls, "%", sep = "")

    b <- matrix(nrow = 3,
                ncol = length(data$value),
                dimnames = list(c("min", "max", "value"), lbls)
    )
    b[1,] <- rep(max(data_n$value), length(data_n$value))
    b[2,] <- rep(0, length(data_n$value))
    b[3,] <- data_n$value
    b <- as.data.frame(b)

    radarchart(b,
               axistype = 0,
               pcol = "skyblue",
               pfcol = alpha("skyblue", 0.3),
               plwd = 3,
               cglcol = "grey2",
               cglty = 3,
               cglwd = 2
    )
  }
  else if(template == "spider4"){
    lbls <- paste(data_n$group, data_n$value, sep = " : ")
    lbls <- paste(lbls, "%", sep = "")

    b <- matrix(nrow = 3,
                ncol = length(data$value),
                dimnames = list(c("min", "max", "value"), lbls)
    )
    b[1,] <- rep(max(data_n$value), length(data_n$value))
    b[2,] <- rep(0, length(data_n$value))
    b[3,] <- data_n$value
    b <- as.data.frame(b)

    radarchart(b,
               axistype = 1,
               pcol = "red",
               plwd = 4,
               cglcol = "grey",
               cglty = 1,
               axislabcol = "black",
               cglwd = 0.5,
               caxislabels=seq(0,max(b[3,]), length.out = 5)
               )
  }
  else if(template == "spider5"){
    lbls <- paste(data_n$group, data_n$value, sep = " : ")
    lbls <- paste(lbls, "%", sep = "")

    b <- matrix(nrow = 3,
                ncol = length(data$value),
                dimnames = list(c("min", "max", "value"), lbls)
    )
    b[1,] <- rep(max(data_n$value), length(data_n$value))
    b[2,] <- rep(0, length(data_n$value))
    b[3,] <- data_n$value
    b <- as.data.frame(b)

    radarchart(b,
               axistype = 1,
               pcol = "deeppink",
               pfcol = alpha("pink", 0.3),
               plwd = 2,
               cglcol = "black",
               cglty = 6,
               axislabcol = "deeppink",
               cglwd = 2,
               caxislabels=seq(0,max(b[3,]), length.out = 5)
    )
  }
}
