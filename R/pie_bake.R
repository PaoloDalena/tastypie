#' Easily create pie charts
#'
#' This function allows you to create cool pie charts easily by providing just
#' the data and a template among the available ones. Moreover, you can choose whether
#'to display percentages or not and also optionally specify a title for the chart
#' and a name for the categories.\cr \cr
#' Using this function you can create the *classical* pie charts (including the donut charts).
#' If you are looking for something more *complex and extravagant* (but probably less
#' understandable), check out the \code{\link{pie_bake_pro}} function.
#'
#' @param data A data frame with two variables (columns):\cr
#' - in the first one there must be the vector of labels;\cr
#' - in the second one there must be the vector of values.
#'
#' You can use \code{\link{pie_datacheck}} to understand if the data is suitable.
#' @param template The chosen template.\cr
#' Type \code{pie_template_list} to display all the available ones for this function.
#' @param perc A logical value. Should the proportions be displayed?
#' @param group_name A string. If you want, you can specify a name for the categories.
#' @param title A string. If you want, you can specify the title of the graph.
#'
#' @export
#' @examples
#' example <- data.frame(
#'   c("a. This", "b. Is", "c. Just", "d. An", "e. Example"),
#'   c(2.9, 6.9, 4.20, 13.12, 6.66)
#' )
#' pie_bake(
#'   data = example,
#'   template = "basic3",
#'   perc = TRUE,
#'   group_name = "groups",
#'   title = "Example1"
#' )
#'
#' pie_bake(
#'   data = example,
#'   template = "red1",
#'    title = "Ex2"
#' )
#'
#' pie_bake(
#'   data = example,
#'   template = "donut2",
#'   perc = TRUE,
#'   group_name = "CAT:"
#' )
#'
#' @seealso
#' See all the available templates displayed
#' \href{https://paolodalena.github.io/tastypie/articles/available_templates.html}{here}!
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom utils head
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shadowtext geom_shadowtext
#' @import ggplot2
pie_bake <- function(
  data,
  template,
  perc = FALSE,
  group_name = "group",
  title = ""
  ){

  # useful checks:
  pie_datacheck(data)
  if(template %in% pie_template_list_pro){
    stop("\n The selected template must be used with the function pie_bake_pro().
 Type pie_template_list to see all the available templates for this function.
 Type pie_template_list_pro to see all the available templates for the function pie_bake_pro().")
  }
  else if(!template %in% pie_template_list){
    stop("\n The selected template does NOT exist.
 Type pie_template_list to see all the available templates for this function.
 Type pie_template_list_pro to see all the available templates for the function pie_bake_pro().")
  }

  # initial setup
  names(data) <- c("group", "value")
  data_n <- arrange(data, desc(group))
  data_n[,2] <- round(data_n[,2]/sum(data_n[,2])*100, 0)

  # without percentage ------------------------------------------
  if(!perc){
    # * basic ---------
    if(template == "basic1"){
      ggplot(data_n, aes(x="", y=data_n[,2], fill=data_n[,1])) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_discrete(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set3", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set2", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Paired", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_linedraw()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Accent", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * bw ------------
    else if(template == "bw1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "beige") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * blue --------
    else if(template == "blue1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    # * red ------
    else if(template == "red1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkgoldenrod") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    # * rainbow ---------
    else if(template == "rainbow1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "antiquewhite4") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * donut -------
    else if(template == "donut1"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        ggtitle(title)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut2"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(-1, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Accent", name = group_name)+
        ggtitle(title)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut3"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        ggtitle(title)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut4"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        ggtitle(title)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut5"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2.5, 4))+
        theme_void()+
        scale_fill_grey(name = group_name)+
        ggtitle(title)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
  }

  # with percentage ------------------------------------------------------
  else if(perc){
    # * basic ------------------
    if(template == "basic1"){
      ggplot(data_n, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity", color = "black")+
        coord_polar("y", start=0)+
        theme_void()+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                      color = "black", bg.color = "white", family = "mono", fontface = "bold", size=5)+
        ggtitle(title)+
        scale_fill_discrete(name = group_name)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set3", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                      color = rev(brewer.pal(length(data_n[,1]), "Set3")),
                      bg.color = "black",family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set2", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "white", family = "mono", fontface = "bold",size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Paired", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold",size=5,
                  bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "basic5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_linedraw()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Accent", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "sans", fontface = "bold",size=5,
                  bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * bw ----------
    else if(template == "bw1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "white", family = "mono", fontface = "bold", size=5,
                  color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "beige") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "beige", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "yellow", family = "mono", fontface = "bold", size=5,
                  color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "aquamarine", family = "mono", fontface = "bold", size=5,
                  color = brewer.pal(length(data_n[,1]), "Greys"))+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "bw5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "deeppink", family = "mono", fontface = "bold", size=5,
                  color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * blue ------
    else if(template == "blue1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5,
                  bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkorange", family = "mono", fontface = "bold", size=5,
                  bg.color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "antiquewhite4", family = "mono", fontface = "bold", size=5,
                  bg.color = "yellow")+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "black", family = "mono", fontface = "bold", size=5,
                  color = rev(brewer.pal(length(data_n[,1]), "Blues")))+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "blue5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "deeppink", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="darkblue", size=16, face="bold", hjust = 0.5))
    }
    # * red ------
    else if(template == "red1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5,
                  bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "black", family = "mono", fontface = "bold", size=5,
                  color = rev(brewer.pal(length(data_n[,1]), "Oranges")))+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkgoldenrod") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5,
                  bg.color = "darkgoldenrod")+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "aquamarine2", family = "mono", fontface = "bold", size=5,
                  bg.color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    else if(template == "red5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "deeppink", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="darkred", size=14, face="bold", hjust = 0.5))
    }
    # * rainbow ---------
    else if(template == "rainbow1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5,
                  bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "black", family = "mono", fontface = "bold", size=5,
                  color = rev(brewer.pal(length(data_n[,1]), "Spectral")))+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "antiquewhite4") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "aquamarine", family = "mono", fontface = "bold", size=5,
                  color = "black")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "rainbow5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_shadowtext(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = scales::label_percent(accuracy = 1)(value/sum(value))),
                  bg.color = "deeppink", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    # * donut------
    else if(template == "donut1"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))
      data_n$labelPosition <- (data_n$ymax + data_n$ymin) / 2

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        ggtitle(title)+
        geom_shadowtext( x=3.5, aes(y=labelPosition,
                               label=scales::label_percent(accuracy = 1)(value/sum(value))),
                   color = "black", family = "mono", fontface = "bold", size=5,
                   bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut2"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))
      data_n$labelPosition <- (data_n$ymax + data_n$ymin) / 2

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(-1, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Accent", name = group_name)+
        ggtitle(title)+
        geom_shadowtext( x=3.5, aes(y=labelPosition,
                              label=scales::label_percent(accuracy = 1)(value/sum(value))),
                   bg.color = "black", family = "mono", fontface = "bold", size=5,
                   color = rev(brewer.pal(length(data_n[,1]), "Accent")))+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut3"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))
      data_n$labelPosition <- (data_n$ymax + data_n$ymin) / 2

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        ggtitle(title)+
        geom_shadowtext( x=3.5, aes(y=labelPosition,
                              label=scales::label_percent(accuracy = 1)(value/sum(value))),
                   bg.color = "blue4", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut4"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))
      data_n$labelPosition <- (data_n$ymax + data_n$ymin) / 2

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        ggtitle(title)+
        geom_shadowtext( x=3.5, aes(y=labelPosition,
                              label=scales::label_percent(accuracy = 1)(value/sum(value))),
                   color = "darkred", family = "mono", fontface = "bold", size=5,
                   bg.color = "white")+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
    else if(template == "donut5"){
      data_n$fraction = data_n[,2] / sum(data_n[,2])
      data_n$ymax = cumsum(data_n$fraction)
      data_n$ymin = c(0, head(data_n$ymax, n=-1))
      data_n$labelPosition <- (data_n$ymax + data_n$ymin) / 2

      ggplot(data_n, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
        geom_rect() +
        coord_polar(theta="y")+
        xlim(c(2.5, 4))+
        theme_void()+
        scale_fill_grey(name = group_name)+
        ggtitle(title)+
        geom_shadowtext( x=3.5, aes(y=labelPosition,
                              label=scales::label_percent(accuracy = 1)(value/sum(value))),
                   bg.color = "black", family = "mono", fontface = "bold", size=5)+
        theme(plot.title = element_text(
          family = "mono", color="black", size=16, face="bold", hjust = 0.5))
    }
  }
}
