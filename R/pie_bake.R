#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom ggplot2 ggplot
#' @importFrom scales label_percent
pie_bake <- function(
  data,
  template,
  perc = FALSE,
  group_name = "group",
  title = ""
  ){

  # useful checks:
  pie_datacheck(data)
  if(!template %in% pie_template_list){
    stop("\n The selected template does NOT exist. \n Type pie_template_list to see all the available templates.")
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
        scale_fill_discrete(name = group_name)
    }
    else if(template == "basic2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set3", name = group_name)
    }
    else if(template == "basic3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set2", name = group_name)
    }
    else if(template == "basic4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Paired", name = group_name)
    }
    else if(template == "basic5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_linedraw()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Accent", name = group_name)
    }
    # * bw ------------
    else if(template == "bw1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)
    }
    else if(template == "bw2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "beige") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)
    }
    else if(template == "bw3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)
    }
    else if(template == "bw4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)
    }
    else if(template == "bw5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)
    }
    # * blue --------
    else if(template == "blue1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)
    }
    else if(template == "blue2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)
    }
    else if(template == "blue3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)
    }
    else if(template == "blue4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)
    }
    else if(template == "blue5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)
    }
    # * red ------
    else if(template == "red1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)
    }
    else if(template == "red2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)
    }
    else if(template == "red3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkgoldenrod") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)
    }
    else if(template == "red4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)
    }
    else if(template == "red5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)
    }
    # * rainbow ---------
    else if(template == "rainbow1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)
    }
    else if(template == "rainbow2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)
    }
    else if(template == "rainbow3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "antiquewhite4") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)
    }
    else if(template == "rainbow4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)
    }
    else if(template == "rainbow5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)
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
        ggtitle(title)
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
        ggtitle(title)
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
        ggtitle(title)
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
        ggtitle(title)
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
        ggtitle(title)
    }
  }

  # with percentage ------------------------------------------------------
  else if(perc){
    # * basic ------------------
    if(template == "basic1"){
      ggplot(data_n, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start=0)+
        theme_void()+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))), size=5)+
        ggtitle(title)+
        scale_fill_discrete(name = group_name)
    }
    else if(template == "basic2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set3", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))), size=5)
    }
    else if(template == "basic3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Set2", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "white", family = "mono", fontface = "bold",size=5)
    }
    else if(template == "basic4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Paired", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold",size=5)
    }
    else if(template == "basic5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_linedraw()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Accent", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "sans", fontface = "bold",size=5)
    }
    # * bw ----------
    else if(template == "bw1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "white", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "bw2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "beige") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "beige", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "bw3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "yellow", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "bw4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "chartreuse", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "bw5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_grey(name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "deeppink", family = "mono", fontface = "bold", size=5)
    }
    # * blue ------
    else if(template == "blue1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "blue2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkorange", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "blue3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "yellow") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "antiquewhite4", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "blue4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "cyan4", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "blue5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkviolet") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Blues", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "deeppink", family = "mono", fontface = "bold", size=5)
    }
    # * red ------
    else if(template == "red1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "red2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "blue4", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "red3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "darkgoldenrod") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "red4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "aquamarine2", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "red5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Oranges", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkviolet", family = "mono", fontface = "bold", size=5)
    }
    # * rainbow ---------
    else if(template == "rainbow1"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "white") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "rainbow2"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "black") +
        coord_polar("y", start=0)+
        theme_classic()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkorange", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "rainbow3"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "antiquewhite4") +
        coord_polar("y", start=0)+
        theme_minimal()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "black", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "rainbow4"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "aquamarine") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkslategray", family = "mono", fontface = "bold", size=5)
    }
    else if(template == "rainbow5"){
      ggplot(data_n, aes(x="", y=value, fill=data_n[,1])) +
        geom_bar(stat="identity", width=1, color = "deeppink") +
        coord_polar("y", start=0)+
        theme_void()+
        ggtitle(title)+
        scale_fill_brewer(palette = "Spectral", name = group_name)+
        geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),
                      label = label_percent(accuracy = 1)(value/sum(value))),
                  color = "darkviolet", family = "mono", fontface = "bold", size=5)
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
        geom_text( x=3.5, aes(y=labelPosition,
                               label=label_percent(accuracy = 1)(value/sum(value))),
                   color = "black", family = "mono", fontface = "bold", size=5)
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
        geom_text( x=3.5, aes(y=labelPosition,
                              label=label_percent(accuracy = 1)(value/sum(value))),
                   color = "black", family = "mono", fontface = "bold", size=5)
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
        geom_text( x=3.5, aes(y=labelPosition,
                              label=label_percent(accuracy = 1)(value/sum(value))),
                   color = "blue4", family = "mono", fontface = "bold", size=5)
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
        geom_text( x=3.5, aes(y=labelPosition,
                              label=label_percent(accuracy = 1)(value/sum(value))),
                   color = "darkorange1", family = "mono", fontface = "bold", size=5)
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
        geom_text( x=3.5, aes(y=labelPosition,
                              label=label_percent(accuracy = 1)(value/sum(value))),
                   color = "white", family = "mono", fontface = "bold", size=5)
    }
  }
}
