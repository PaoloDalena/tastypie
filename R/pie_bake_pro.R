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
      scale_fill_brewer(palette = "Set3", name = group_name)
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
      scale_fill_brewer(palette = "YlOrRd", name = group_name)
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
                color = "black", family = "mono", fontface = "bold", size=5)
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
      geom_text(aes(y = value+ 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = "black", family = "mono", fontface = "bold", size=5)
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
                color = "black", family = "mono", fontface = "bold", size=5)
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
      scale_fill_brewer(palette = "Set3", name = group_name)
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
      scale_fill_brewer(palette = "YlOrRd", name = group_name)
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
      geom_text(aes(y = value - 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = "black", family = "mono", fontface = "bold", size=5)
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
      scale_fill_brewer(palette = "Greys", name = group_name)
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
      geom_text(aes(y = value - 5,
                    label = scales::label_percent(accuracy = 1)(value/sum(value))),
                color = "black", family = "mono", fontface = "bold", size=5)
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
      scale_fill_brewer(palette = "Set3", name = group_name)
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
      scale_fill_brewer(palette = "YlOrRd", name = group_name)
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
      scale_fill_brewer(palette = "Greens", name = group_name)
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
      scale_fill_brewer(palette = "Greys", name = group_name)
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
      scale_fill_brewer(palette = "Blues", name = group_name)
  }

  # watermelon -----
  else if(template == "watermelon1"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "Set3", name = group_name)+
      ggtitle(title)
  }
  else if(template == "watermelon2"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "Spectral", name = group_name)+
      ggtitle(title)
  }
  else if(template == "watermelon3"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "orange")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "YlOrRd", name = group_name)+
      ggtitle(title)
  }
  else if(template == "watermelon4"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "white")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "BrBG", name = group_name)+
      ggtitle(title)
  }
  else if(template == "watermelon5"){
    ggplot(data_n, aes(fill = data_n[,1],x = factor(1), y = data_n[,2]))+
      geom_bar(width = 0.5, stat = "identity", color = "black")+
      coord_polar()+
      theme_void()+
      scale_fill_brewer(palette = "RdPu", name = group_name)+
      ggtitle(title)
  }
}
