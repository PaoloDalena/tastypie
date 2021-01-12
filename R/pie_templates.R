pie_templates <- function(template, n_groups = 4, perc = FALSE){
  if(!template %in% pie_template_list){
    stop("\n The selected template does NOT exist. \n Type pie_template_list to see all the available templates.")
  }
  if(!perc){
    pie_bake(ex_groups(n_groups), template = template, title = paste("template = ",template))
  }
  else if(perc){
    pie_bake(ex_groups(n_groups), template = template, perc = TRUE,
             title = paste("template = ",template, ", perc = TRUE", sep = ""))
  }
}
