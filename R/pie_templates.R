pie_templates <- function(template, n_groups = 4, perc = FALSE){
  if(!template %in% c(pie_template_list, pie_template_list_pro)){
    stop("\n The selected template does NOT exist.
 Type pie_template_list and pie_template_list_pro to see all the available templates.")
  }
  else if(template %in% pie_template_list){
    if(!perc){
      pie_bake(ex_groups(n_groups), template = template,
               title = paste(
                 "pie_bake( ... , template = '",template, "')", sep = "")
               )
    }
    else if(perc){
      pie_bake(ex_groups(n_groups), template = template, perc = TRUE,
               title = paste(
                 "pie_bake( ... , template = '",template, "', perc = TRUE)", sep = "")
               )
    }
  }
  else if(template %in% pie_template_list_pro){
    pie_bake_pro(ex_groups(n_groups), template = template,
             title = paste(
               "pie_bake_pro( ... , template = '",template, "')", sep = "")
             )
  }
}
