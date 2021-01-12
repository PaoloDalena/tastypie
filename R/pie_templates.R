pie_templates <- function(template, n_groups = 4, perc = FALSE){
  if(!perc){
    pie_bake(ex_groups(n_groups), template = template, title = paste("template = ",template))
  }
  else if(perc){
    pie_bake(ex_groups(n_groups), template = template, perc = TRUE,
             title = paste("template = ",template, ", perc = TRUE", sep = ""))
  }
}
