pie_discover <- function(
  which = c("all", "bake", "pro")
){

  if(missing(which)){
    which <- "all"
  }

  if(which == "all"){
    templ <- sample(c(pie_template_list, pie_template_list_pro), 1)
  }
  else if(which == "bake"){
    templ <- sample(pie_template_list, 1)
  }
  else if(which == "pro"){
    templ <- sample(pie_template_list_pro, 1)
  }

  n <- sample(2:5, 1)
  pe <- sample(0:1, 1)
  pie_templates(template = templ, n_groups = n, perc = pe)
}
