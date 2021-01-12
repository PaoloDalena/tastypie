pie_discover <- function(){
  templ <- sample(pie_template_list, 1)
  n <- sample(2:5, 1)
  pe <- sample(0:1, 1)
  pie_templates(template = templ, n_groups = n, perc = pe)
}
