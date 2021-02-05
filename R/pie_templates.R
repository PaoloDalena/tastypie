#' Display an example of a particular template
#'
#' This function allows to user to display a pie chart by selecting the template,
#' the number of groups of interest and whether to display the proportions or not,
#' in order to make it easier to choose between the many templates available in the
#' package \code{tastypie}.
#'
#' @param template The chosen template.
#' @param n_groups A number from 2 to 9.
#' @param perc A logical value. Should the proportions be displayed?\cr
#' Note that if the selected template is one of those to be used with
#' \code{\link{pie_bake_pro}} (listed in the \code{pie_template_list_pro} vector),
#' this argument is useless.
#'
#' @return
#' A ggplot object.
#' In particular, this function returns a pie (or similar) chart according to the
#' choice of template and the other specifications provided.
#'
#' @seealso
#' See all the available templates displayed
#' \href{https://paolodalena.github.io/tastypie/articles/available_templates.html}{here}!
#'
#' @examples
#' pie_templates(template = "bw1", n_groups = 3, perc = TRUE)
#' pie_templates(template = "watermelon2", n_groups = 8)
#' @export
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
