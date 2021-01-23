#' Discover templates for \code{tastypie}
#'
#' This function allows to user to find out a random combination of templates,
#' number of groups and features in order to get an idea of the many available
#' plots in the package \code{tastypie}.
#'
#' @param which Allows to select a subset of the available templates. \cr
#' \cr
#' If \code{"all"}, the template is randomly chosen among all the available ones. \cr
#' If \code{"bake"}, the template is randomly chosen among the ones that can be used
#' through \code{\link{pie_bake}}.\cr
#' If \code{"pro"}, the template is randomly chosen among the (more complex) ones
#' that can be used through \code{\link{pie_bake_pro}}.\cr
#' \cr
#' The default value is \code{"all"}.
#'
#' @seealso
#' See all the available templates displayed
#' \href{https://paolodalena.github.io/tastypie/articles/available_templates.html}{here}!
#'
#' @examples
#' pie_discover()
#' pie_discover("pro")
#' @export
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

  n <- sample(2:9, 1)
  pe <- sample(0:1, 1)
  pie_templates(template = templ, n_groups = n, perc = pe)
}
