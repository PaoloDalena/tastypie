#' @importFrom magrittr %>%
ex_groups_prop <- function(n){
  ex <- ex_groups(n) %>%
    arrange(desc(group)) %>%
    mutate(prop = value / sum(ex$value)*100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
}
