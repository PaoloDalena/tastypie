#' Check if the data is good for making tasty pies
#'
#' \code{pie_datacheck} checks if the provided data are suitable for creating pie charts
#' using the useful functions in the \code{tastypie} package.
#'
#' @param data the data that you want to use for creating pie charts
#' @param check logical, set equal to TRUE if you need a message to know if there are no problems
#'
#' @return If the provided data are a dataframe with only two variables (columns) with
#' the vector of labels in the first one and the vector of values in the second one, nothing
#' will happen. Otherwise, an error that tells you what's wrong occurs.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' wrong <- c(1, 2, 3)
#' pie_datacheck(wrong) # Error
#'
#' wrong2 <- data.frame("a" = c(1, 2, 3), "b" = c("ex", "am", "ple"))
#' pie_datacheck(wrong2) # Error
#'
#' right <- data.frame("a" = c("ex", "am", "ple"), "b" = c(1, 2, 3))
#' pie_datacheck(right) # No Error ==> OK!
#' pie_datacheck(right, check = T) # Positive message}
#'

pie_datacheck <- function(data, check = FALSE){
  if(!is.data.frame(data)){
    stop("You have to provide a dataframe with two variables (columns):
         - in the first one there must be the vector of labels
         - in the second one there must be the vector of values.")
  }
  if(is.null(dim(data)[1])){
    stop("You have to provide a bidimensional dataframe!")
  }
  if(dim(data)[2] != 2){
    stop("You have to provide a dataframe with only two variables (columns):
         - in the first one there must be the vector of labels
         - in the second one there must be the vector of values.")
  }
  if(!is.numeric(data[,2])){
    stop("In the second variable (column) of the dataframe there must be numerical values!")
  }
  if(check){
    message("Your dataframe is ready for making tasty pies :)")
  }
  if(dim(data)[1] > 8){
    warning("Maybe the number of groups is too high for an understable pie chart.")
  }
}
