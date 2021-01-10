ex_groups <- function(n){
  if(n == 2){
    ex <- data.frame("group" = LETTERS[1:n], "value" = c(70, 30))
  }
  else if(n == 3){
    ex <- data.frame("group" = LETTERS[1:n], "value" = c(60, 25, 15))
  }
  else if(n == 4){
    ex <- data.frame("group" = LETTERS[1:n], "value" = c(40, 25, 20, 15))
  }
  else if(n == 5){
    ex <- data.frame("group" = LETTERS[1:n], "value" = c(30, 25, 20, 15, 10))
  }
  else{
    stop("The argument should be a number from 2 to 5.")
  }
}
