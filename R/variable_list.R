#' Variable List
#'
#' @param x A data frame
#'
#' @return Gives variable number, name, first observation, and the variable's class and returns a dataframe
#' @export
#'
#' @examples variable_list(mtcars)

variable_list <- function(x) {
  data <- x[1,]
  data <- t(data)
  vars <- row.names(data)
  num <- 1:length(vars)
  data <- as.data.frame(cbind(num,vars,data))
  names(data) <- c("variable_index_position", "variable_name", "first_observation")
  row.names(data) <- NULL
  class <- sapply(x, class)
  data$class <- class
  data <- data[,c(2,1,4,3)]

  return(data)
}