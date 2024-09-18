#' Converts content to emoji
#'
#' @param x a logical vector  
#' @param type a parameter that will allow for alternative codings. 
#'
#' @return a vector of emoji
#' @export
#'
#' @examples
#' emojify(c(TRUE, FALSE, TRUE))
emojify <- function(x, type = "TF") {
  
  if(!is.logical(x)) stop("emojify currently only supports logical variables")
  
  x <- ifelse(x, "y", "n") 
  if(type == "TF") {index = c(y = "✔", n = "❌")}
  
  index[x]
}


