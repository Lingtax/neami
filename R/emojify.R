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
  
   if(!is.logical(x) & type == "TF" ) stop("emojify is expecting a logical variable")
   if(is.logical(x) & type == "TL" ) stop("emojify is expecting a character variable with levels 'red', 'yellow', and 'green'")
  
  
  if(type == "TF") {
    x <- ifelse(x, "y", "n") 
    index = c(y = "✔", n = "❌")
    
    }
  
  if(type == "TL") {
    index = c(green = "🟢", yellow = "🟡", red = "🔴")
    
    }
  
  
  
  index[x]
  
  
  
}


