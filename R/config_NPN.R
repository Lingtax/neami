#' Configure VPN settings
#' 
#' Walks a user through setting VPN parameters to use the `login_vpn()` function to connect to the Neami VPN. Credentials are held on system. 
#'
#' @param vpn The name of the VPN
#' @param password User Password
#'
#' @return
#' @export
#'
#' @examples
config_vpn <- function(vpn = NULL, password = NULL) {
  
  
  if(all(Sys.getenv("neami_vpn")  != "",
      Sys.getenv("neami_email") != "",
      Sys.getenv("neami_password") != "")) {
    
    if(menu(c("Nope", "Yes", "Nah"), 
            title = "There is an existing configuration set up, would you like to overwrite?") != 2) {
      return(cat("Configuration aborted."))
    }
  }
  
  if (is.null(vpn)) {
  
    vpn <- readline("What's the name of the VPN?")
    
  }
  
  if (is.null(password)) {
  
    password <- readline("What's your password?")
    password2 <- readline("Please confirm your password.")
    
    if(password != password2) {
      cat("Passwords do not match\n")
      cat("Restarting configuration\n")
      
      config_vpn(vpn = vpn)
    }
    
  }
  
  email <- paste0(Sys.getenv("USERNAME"), "@neaminational.org.au")
  
  path <- paste0(Sys.getenv("HOME"), "\\.Renviron")
  
  l1 <- paste0("neami_vpn=", vpn)
  l2 <- paste0("neami_email=",  email)
  l3 <- paste0("neami_password=", password)
  
  write(l1, path, append = TRUE)
  write(l2, path, append = TRUE)
  write(l3, path, append = TRUE)
  
  
cat("Configuration complete! Restart R and run `login_vpn()` to connect.")

    
  }
  
#' Login to Neami VPN
#' 
#' Provides a function to establish a connection with a VPN. Requires configuration using the `config_vpn()` function.
#'
#' @return NULL
#' @export
#'
#' @examples
#' login_vpn()
login_vpn <- function() {
  
  if(any(Sys.getenv("neami_vpn")  == "",
  Sys.getenv("neami_vpn") == "",
  Sys.getenv("neami_password") == "")) {
    return(cat("Run `config_vpn()` to configure first.")) } else {
  
  i <- system(paste("c:\\windows\\system32\\rasdial.exe", Sys.getenv("neami_vpn"), Sys.getenv("neami_email"), Sys.getenv("neami_password")))
    }
  
  if(i == 691) {cat("Password error; please re-run `config_vpn`")}
  if(i == 623) {cat("VPN name error; please re-run `config_vpn`")}
  if(i == 0) {cat("Login successful!")}
}   
  
  
  

