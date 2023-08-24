.onAttach <- function(libname, pkgname){
  packageStartupMessage(sample(c("Black Lives Matter", "Trans rights are Human Rights", "Always was, Always will be Aboriginal Land", "No human is illegal", "Believe women", "Silence in the face of injustice is complicity with the oppressor."), 1))
  extrafont::loadfonts()
  sysfonts::font_add_google("Lexend", "lexend")
  showtext::showtext_auto()
}
