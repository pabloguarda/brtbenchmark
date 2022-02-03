# a) Libraries --------------------------------------------------------------------

options(install.packages.compile.from.source = "never")

# - List of libraries ------------------------------------------------------------------

libraries_tools <- c("tidyverse", 'Hmisc', 'visreg','XML')
libraries_tables <- c('stargazer', 'texreg', 'apsrtable', 'xtable','markdown' ) 
libraries_graphics <- c('ggplot2','ggrepel',"ggjoy","ggthemes")
libraries <- c(libraries_tools, libraries_tables, libraries_graphics)

# - Loading/installing ---------------------------------------------------------------

for(i in 1:length(libraries)){
  if(!require(package = libraries[i],character.only = TRUE)){ 
    install.packages(libraries[i],character.only = TRUE, dependencies = TRUE)
  }
  library(libraries[i],character.only = TRUE)
}
