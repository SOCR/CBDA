# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

local({r <- getOption("repos")
r["CRAN"] <- "<https://cran.mtu.edu/>" 
options(repos=r)
})

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
   # install.packages(new.pkg, dependencies = TRUE)
   install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}


