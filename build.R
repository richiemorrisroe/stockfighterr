setwd("~/Dropbox/Code/Starfighter/stockfighterr")
devtools::setup(".", rstudio=FALSE)
devtools::use_build_ignore("build.R")
devtools::use_build_ignore("README.org")
devtools::use_build_ignore("cran-comments.md")
devtools::use_build_ignore("^#")
devtools::use_testthat()
devtools::document()
devtools::use_package("httr")
devtools::use_package("jsonlite")
devtools::check(".", cran=TRUE)
devtools::build(".")
devtools::install(".")
