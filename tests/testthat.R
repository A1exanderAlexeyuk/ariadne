# Sys.setenv("R_TESTS" = "")
# options(fftempdir = file.path(getwd(),'fftemp'))
library(testthat)
library(ariadne)
test_check("ariadne")
unlink("C:/Temp", recursive = T)
