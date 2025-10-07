rm(list = ls())

library(targets)
library(tarchetypes)
library(tidyverse)

tar_make() #run all the code and produce the results

tar_visnetwork() # visualise the pieline of the ranking process


#---------------------

#' To Dos
#' 1) Simple shiny app within targets framework for running through MCMC chains for each of the variables and checking convergence was sensible

#--------------

#To inspect the final rankings

rankings <- tar_read()



 