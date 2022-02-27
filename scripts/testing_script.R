library(devtools)
library(ggplot2)
library(usethis)
library(stringr)
library(magrittr)
library(openxlsx)
library(testthat)
library(vdiffr)
library(tidyverse)
set.seed(123)

source(file.path("scripts","make_pairwise_plots.R"))
load_all()

vdiffr::manage_cases()

test()
