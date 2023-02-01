
library(shinythemes)
library(shiny.info)
library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(glue)
library(bslib)
library(shinyWidgets)
library(scales)
library(ggplot2)
library(markdown)
library(shinyalert)
library(shinybrowser)
library(feather)

sim_results <- read_rds("sim_results.RDS")


source("plot_results.R")
source("fun_get_results.R")

new_order <- c("#E6CED4","#76908E","#F1FAFE","#C8B7E4")

choices <- levels(sim_results$p1_hdc)

choices <- str_replace(choices, "([+])([1-9])","\\1 \\2") ##\\1 means first element matched 

my_theme <- bs_theme(bootswatch = "flatly", 
                     bg = "#fff", 
                     fg = "#576B6A",
                     primary = "#576B6A",
                     secondary = "#576B6A",
                     success = "#89B6AD",
                     info = "#576B6A",
                     spacer = "2rem")
