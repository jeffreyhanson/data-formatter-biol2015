## Load dependencies
library(zoo)
library(shiny)
library(shinyBS)
library(shinyTable)
library(data.table)
library(devtools)
library(plyr)
library(dplyr)

## Set global parameters
# dir params
main_DIR="data"
project_names_VCHR=c("Crabs", "Spiders", "Mangroves", "Vegetation")
group_colors_VCHR=c("blue","green", "red", "yellow")
week_numbers_VCHR=paste("Week", seq_len(2))

# color params
fixedColor="#dff0d8" # bootstrap success color
errorColor="#f2dede" # bootstrap error color
ignoreColor="#fcf8e3" # bootstrap warning color
highlightColor="#fbff00" # bright yellow

## Source code
source("classes.R")
source("functions.R")

## make id class
id=ID$new()

## load error generators
source("Crabs_errors.R")
source("Spiders_errors.R")
source("Mangroves_errors.R")
source("Boomajin_walk_errors.R")

