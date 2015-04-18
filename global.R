options(stringsAsFactors=TRUE, shiny.error=traceback)

## Load dependencies
library(zoo)
library(shiny)
library(shinyBS)
library(shinyTable)
library(data.table)
library(devtools)
library(plyr)
library(dplyr)
library(testthat)

## Set global parameters
# dir params
main_DIR="data"
project_names_VCHR=c(crabs="Crabs", spiders="Spiders", `mangrove herbivory`="Mangroves_Herbivory", `mangrove community`="Mangroves_Community", `vegetation assessment`="Vegetation_Assessment")
group_colors_VCHR=c(blue="blue",green="green", red="red", yellow="yellow")
week_numbers_VCHR=paste("Week", seq_len(2))
names(week_numbers_VCHR)=tolower(week_numbers_VCHR)

# color params
fixedColor="#dff0d8" # bootstrap success color
errorColor="#f2dede" # bootstrap error color
ignoreColor="#fcf8e3" # bootstrap warning color
highlightColor="#fbff00" # bright yellow

## Source code
source("classes.R")
source("functions.R")
source("list.R")
source("datatable.R")

## make id class
id=ID$new()

## load error generators
source("projects/Crabs.R")
source("projects/Spiders.R")
source("projects/Mangroves_Herbivory.R")
source("projects/Mangroves_Community.R")
source("projects/Vegetation_Assessment.R")

