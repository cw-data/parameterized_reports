# this script runs the parameterized reports

library(tidyverse)
library(rstudioapi)
library(janitor)
library(reshape2)
library(scales)
library(lubridate)
library(gt)
library(cowplot)
library(ggrepel)
library(patchwork)
library(tidyquant)
library(timetk)
# tinytex::install_tinytex() # install tinytex() if you get an error: "No LaTeX installation detected"
# dir_script <- dirname(rstudioapi::getSourceEditorContext()$path) # extract the filepath in which this script and dataset are saved
# # setwd(dir_script) # set working directory so the script finds the dataset
data <- read.csv("cleandata.csv", header = TRUE) # load data, from https://www.kaggle.com/datasets/vivek468/superstore-dataset-final?resource=download
# 
# #------------ data cleaning #------------
working_data <- data # work on a copy of the data
# # change date columns to class:date
cols.to.date <- grep('date', names(working_data), ignore.case = TRUE) # find index of column(s) containing the word 'date'
for (i in 1:length(cols.to.date)){
  working_data[,cols.to.date[i]] <- as.Date(working_data[,cols.to.date[i]], format = '%Y-%m-%d') # change columns containing the word 'date' to Date format
}
# # change factor columns to class:factor
data.classes <- lapply(working_data, class) # find the class of all columns
# # if the column class == 'character' then that column should be changed to a factor
data.classes <- lapply(working_data, class) # extract class of all columns
data.classes <- melt(data.classes)
chr.to.fct <- subset(data.classes, value == 'character')$L1
working_data[chr.to.fct] <- lapply(working_data[chr.to.fct], as.factor) # https://stackoverflow.com/questions/35426903/change-class-of-multiple-columns-in-data-frame-without-for-loop
# glimpse(working_data)
# # In this dataset, integers should also be factors
int.to.fct <- subset(data.classes, L1 != 'Quantity') # leave Quantity as integer
int.to.fct <- subset(int.to.fct, value == 'integer')$L1 # change other integers to factors
working_data[int.to.fct] <- lapply(working_data[int.to.fct], as.factor) # https://stackoverflow.com/questions/35426903/change-class-of-multiple-columns-in-data-frame-without-for-loop
# 



library(tidyverse)
library(rstudioapi)

# establish which parameter we need to iterate the report over
reports <- tibble(
  class = unique(working_data$Region),
  filename = stringr::str_c(tolower(class)),
  params = purrr::map(as.character(class), ~list(x=.))
)
reports

# grab filepath that holds our the .Rmd file to run
#dir_script <- dirname(rstudioapi::getSourceEditorContext()$path)
dir_script <- getwd()
dir_script


# parameters for reports
x <- as.character(reports$class)
my_function <- function(x, output_dir_html, output_dir_pdf) {
  rmarkdown::render( # make the html intermediate
    paste0(dir_script, "//","region_reports.Rmd"), # filename of the rmd to render
    output_format = "pagedreport::paged_windmill", # this is one way to call a pagedreport pdf template
    output_dir = output_dir_html, # map a directory as the output location for the html intermediate step
    output_file = paste0(tolower(x), ".html"), # naming convention for html intermediate
    params = list(param1= x)
  )
  
  # to PDF
  pagedown::chrome_print(paste0(output_dir_html, "//", tolower(x), ".html"), # convert each html intermediate
                         output = paste0(output_dir_pdf, "//", "report_", tolower(x), ".pdf")) # into a pdf final copy
}

# make pdf parameterized pagedreports
walk(reports$params, my_function, dir_script, dir_script)
