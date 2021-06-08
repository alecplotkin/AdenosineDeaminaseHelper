# check if packages are installed and load libraries
if (!require(readxl)) install.packages('readxl')
library(readxl)  # for reading excel files
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)  # because tidyverse

# source custom functions
source('functions.R')
