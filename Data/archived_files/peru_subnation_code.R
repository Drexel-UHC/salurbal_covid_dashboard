library(tidyverse)
library(readxl)
library(janitor)
library(httr)

## Raw Data 
github_link = "https://github.com/jincio/COVID_19_PERU/blob/master/data/descargas_sala_situacional/CASOS_23052020.xlsx?raw=true"
temp_file <- tempfile(fileext = ".xlsx")
req <- GET(github_link,  write_disk(path = temp_file))
df <- readxl::read_excel(temp_file) %>% 
  clean_names()
