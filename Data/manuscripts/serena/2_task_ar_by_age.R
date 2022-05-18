
  ## 0. Setup -----
  {
    library(stringr)
    rm(list = ls())
    cpu_RL = (str_detect(getwd(),"ranli"))
    cpu_UHC = !cpu_RL
    if (cpu_UHC) { 
      setwd("C:/Users/rl627/Desktop/Git/SALURBAL COVID19 Dashbaord (Private)/Data") 
    } else { setwd("C:/Users/ranli/Desktop/Git local/SALURBAL COVID-19 Dashboard/Data") }
    load("manuscripts/serena/processed-data/pop_ar_by_age.rdata")
  }

  
  
  ## 1. Clean data -----
  {
    task1 = try ({ source("code_salurbal_data_updater_AR.R")}) %>% as.character()
  }
  
  
  