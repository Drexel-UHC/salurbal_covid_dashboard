rm(list=ls())
options(timeout=14400)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
library(zip)
source("code_salurbal_data_updater_util.R")

#### ********************************************* ####
# 1. Download files  ------- 
## Guatemala
# "Confirmados po municipio fecha de emision "
# url_cases = "https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_3fa75ca6/session/2977ea30e7dbb4a6d6cf95b489572163/download/confirmadosFER?w=3fa75ca6"
# download.file(url_cases,destfile = "../../SALURBAL Covid19 Files/tmp_files/gt_cases_tmp.csv")
# "tamizados  por municipio"
# url_deaths = "https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_3fa75ca6/session/2977ea30e7dbb4a6d6cf95b489572163/download/tamizadosFER?w=3fa75ca6"
# download.file(url_deaths,destfile = "../../SALURBAL Covid19 Files/tmp_files/gt_tests_tmp.csv")
# "Fallecidos por municipio"
# url_deaths = "https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_3fa75ca6/session/2977ea30e7dbb4a6d6cf95b489572163/download/fallecidosFF?w=3fa75ca6"
# download.file(url_deaths,destfile = "../../SALURBAL Covid19 Files/tmp_files/gt_deaths_tmp.csv")

## Brazil
url_br = "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv.gz"
download.file(url_br,destfile = "../../SALURBAL Covid19 Files/tmp_files/cases-brazil-cities-time.csv.gz")
url_br_state = "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv"
download.file(url_br_state, destfile = "../../SALURBAL Covid19 Files/tmp_files/brazil_state_tmp.csv")
## Mexico 
url_mx_cases =  "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
download.file(url_mx_cases,"mexico_tmp.zip",quiet=F,mode="wb",timeout=79200)
file.remove(list.files("mexico_folder_tmp", full.names = T))
zip::unzip(zipfile = "mexico_tmp.zip",exdir = "mexico_folder_tmp", overwrite = T)
unzipped_file = list.files(path = "mexico_folder_tmp", full.names = T)
dfa = data.table::fread( unzipped_file  ) 
dfa %>% 
  fwrite("../../SALURBAL Covid19 Files/tmp_files/mx_mun_cases_tmp.csv")
file.remove("mexico_tmp.zip")
## Chile
chile_raw_counts_url  = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv"
download.file(chile_raw_counts_url,destfile = "../../SALURBAL Covid19 Files/tmp_files/chile_raw_counts_tmp.csv")
chile_raw_tests_url = "https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto55/Positividad_por_comuna.csv"
download.file(chile_raw_tests_url,destfile = "../../SALURBAL Covid19 Files/tmp_files/chile_raw_tests_tmp.csv")
chile_raw_deaths_url  = "https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto38/CasosFallecidosPorComuna_std.csv"
download.file(chile_raw_deaths_url,destfile = "../../SALURBAL Covid19 Files/tmp_files/chile_raw_deaths_tmp.csv")
## Colombia
colombia_url ="https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD"
download.file(colombia_url,destfile = "../../SALURBAL Covid19 Files/tmp_files/colombia_tmp.csv")
## Peru
url_peru_cases = "https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download"
download.file(url_peru_cases,destfile = "../../SALURBAL Covid19 Files/tmp_files/url_peru_cases_tmp.csv")
url_peru_deaths = "https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download"
download.file(url_peru_deaths,destfile = "../../SALURBAL Covid19 Files/tmp_files/url_peru_deaths_tmp.csv")


# ## Argentina (Manual Download on Work PC)
# url_benosares = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv"
# download.file(url_benosares, destfile = "../../SALURBAL Covid19 Files/tmp_files/argentina_BA_tmp.csv")
# url_ar_testing = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Determinaciones.csv"
# download.file(url_ar_testing, destfile = "../../SALURBAL Covid19 Files/tmp_files/argentina_testing_tmp.csv")
# url_ar_micro = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv"
# download.file(url_ar_micro, destfile = "../../SALURBAL Covid19 Files/tmp_files/argentina_tmp.csv")


#### ********************************************* ####
##### 2.zip files to server #####

library(zip)
files_to_zip = list.files(path  = "../../SALURBAL Covid19 Files/tmp_files/", full.names = T)
zip::zip(zipfile = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Bilal_DP5/Data/SALURBAL_dp5_server/covid_data/salurbal_covid.zip",
         files_to_zip,
         mode = "cherry-pick")

