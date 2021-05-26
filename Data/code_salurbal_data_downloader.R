# 0. Setup  ------- 
{
  rm(list=ls())
  options(timeout=144000)
  source("code_salurbal_data_updater_util.R")
  
  ### Clear previous tmp_files
  file.remove(list.files("tmp_files/", full.names = T))
}



# 1. Auto Download  ------- 
{
  df_salurbal_covid_links = read.csv("SALURBAL_COVID19_sources.csv") %>% 
    as_tibble() %>%
    filter(auto)
  df_auto = pmap_df(list(df_salurbal_covid_links$country,
                         df_salurbal_covid_links$url,
                         df_salurbal_covid_links$output_name),
                    function(a,b,c){salurbal_download(a,b,c)})
}

# 2. Manual Download  ------- 
{
  # ## Guatemala
  # "Confirmados po municipio fecha de emision "
  # url_cases = "https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_7fc790c8/session/2de634f0e89f573e133a232052f96c78/download/confirmadosFER?w=7fc790c8"
  # download.file(url_cases,destfile = "raw_files/gt_cases_tmp.csv")
  # "tamizados  por municipio"
  # url_deaths ="https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_7fc790c8/session/2de634f0e89f573e133a232052f96c78/download/tamizadosFER?w=7fc790c8"
  # download.file(url_deaths,destfile = "raw_files/gt_tests_tmp.csv")
  # "Fallecidos por municipio"
  # url_deaths = "https://gtmvigilanciacovid.shinyapps.io/3869aac0fb95d6baf2c80f19f2da5f98/_w_7fc790c8/session/2de634f0e89f573e133a232052f96c78/download/fallecidosFF?w=7fc790c8"
  # download.file(url_deaths,destfile = "raw_files/gt_deaths_tmp.csv")
}

