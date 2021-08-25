# 0. Setup  ------- 
{
  rm(list=ls()[ !str_detect(ls(), c("cpu_RL|cpu_UHC|df_update_status")) ])
  options(timeout=144000)
  source("code_salurbal_data_updater_util.R")
  
  ### Clear previous tmp_files
  file.remove(list.files("tmp_files/", full.names = T))
  file.remove("tmp_zipped_file.zip")
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
  ### Rename Manual data
  df_salurbal_manual = read.csv("SALURBAL_COVID19_sources.csv") %>% 
    as_tibble() %>%
    filter(!auto)
  dfOutputNames = tibble(fileName = list.files(path="./manual_files/")) %>% 
    mutate(outputName = case_when(
      str_detect(fileName,"MEXICO")~"mx_mun_cases_tmp.csv",
      str_detect(fileName,"Confirmados por municipio")~"gt_cases_tmp.csv",
      str_detect(fileName,"Tamizados por municipio")~"gt_tests_tmp.csv",
      str_detect(fileName,"Fallecidos por municipio")~"gt_deaths_tmp.csv"
    )) %>% drop_na() %>% 
    mutate_at(vars(fileName,outputName),~paste0("./manual_files/",.x))
  map2(dfOutputNames$fileName,dfOutputNames$outputName,~{ file.rename(.x,.y) })
  
  ### Copy all manual downloads to raw_tmp
  outputDirs = paste0("./raw_files/",list.files(path="./manual_files/"))
  map2(list.files(path="./manual_files/", full.names = T),
       outputDirs,
       ~{file.remove(.y)
         file.rename(.x,.y)} )
}

# 3. Status update  ------- 
{
  df_status_auto = df_auto %>% 
    mutate(date = Sys.Date(),
           status = status %>% recode("Download Okay"="Okay"))
  
  df_status_manual = read.csv("SALURBAL_COVID19_sources.csv") %>% 
    as_tibble() %>%
    filter(!auto) %>% 
    mutate(full_name = paste0("manual_files/",output_name),
           date = file.info(full_name) %>% pull(mtime)%>% as.Date(),
           status = "Manual") %>% 
    select(country, output = output_name,status, date)
  
  df_update_status = bind_rows(df_status_auto,df_status_manual) %>% 
    group_by(country, status) %>% 
    summarize(date = max(date)) %>% 
    ungroup() %>% 
    arrange(country) %>% 
    select(Country = country, Status = status, Date = date)
  
  df_update_status %>% write.csv("../Clean/status_log.csv")
}
