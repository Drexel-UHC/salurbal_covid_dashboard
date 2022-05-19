#' just some basic checks. Aggregate the byAge dataset and compare to original dataset. Note that the overall
#' numebrs will be slightly  lower beacause the byAge dataset only keeps rows where age is between 0-84.
#' 

{## 0. Setup -----
  library(tidyverse)
  library(data.table)
  library(plotly)
  
}


{## 1. Check cumulative numbers ----
  ## Original data
  df_cum_old = fread("processed-data/salurbal_AR_covid 5-18-22/Aggregated/SALURBAL_AR_COVID19_cumulative_cases_death.csv")
  
  ## Aggregate n
  df_cum_age = fread("processed-data/salurbal_AR_covid 5-18-22/Age-groups/SALURBAL_AR_COVID19_cumulative_cases_death_byAge.csv")
  df_cum_new = df_cum_age %>% 
    select( -country) %>% 
    group_by(level,salid) %>% 
    summarize(
      cases = sum(cases),
      deaths = sum(deaths),
      cases_per_1M = mean(cases_per_1M),
      deaths_per_10M = mean(deaths_per_10M)
    ) %>% 
    ungroup() %>% 
    mutate(salid = paste0(salid)) %>% 
    pivot_longer(cols = -c(salid, level), values_to  = 'value_new')
  
  ## Compare
  df_merge =  df_cum_old %>% 
    select( -country) %>% 
    pivot_longer(cols = -c(salid, level), values_to  = 'value_old') %>% 
    left_join(df_cum_new) %>% 
    filter(name%in%c("cases","deaths"))
  
  qc = df_merge%>% 
    ggplot(aes(value_old, value_new, label = salid))+
    geom_point()+
    facet_grid(vars(level), vars(name), scales = 'free')+
    geom_abline(slope = 1, intercept = 0)
  
  ggplotly(qc)
  
  
}


{## 2. Check Trends numbers ----
  ## Original data
  df_trends_old = fread("processed-data/salurbal_AR_covid 5-18-22/Aggregated/SALUBRAL_AR_COVID19_trends_cases_death.csv") 
  min(df_trends_old$date)
  max(df_trends_old$date)
  
  ## Aggregate n
  df_trends_age = fread("processed-data/salurbal_AR_covid 5-18-22/Age-groups/SALUBRAL_AR_COVID19_trends_cases_death_byAge.csv")
  df_trends_new = df_trends_age %>% 
    select( -country) %>% 
    group_by(level,salid,date) %>% 
    summarize(
      cases_raw = sum(cases_raw),
      deaths_raw = sum(deaths_raw),
      cases_per_1M = mean(cases_per_1M),
      deaths_per_10M = mean(deaths_per_10M)
    ) %>% 
    ungroup() %>% 
    mutate(salid = paste0(salid)) %>% 
    pivot_longer(cols = -c(salid,date, level), values_to  = 'value_new')
  min(df_trends_new$date)
  max(df_trends_new$date)
  ## Compare
  df_merge2 =  df_trends_old %>% 
    select( -country) %>% 
    pivot_longer(cols = -c(salid, date, level), values_to  = 'value_old') %>% 
    left_join(df_trends_new) %>% 
    filter(date<min( max(df_trends_old$date),  max(df_trends_new$date))) %>% 
    filter(name%in%c('cases_raw','deaths_raw')) %>% 
    rename(metric = name) %>%  
    pivot_longer(cols=c(value_old, value_new))

  df_merge2 %>% 
    filter(salid == "101112") %>% 
    ggplot(aes(x=date, y = value,  color = name, group = salid))+
    geom_line()+
    facet_wrap(~metric, scales = 'free')
  
  df_merge2 %>% 
    filter(salid == "101105") %>% 
    ggplot(aes(x=date, y = value,  color = name, group = salid))+
    geom_line()+
    facet_wrap(~metric, scales = 'free')
  ggplotly(qc)
}

{ ## 3. Check pop denoms ----
  
  
}