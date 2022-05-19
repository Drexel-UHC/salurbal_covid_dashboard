# 0. Setup -----
{
  
  source("code_salurbal_data_updater_util.R")
  cutoff_date = "05-25-2021" 
  
  ## Subset to every 3 days 
  n_days = 7
  
  ### Global Variables
  xwalk_countries = tibble(country =c("Argentina",
                                      "Brazil",
                                      "Chile",
                                      "Colombia",
                                      "Guatemala",
                                      "Mexico",
                                      "Peru")) %>% 
    mutate(iso2 = country %>% 
             recode("Argentina"="AR",
                    "Brazil"="BR",
                    "Chile"="CL",
                    "Colombia"="CO",
                    "Guatemala"="GT",
                    "Mexico"="MX",
                    "Peru"="PE"))
  
  ## By Age denoms
  load("manuscripts/serena/processed-data/pop_ar_by_age.rdata")
  pop_df_ar_age = df_pop_l1_ar %>% 
    mutate(level = "L1") %>% 
    select(level, age_group, loc = salid1, pop) %>% 
    bind_rows(
      df_pop_l2_ar %>% 
        mutate(level = "L2") %>% 
        select(level, age_group,loc = salid2, pop)
    ) %>% 
    mutate(loc = as.character(loc))
  save(pop_df_ar_age,file = 'manuscripts/serena/processed-data/pop_df_ar_age.rdata')
  save(pop_df,file = 'manuscripts/serena/processed-data/pop_df.rdata')
}


# 9. Argentina  ####
try_AR = try({
  {#___9.1  Get raw data  #####
    ## Raw BA Data
    ar_micro_BA_raw = fread("raw_files/casos_covid19.csv") %>%
      as_tibble() %>%
      filter(provincia== "CABA") %>%
      filter(clasificacion=="confirmado") %>%
      filter(!is.na(comuna)) %>%
      select(comuna,
             edad,
             date = fecha_apertura_snvs,
             death=fallecido) %>%
      mutate(death = case_when(death == "si"~"SI",
                               is.na(death)~"NO",
                               TRUE~"NO")) %>%
      mutate(date = date %>% str_sub(1,9) %>% dmy()) %>%
      mutate(salid2_name = paste("Comuna",comuna)) %>%
      left_join(xwalk_sal_ar %>% select(salid2_name, mun)) %>%
      select(mun, date,edad, death)
    
    
    ## Raw Agertina Data
    ar_micro_raw = fread("raw_files/Covid19Casos.csv") %>%
      # filter(residencia_provincia_nombre!="CABA") %>%
      as_tibble() %>%
      select(prov = residencia_provincia_id,
             dept = residencia_departamento_id,
             prov_name = residencia_provincia_nombre,
             dept_name = residencia_departamento_nombre,
             # date = fecha_apertura   , # fecha_apertura
             date = fecha_diagnostico   , # fecha_diagnostico
             type = clasificacion_resumen,
             edad,
             death = fallecido) %>%
      mutate(prov = str_pad(prov,2, "left","0"),
             dept = str_pad(dept,3, "left","0"),
             mun = paste0(prov, dept),
             date = ymd(date)) %>%
      filter(type == "Confirmado") %>%
      filter(!mun%in%ar_micro_BA_raw$mun) %>%
      bind_rows(ar_micro_BA_raw) %>%
      filter(!is.na(date),
             between(edad,0,84))%>% 
      mutate( 
        age_group = case_when(
          edad<=9~'0-9',
          edad<=19~'10-19',
          edad<=29~'20-29',
          edad<=39~'30-39',
          edad<=49~'40-49',
          edad<=59~'50-59',
          edad<=69~'60-69',
          edad<=79~'70-79',
          edad<=84~'80-84',
          TRUE~NA_character_
        ) 
      
      )
    
    ## Raw Cases (ar_mun_cases)
    ar_mun_cases_raw = ar_micro_raw %>%
      select(mun,age_group, date) %>%
      count(mun, date, age_group,name = "confirmed")
    min_date =  mdy("03-15-2020")
    max_date = max(ar_mun_cases_raw$date)
    ar_mun_cases = tibble(mun= list(unique(ar_mun_cases_raw$mun)),
                          age_group = list(unique(ar_mun_cases_raw$age_group)),
                          date = seq(min_date,max_date, by = "day")) %>%
      unnest( cols = 'mun') %>%
      unnest( cols = 'age_group') %>%
      left_join(ar_mun_cases_raw) %>%
      mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>%
      group_by(mun, age_group) %>%
      group_modify(~.x %>%
                     arrange(date) %>%
                     mutate(confirmed = cumsum(confirmed))) %>%
      ungroup() %>%
      arrange(mun, age_group, date)
    
    ## Raw Deaths (ar_mun_deaths)
    ar_mun_deaths_raw = ar_micro_raw %>%
      filter(death == "SI") %>%
      select(mun, age_group, date) %>%
      count(mun, date, age_group, name = "deaths")
    ar_mun_deaths = tibble(mun= list(unique(ar_mun_cases_raw$mun)),
                           age_group = list(unique(ar_mun_cases_raw$age_group)),
                           date = seq(min_date,max_date, by = "day")) %>%
      unnest( cols = 'mun') %>%
      unnest( cols = 'age_group') %>%
      left_join(ar_mun_deaths_raw) %>%
      mutate(deaths = ifelse(is.na(deaths),0,deaths)) %>%
      group_by(mun, age_group) %>%
      group_modify(~.x %>%
                     arrange(date) %>%
                     mutate(deaths = cumsum(deaths))) %>%
      ungroup() %>%
      arrange(mun, age_group,date)
    
    
    ## Final Processed File (ar_mun_file)
    ar_mun_file_raw  = left_join(ar_mun_cases,ar_mun_deaths)%>%
      arrange(mun, date) 
    min_date_tmp = mdy("03-15-2020")
    max_date_tmp = max(ar_mun_file_raw$date)
    ar_mun_file = tibble(mun = list(unique(ar_mun_file_raw$mun)),
                         age_group = list(unique(ar_mun_cases_raw$age_group)),
                         date = seq(min_date_tmp,max_date_tmp, by = 'day') ) %>%
      unnest( cols = 'mun') %>%
      unnest( cols = 'age_group') %>%
      arrange(mun, date) %>%
      left_join(ar_mun_file_raw, by = c('mun','age_group',"date")) %>%
      mutate(country = "Argentina")}
  
  {#___9.2 -  Cumulative ####
    full_ar_l1 = ar_mun_file  %>%
      mutate(grouper = age_group) %>% 
      group_by(grouper) %>% 
      group_modify(~.x %>%  
                     left_join(xwalk_sal_ar) %>%
                     mutate(level = "L1") %>%
                     group_by(date,age_group, level,salid1, salid1_name, country) %>%
                     summarise(confirmed = sum(confirmed),
                               deaths  = sum(deaths)) %>%
                     ungroup() %>%
                     arrange(salid1_name,date) %>%
                     mutate(salid1_name = ifelse(is.na(salid1),
                                                 "Argentina Non-salurbal",
                                                 salid1_name),
                            salid1 = ifelse(is.na(salid1),
                                            paste0("AR","888"),
                                            salid1)) %>%
                     left_join(
                       # pop_df %>% filter(level == "L1") %>% select(-level)
                       pop_df_ar_age %>% filter(level == "L1") %>% select(-level),
                       by = c("salid1"="loc", 'age_group')) %>%
                     mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
                            deaths_rate = round((deaths/pop)*10^7,2))   %>%
                     rename(loc = salid1_name) %>%
                     mutate(country = "Argentina")%>%
                     arrange(salid1, date)
      ) %>% 
      ungroup() %>% 
      select(-grouper)
    
    
    full_ar_l2 = ar_mun_file  %>%
      mutate(grouper = age_group) %>% 
      group_by(grouper) %>% 
      group_modify(~.x %>% 
                     left_join(xwalk_sal_ar) %>%
                     mutate(level = "L2") %>%
                     group_by(date, age_group, level,salid2, salid2_name, country) %>%
                     summarise(confirmed = sum(confirmed),
                               deaths = sum(deaths)) %>%
                     ungroup() %>%
                     arrange(salid2_name,date) %>%
                     mutate(salid2_name = ifelse(is.na(salid2),
                                                 "Argentina Non-salurbal",
                                                 salid2_name),
                            salid2 = ifelse(is.na(salid2),
                                            paste0("AR","888"),
                                            salid2)) %>%
                     left_join(
                       # pop_df %>% filter(level == "L2") %>% select(-level),
                       pop_df_ar_age %>% filter(level == "L2") %>% select(-level),
                       by = c("salid2"="loc", 'age_group')) %>%
                     mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
                            deaths_rate = round((deaths/pop)*10^7,2))   %>%
                     rename(loc = salid2_name) %>%
                     mutate(country = "Argentina")%>%
                     arrange(salid2, date)) %>% 
      ungroup() %>% 
      select(-grouper)
    }
  
  
  
  { #___9.3 -  Daily ####
    tidy.daily.ar.l1 = full_ar_l1 %>% 
      group_by(age_group) %>% 
      group_modify(~.x %>% 
                     clean_daily_salurbal_smooth() %>%
                     ungroup()%>%
                     mutate(level = "L1")) %>% 
      ungroup()
    tidy.daily.ar.l2 = full_ar_l2 %>% 
      group_by(age_group) %>% 
      group_modify(~.x %>% 
                     clean_daily_salurbal_smooth()%>%
                     ungroup()%>% 
                     mutate(level = "L2")) %>% 
      ungroup()}
  
  
})


# 10. Compile   ####
try_compile = try({
  #___10.1 -  full ####
  full_global =  list(
    full_ar_l1,
    full_ar_l2
  ) %>% 
    bind_rows() %>% 
    pivot_longer(cols = c(deaths,confirmed,"confirmed_rate","deaths_rate"), 
                 names_to = "rate", values_to = "n") %>% 
    filter(!is.na(n)) %>% 
    mutate(title = case_when(
      level == "country"~"in the LAC Region",
      level == "state" ~paste0("in ",country, " (State)"),
      level == "L1" ~paste0("in ",country, " (Cities)"),
      level == "L2" ~paste0("in ",country, " (Sub-Cities)"))) %>% 
    mutate(type = ifelse(str_detect(rate,"deaths"),"deaths","cases")) %>% 
    mutate(rate = ifelse(str_detect(rate,"rate"),"rate","count"))
  
  
})

#  11. Saving Rdata (covid19_processed_data.rdata) ####
{
  {# ___11.1 -  tidy.daily  ####
    tidy.daily.subnational = list( tidy.daily.ar.l1,
                                   tidy.daily.ar.l2) %>% 
      bind_rows() %>% 
      filter(smooth_days == 7)
    
    
    tidy.daily = tidy.daily.subnational %>% 
      select(level,country, type,age_group, loc, salid,date, rollsum, rate) %>% 
      pivot_longer(cols = c(rollsum,rate),names_to = "rate") %>% 
      mutate(value = ifelse(type == "deaths"&rate == "rate",value*10, value)) %>%
      filter(!(type == "positivity"&rate == "rollsum")) %>% 
      mutate(ylabs = case_when(type =="confirmed"&rate=="rollsum"~"New Cases",
                               type =="confirmed"&rate=="rate"~"New Cases per 1M",
                               type =="deaths"&rate=="rollsum"~"New Deaths",
                               type =="deaths"&rate=="rate"~"New Deaths per 10M",
                               type =="tests"&rate=="rollsum"~"Daily Tests",
                               type =="tests"&rate=="rate"~"New Tests per 1M",
                               type =="positivity"&rate=="rate"~"New Testing Positivity")) %>% 
      mutate(title = case_when(
        level =="country"~paste("Daily",ylabs,"in the LAC Region" ),
        level == "state"~paste("Daily",ylabs,"in States of",country),
        level == "L1"~paste("Daily",ylabs,"in Cities of",country),
        level == "L2"~paste("Daily",ylabs,"in Sub-Cities of",country)
      )) %>% 
      mutate(rate = rate %>% recode("rollsum"="count"))  %>% 
      mutate(rate_cleaned = case_when(
        type =="confirmed"&rate=="rate"~"count per 1 M",
        type =="deaths"&rate=="rate"~"count per 10 M",
        type =="tests"&rate=="rate"~"count per 1M",
        type =="positivity"&rate=="rate"~"% of Tests Positive",
        TRUE~rate
      )) %>% 
      filter(!is.na(value)) %>% 
      mutate(type = ifelse(type =="confirmed","cases",type)) %>% 
      mutate(value = ifelse(value<0,0,value))
    
    
    
    tidy.daily.data = tidy.daily %>% 
      select(level, country, age_group, type, rate, salid, loc, date, value) %>% 
      filter(level%in%c("L1","L2"))}
  
  
  
  {# ___11.2 - tidy.cumulative   ####
    dfa = tidy.daily.subnational %>% 
      select(level,country, type, age_group, loc, salid,date, daily_counts) %>% 
      mutate(type = type %>% recode("confirmed"="cases")) %>% 
      group_by(level, country, age_group, loc, salid,type) %>% 
      group_modify(~.x %>% 
                     arrange(date) %>% 
                     mutate(cum_value = cumsum(daily_counts) ) ) %>% 
      ungroup() %>% 
      arrange(level, country, loc, salid,type,date)
    
    df2_cases_tests_deaths = dfa %>%   
      filter(type!="positivity") %>% 
      mutate(loc2 = ifelse(!level%in%c("L1","L2"),
                           loc,
                           salid)) %>% 
      left_join(
        pop_df_ar_age %>% rename(loc2 = loc)
      ) %>% 
      filter(!is.na(pop)) %>% 
      select(-loc2, -daily_counts) %>% 
      mutate(rate = case_when(
        type%in%c("cases", "tests")~(cum_value/pop)*10^6,
        type%in%c("deaths")~(cum_value/pop)*10^7
      )) %>% 
      select(-pop)
    
    
    df2 = bind_rows(df2_cases_tests_deaths )
    
    
    
    tidy.cumulative = df2%>% 
      pivot_longer(cols = c(cum_value, rate), 
                   names_to = "rate",
                   values_to = "n") %>% 
      filter(!(type == "positivity"& rate == "cum_value")) %>% 
      mutate(rate = rate %>% recode("cum_value"="count")) %>% 
      mutate(rate_cleaned = case_when(
        type =="cases"&rate=="rate"~"count per 1 M",
        type =="deaths"&rate=="rate"~"count per 10 M",
        type =="tests"&rate=="rate"~"Tests per 1 M",
        type =="positivity"&rate=="rate"~"% of Tests Positive",
        TRUE~"count"
      )) %>% 
      mutate(n = round(n, 0)) %>% 
      arrange(country, type,rate, loc, date) 
    
    tidy.cumulative %>%
      filter(level == "L2") %>% 
      # filter(type == "tests") %>% 
      filter(rate == "count") %>% 
      ggplot(aes(date, n,col =loc))+
      geom_line()+
      # facet_wrap(~country)+
      facet_grid(rows = vars(country), cols = vars(type))+
      # facet_grid(rows = vars(type), cols = vars(rate))+
      theme(legend.position = 'none')
    
    tidy.cumulative.data = tidy.cumulative %>%
      mutate(level = case_when(
        level == "city"~"L1",
        level == "sub-city"~"L2",
        TRUE~level
      )) %>% 
      select(level, country,type, rate, salid,age_group, loc, date, value = n)%>% 
      filter(level%in%c("L1","L2"))}
}

#12. Outputs   ###### 
if (!str_detect(getwd(),"Dev")){
  # ___12.1 trends_cases_death.csv  -------
  {
    ##  Daily Counts
    raw.daily.subnational =  tidy.daily.subnational %>%
      filter(!type%in%c( "positivity","tests")) %>%
      filter(level%in%c("L1","L2")) %>%
      mutate(type = type %>% recode("confirmed"="cases")) %>%
      mutate(rate = 'raw count')%>%
      mutate(type = type %>% recode("positivity"="tests")) %>%
      select(level,country, type, rate, salid, loc, age_group, date,value = daily_counts )
    ## Get Daily Rate
    raw.daily.subnational.rates =  tidy.daily.data %>%
      filter(level%in%c("L1","L2")) %>%
      filter(!type%in%c( "positivity","tests")) %>%
      filter(rate =="rate") %>%
      mutate(rate_label = type %>% recode(
        "cases"="New Cases per 1M",
        "deaths"="New Deaths per 10M",
        "tests"="New Tests per 1M"
      )) %>%
      select(level,country, salid, loc, age_group, type, date, rate_7dayMA = value,rate_label )
    tidy.daily.data.output_1 = tidy.daily.data %>%
      bind_rows(raw.daily.subnational) %>%
      filter(rate !="rate") %>%
      filter(!type%in%c( "positivity","tests")) %>%
      rename(count_smoothed = rate) %>%
      mutate(count_smoothed = count_smoothed %>%
               recode("count"="count_7dayMA",
                      "raw count"="count_raw")) %>%
      pivot_wider(names_from = count_smoothed, values_from = value) %>%
      left_join(
        # bind_rows(pop_l1,pop_l2) %>% select(salid = loc, pop)
        pop_df_ar_age %>% select(salid = loc, age_group, pop)
      ) %>%
      select(level, country, salid,loc, age_group, type, date,count_raw,count_7dayMA)%>%
      arrange(level, country, salid, loc, type,date) %>%
      mutate_at(vars(count_raw, count_7dayMA), ~ifelse(.x<0,NA,.x))  %>%
      left_join(raw.daily.subnational.rates %>%
                  select(level, salid, age_group, type, date,rate_7dayMA, rate_label ),
                by =c("level", "salid","type", "age_group","date")) %>% 
      left_join(xwalk_countries) %>% 
      select(country= iso2, level, salid, type, date, age_group,
             count_raw, count_7dayMA,
             rate_7dayMA, rate_label) %>% 
      distinct()
    
    tidy.daily.data.output = tidy.daily.data.output_1 %>% 
      select(-rate_label) %>% 
      pivot_wider(names_from = type, values_from = c(count_raw, count_7dayMA, rate_7dayMA)) %>% 
      select(country, level, salid, date,age_group, 
             cases_raw = count_raw_cases,
             cases_7dayMA = count_7dayMA_cases,
             cases_per_1M = rate_7dayMA_cases, 
             deaths_raw = count_raw_deaths,
             deaths_7dayMA = count_7dayMA_deaths,
             deaths_per_10M = rate_7dayMA_deaths) %>% 
      mutate_at(vars(cases_raw,cases_7dayMA,cases_per_1M,deaths_raw,deaths_7dayMA,deaths_per_10M),
                ~round(.x,0))
    
  }
  
 
  # ___12.3 cumulative_cases_death.csv -----
  {
    tidy.cumulative.output =  tidy.cumulative %>%
      filter(!type%in%c("positivity","tests")) %>% 
      filter(level%in%c("L1","L2")) %>% 
      select(level, country, salid,type, age_group, rate, rate_cleaned,date, n) %>% 
      group_by(level, country, salid, age_group,type, rate, rate_cleaned) %>% 
      group_modify(~{
        .x %>% filter(date == max(date))
      }) %>% 
      ungroup() %>% 
      left_join(xwalk_countries) %>% 
      mutate(rate_cleaned = str_replace(rate_cleaned,
                                        "count",
                                        type) %>% 
               recode("cases per 1 M"="cases_per_1M",
                      "deaths per 10 M"="deaths_per_10M",
                      "Tests per 1 M"="test_per_1M",
                      "% of Tests Positive"="pct_positive")) %>% 
      select(level, country = iso2, salid, age_group,rate_cleaned, n) %>% 
      pivot_wider(names_from = rate_cleaned, values_from = n) %>% 
      drop_na()
  }
  
 
  
  {
    # ## Trends in cases/deaths
    # fwrite(tidy.daily.data.output,"../Outputs/SALURBAL_COVID19_trends_cases_death.csv")
    # zip::zip(files= c("../Outputs/SALURBAL_COVID19_trends_cases_death.csv"),
    #          zipfile = "../Outputs/SALURBAL_COVID19_trends_cases_death.zip",
    #          mode = "cherry-pick")
    # ## Trends in testing
    # fwrite(tidy.daily.testing.output,"../Outputs/SALURBAL_COVID19_trends_tests_positivity.csv")
    # zip::zip(files ="../Outputs/SALURBAL_COVID19_trends_tests_positivity.csv",
    #          zipfile = "../Outputs/SALURBAL_COVID19_trends_tests_positivity.zip",
    #          mode = "cherry-pick")
    # ## Cumulative outputs
    # fwrite(tidy.cumulative.output,"../Outputs/SALURBAL_COVID19_cumulative_cases_death.csv")
    # fwrite(tidy.cumulative.testing.output,"../Outputs/SALURBAL_COVID19_cumulative_tests_positivity.csv")
    # 
  }
}


