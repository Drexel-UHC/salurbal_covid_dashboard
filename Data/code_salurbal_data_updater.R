# 0. Setup -----
{
  rm(list=ls()[ !str_detect(ls(), c("cpu_RL|cpu_UHC|df_update_status")) ])
  options(timeout=14400)
  source("code_salurbal_data_updater_util.R")
  cutoff_date = "05-25-2021" 
  
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
}

# load("raw_files/work_space.RData")



# 2. Country Level  ------- 
try_country = try({
  # ___2.1 Get Raw Data (full_raw, full)####
  ## Get Data from Github
  raw_count  = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    as_tibble()
  raw_deaths =  fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
    as_tibble()
  ## Clean counts and deaths
  counts_file = clean_cum_csse(raw_count,"confirmed")
  deaths_file = clean_cum_csse(raw_deaths,"deaths")
  
  country_max_date = max(counts_file$date)
  country_min_date = mdy("03-15-2020")
  country_dates = seq(country_min_date,country_max_date, by ='day')
  counts  = tibble(loc = list(counts_file$loc %>% unique()),
                   date =country_dates) %>% 
    unnest() %>% 
    left_join(counts_file) %>% 
    arrange(loc, date) %>% 
    group_by(loc) %>% 
    group_modify(~{
      first_value_date = .x %>%pull(date) %>% min()
      .x %>% 
        mutate(confirmed=ifelse(is.na(confirmed)&date<first_value_date, 0, confirmed)) 
    }) %>% 
    ungroup()
  deaths  = tibble(loc = list(deaths_file$loc %>% unique()),
                   date =country_dates) %>% 
    unnest() %>% 
    left_join(deaths_file) %>% 
    arrange(loc, date) %>% 
    group_by(loc) %>% 
    group_modify(~{
      first_value_date = .x %>%pull(date) %>% min()
      .x %>% 
        mutate(deaths=ifelse(is.na(deaths)&date<first_value_date, 0, deaths)) 
    }) %>% 
    ungroup()
  
  ## Countries of Interest
  countries_references = get_ref_countries(counts,10)
  countries_interest =  c(countries_salurbal,other_lac_countries,countries_references)
  
  # Prepare Color Pallete
  df.color = lac_colors()
  # Country Centroid coordinates  
  country_coords = fread("helper_files/country_google_cord.csv") %>% as_tibble() %>% 
    filter(!loc%in%c("Diamond Princess",
                     "Georgia",
                     "Holy See","Togo")) 
  
  ## Add country coordinates and color  
  full_all_raw  = full_join(counts,deaths, by = c("loc","date")) %>% 
    left_join(df.color, by = "loc") %>% 
    left_join(country_coords) %>% 
    mutate(date_label = paste(lubridate::month(date,label=T, abbr = T),",",day(date))) 
  
  
  full_all= full_all_raw%>% 
    filter(!is.na(lng))
  
  
  all_dates_tmp = full_all %>% 
    select(date, date_label) %>% 
    distinct() %>% 
    arrange(date)
  n_weeks_tmp =floor( nrow(all_dates_tmp)/7)
  n_months_tmp =floor( nrow(all_dates_tmp)/30)
  rows_tmp =  rev(nrow(all_dates_tmp)-seq(0,(n_months_tmp*30),by =30))
  subset_dates_tmp = all_dates_tmp %>% 
    slice(rows_tmp) %>% 
    pull(date)
  map_global_totals = full_all_raw %>% group_by(date) %>% 
    summarize(confirmed  = sum(confirmed),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    filter(date%in%subset_dates_tmp)
  
  
  map_daily_data = full_all  %>% 
    filter(!loc%in%c("Burma","Kosovo","Moldova","West Bank and Gaza") )%>% 
    left_join(pop_df %>% filter(level == "country") %>% select(-level) %>% 
                mutate(loc = case_when(
                  loc=="Brunei Darussalam" ~"Brunei",
                  loc == "Congo" ~ "Congo (Brazzaville)",
                  loc == "Democratic Republic of the Congo" ~ "Congo (Kinshasa)",
                  loc == "C?te d'Ivoire" ~ "Cote d'Ivoire",
                  loc == "Republic of Korea" ~ "Korea, South",
                  loc == "Syrian Arab Republic" ~ "Syria",
                  loc == "China, Taiwan Province of China" ~ "Taiwan*",
                  loc == "Viet Nam" ~ "Vietnam",
                  loc == "United Republic of Tanzania" ~ "Tanzania",
                  loc == "Lao People's Democratic Republic" ~ "Laos",
                  TRUE~loc
                ))) %>% 
    select(-colors,-size) %>% 
    mutate(confirmed_rate = (confirmed/pop)*10^6,
           deaths_rate = (deaths/pop)*10^7 ) %>% 
    pivot_longer(cols=c(confirmed,deaths,confirmed_rate,deaths_rate),
                 names_to = "rate",
                 values_to = "n") %>% 
    mutate(n = as.integer(n))
  
  max_radius = 40
  min_radius = 2
  map_daily_radius = map_daily_data %>% 
    filter(date == max(date)) %>% 
    filter(loc!="San Marino") %>% 
    group_by(date,rate) %>% 
    top_n(1,n) %>% 
    mutate(radius_factor = log(n)/log(max_radius)) %>% 
    ungroup() %>% 
    select(rate,radius_factor )
  
  map_daily_data = map_daily_data %>% 
    filter(loc!="San Marino")%>% 
    filter(n>0) %>% 
    mutate(log_n = log10(n)) %>% 
    left_join(map_daily_radius)%>% 
    mutate(radius = (n)^(1/radius_factor)) %>% 
    mutate(radius =ifelse(radius<min_radius,min_radius,radius)) %>% 
    filter(date%in%subset_dates_tmp)
  
  
  # ___2.2 Cumulative ####
  full  =  full_all %>%  filter(loc%in%countries_interest) %>% 
    mutate(level = 'country', country ="Global") %>% 
    left_join(pop_df %>% filter(level == "country") %>% select(-level)) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  
  
  
  
  # ___2.3 Daily ####
  
  ## deaths and confirmed data
  tidy.daily.country = 
    list(7) %>% 
    map_df(function(x){
      clean_daily_csse(counts, deaths,lock.dates.country,x) %>% 
        mutate(smooth_days = x)
    })  %>% 
    select(-smooth_days) %>% 
    left_join(  pop_df %>% 
                  filter(level == "country")  )  %>% 
    mutate(rate = (rollsum/pop)*10^6 )%>% 
    select(-pop) %>% 
    mutate(country = "global")
  
  print("Okay")
})
# 3. Brazil   ####

try_BR = try({
  #___3.1 Get Raw Data ####
  raw_br_mun_file  =  fread("raw_files/cases-brazil-cities-time.csv.gz") %>% 
    as_tibble() %>% 
    filter(ibgeID != "0") %>% 
    filter(!str_detect(string = city, pattern = "INDEFINIDA")) %>% 
    filter(nchar(ibgeID)>3) %>% 
    select(date,
           mun = ibgeID,
           confirmed = totalCases,
           deaths) %>%
    mutate(country = "Brazil",
           date = ymd(date),
           mun = as.character(mun)) %>% 
    mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII"))%>% ungroup() %>% 
    arrange(mun, date) 
  
  br_max_date = max(raw_br_mun_file$date) 
  br_min_date = mdy("03-15-2020")
  br_dates = seq(br_min_date,br_max_date, by ='day')
  
  br_mun_tmp = tibble(mun = list(raw_br_mun_file$mun %>% unique()),
                      date =br_dates) %>% 
    unnest() %>% 
    filter(date>mdy("03-15-2020")) %>% 
    left_join(raw_br_mun_file) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(country)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Brazil") %>% 
        fill(confirmed) %>% 
        fill(deaths)
    }) %>% 
    ungroup()
  
  load("helper_files/xwalk_BR_states.rdata")
  br_state_tmp  = fread("raw_files/brazil_state_tmp.csv") %>%
    as_tibble() %>% #stringi::stri_trans_general(.x, "Latin-ASCII")
    select(date,
           loc = state,
           confirmed = totalCases,
           deaths) %>%
    mutate(country = "Brazil",
           date = ymd(date)) %>%
    filter(loc != "TOTAL") %>%
    left_join(df.color) %>%
    group_by(date, loc, country) %>%
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>%
    left_join(xwalk.brazil.states, by = c("loc"="state")) %>%
    select(date, loc = state.name, country, confirmed, deaths) %>%
    mutate(level = "state") 
  
  #___3.2 Cumulative  ####
  full_br_state = tibble(loc = list(unique(br_state_tmp$loc)),
                         date = seq(br_min_date,br_max_date, by = 'day') ) %>% 
    unnest() %>% 
    filter(date>mdy("03-15-2020")) %>% 
    arrange(loc, date) %>% 
    left_join(br_state_tmp) %>% 
    group_by(loc) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(country)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Brazil") %>% 
        fill(confirmed) %>% 
        fill(deaths)
    }) %>% 
    ungroup()%>%
    mutate(level = "state")  %>% 
    arrange(loc,date)
  
  full_br_l1 = br_mun_tmp %>% 
    left_join(select(xwalk_sal_br,mun, salid1, salid1_name )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Brazil Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("BR","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    ungroup() %>% 
    arrange(salid1, date)
  
  full_br_l2 = br_mun_tmp %>% 
    left_join(select(xwalk_sal_br,mun, salid2, salid2_name, )) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Brazil Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("BR","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name)%>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    ungroup() %>% 
    arrange(salid2, date)
  
  #___3.2 Brazil Daily #####
  tidy.daily.br.state = clean_daily_salurbal_smooth(full_br_state) %>% ungroup() %>% mutate(level = "state")
  tidy.daily.br.l1 = clean_daily_salurbal_smooth(full_br_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.br.l2 = clean_daily_salurbal_smooth(full_br_l2)%>% ungroup()%>% mutate(level = "L2")
  
  print("Okay")
})

# 4. Mexico  ####
try_MX = try({
  # ___4.1 Raw Data  ####
  
  ## Cases at Municipal Level
  raw_mx_mun_confirmed  = fread("raw_files/mx_mun_cases_tmp.csv") %>% 
    as_tibble() %>% 
    select(FECHA_INGRESO,RESULTADO=RESULTADO_LAB ,ENTIDAD_RES,MUNICIPIO_RES) %>% 
    filter(RESULTADO==1) %>%
    clean_names() %>% 
    select(date = fecha_ingreso,ent =entidad_res, mun = municipio_res) %>% 
    mutate(ent = str_pad(ent, width = 2, pad = "0"),
           mun = str_pad(mun, width = 3, pad = "0"),
           mun = paste0(ent, mun)) %>% 
    select(-ent) %>% 
    group_by(date, mun) %>% 
    summarise(confirmed =n()) %>% 
    ungroup() %>% 
    mutate(date = ymd(date)) %>% 
    select(mun, date, confirmed) %>% 
    arrange(mun, date) 
  
  raw_mx_mun_tests  = fread("raw_files/mx_mun_cases_tmp.csv") %>% 
    as_tibble() %>% 
    select(FECHA_INGRESO,RESULTADO=RESULTADO_LAB ,ENTIDAD_RES,MUNICIPIO_RES) %>% 
    filter(RESULTADO%in%c(1,2)) %>%
    clean_names() %>% 
    select(date = fecha_ingreso,
           ent =entidad_res, 
           mun = municipio_res,
           result = resultado) %>% 
    mutate(ent = str_pad(ent, width = 2, pad = "0"),
           mun = str_pad(mun, width = 3, pad = "0"),
           mun = paste0(ent, mun)) %>% 
    select(-ent) %>% 
    group_by(date, mun) %>% 
    summarise(tests =n()) %>% 
    ungroup() %>% 
    mutate(date = ymd(date)) %>% 
    select(mun, date, tests) %>% 
    arrange(mun, date)
  
  
  
  raw_mx_mun_deaths = fread("raw_files/mx_mun_cases_tmp.csv") %>% 
    as_tibble() %>% 
    select(FECHA_DEF,RESULTADO=RESULTADO_LAB,ENTIDAD_RES,MUNICIPIO_RES) %>% 
    filter(RESULTADO==1)%>%
    filter(FECHA_DEF != "9999-99-99") %>% 
    select(date_death = FECHA_DEF,ent =ENTIDAD_RES, mun = MUNICIPIO_RES) %>% 
    mutate(ent = str_pad(ent, width = 2, pad = "0"),
           mun = str_pad(mun, width = 3, pad = "0"),
           mun = paste0(ent, mun)) %>% 
    select(-ent) %>% 
    group_by(date_death, mun) %>% 
    summarise(deaths =n()) %>% 
    ungroup() %>% 
    mutate(date = ymd(date_death)) %>% 
    select(mun, date, deaths) %>% 
    arrange(mun, date) 
  
  raw_mx_mun_file = full_join(raw_mx_mun_confirmed,raw_mx_mun_deaths) %>%
    left_join(raw_mx_mun_tests) %>% 
    mutate_at(vars(confirmed, tests,deaths), ~ifelse(is.na(.x),0,.x)) %>% 
    mutate(country = "Mexico") %>% 
    arrange(mun,date) %>% 
    group_by(mun) %>% 
    mutate(confirmed = cumsum(confirmed),
           deaths = cumsum(deaths),
           tests = cumsum(tests)) %>% 
    ungroup()
  
  mx_min_date = mdy("03-15-2020")
  mx_max_date = max(raw_mx_mun_file$date)
  mx_dates = seq(mx_min_date,mx_max_date, by ='day')
  raw_mx_mun =  tibble(mun = list(raw_mx_mun_file$mun %>% unique()),
                       date =mx_dates) %>% 
    unnest() %>% 
    left_join(raw_mx_mun_file) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(country)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths,tests),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Mexico") %>% 
        fill(confirmed) %>% 
        fill(deaths) %>% 
        fill(tests)
    }) %>% 
    ungroup()
  
  
  
  ## State Level
  url_mx_deaths = "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_muertes.csv"
  url_mx_cases = "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_confirmados.csv"
  mx_state_cases_raw = fread(url_mx_cases) %>%
    as_tibble() %>% 
    pivot_longer(cols = -Fecha,names_to = "state", values_to = "confirmed") %>% 
    rename(date = Fecha) %>% 
    mutate(state = stringi::stri_trans_general(state, "Latin-ASCII") %>% str_trim())
  mx_state_deaths_raw = fread(url_mx_deaths) %>%
    as_tibble()%>% 
    pivot_longer(cols = -Fecha,names_to = "state", values_to = "deaths") %>% 
    rename(date = Fecha) %>% 
    mutate(state = stringi::stri_trans_general(state, "Latin-ASCII") %>% str_trim())
  raw_mx_state_file =mx_state_cases_raw %>% left_join(mx_state_deaths_raw) %>% 
    mutate(level = 'state',
           country = "Mexico",
           date = ymd(date)) %>% 
    filter(state != "Nacional") %>% 
    mutate(state = case_when(
      state=="Ciudad de MA(C)xico" ~ "Ciudad de Mexico",
      state=="QuerA(C)taro" ~ "Queretaro",
      state=="MA(C)xico" ~ "Mexico",
      state=="Nuevo LeA?n" ~ "Nuevo Leon",
      state=="Nuevo LeA?n" ~ "Nuevo Leon",
      state=="YucatA?n" ~ "Yucatan",
      state=="YucatA?n" ~ "Yucatan",
      state=="San Luis PotosA-" ~ "San Luis Potosi",
      state=="MichoacA?n" ~ "Michoacan",
      TRUE ~ state
    )) %>% 
    rename(loc = state) %>% 
    arrange(loc, date) 
  
  # ___4.2 Cumulative  ####
  full_mx_state =  tibble(loc = list(raw_mx_state_file$loc %>% unique()),
                          date =mx_dates) %>% 
    unnest() %>% 
    left_join(raw_mx_state_file) %>% 
    arrange(loc, date) %>% 
    group_by(loc) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(country)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Mexico") %>% 
        fill(confirmed) %>% 
        fill(deaths)
    }) %>% 
    ungroup() %>% 
    mutate(level = "state" ) %>% 
    select(date, loc, country,confirmed, deaths, level) %>% 
    arrange(loc,date)
  
  full_mx_l1 = raw_mx_mun  %>% 
    left_join(select(xwalk_sal_mx,mun, salid1, salid1_name )) %>%  
    group_by(country, salid1, salid1_name) %>%
    group_modify(function(x,...){
      all_dates = tibble(date = seq(min(x$date),max(x$date), by = '1 day'))
      x %>% 
        group_by(mun) %>% 
        group_modify(function(z,...){
          all_dates %>% left_join(z) %>% 
            fill(-date, .direction = "down") %>% 
            filter(!(is.na(confirmed)&is.na(deaths)))
        })
    }) %>% 
    ungroup() %>% 
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Mexico Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("MX","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    arrange(salid1,date)
  
  full_mx_l2 = raw_mx_mun %>% 
    left_join(select(xwalk_sal_mx,mun, salid2, salid2_name )) %>%  
    group_by(country, salid2, salid2_name) %>%
    group_modify(function(x,...){
      all_dates = tibble(date = seq(min(x$date),max(x$date), by = '1 day'))
      x %>% 
        group_by(mun) %>% 
        group_modify(function(z,...){
          all_dates %>% left_join(z) %>% 
            fill(-date, .direction = "down") %>% 
            filter(!(is.na(confirmed)&is.na(deaths)))
        })
    }) %>% 
    ungroup() %>% 
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Mexico Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("MX","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name)%>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2)) %>% 
    arrange(salid2, date)
  
  # ___4.3 Daily #####
  tidy.daily.mx.state = clean_daily_salurbal_smooth(full_mx_state) %>% ungroup() %>% mutate(level = "state")
  tidy.daily.mx.l1 = clean_daily_salurbal_smooth(full_mx_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.mx.l2 = clean_daily_salurbal_smooth(full_mx_l2)%>% ungroup()%>% mutate(level = "L2")
  
  # ___4.4 Daily Testing #####
  tidy.daily.tests.mx.l1 = raw_mx_mun %>% 
    left_join(select(xwalk_sal_mx,mun, salid1, salid1_name, country )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Mexico Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("MX","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    arrange(salid1, date) %>% 
    select(country,loc,salid = salid1, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    left_join(pop_l1 %>% select(salid = loc, pop)) %>% 
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L1") %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  tidy.daily.pos.mx.l1 = raw_mx_mun %>% 
    left_join(select(xwalk_sal_mx,mun, salid1, salid1_name, country )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Mexico Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("MX","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    arrange(salid1, date) %>% 
    # filter(salid1 == "204101") %>% 
    select(country,loc,salid = salid1, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    mutate(daily_positivity_pct =daily_cases/daily_tests*100 %>% round(2),
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L1") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  
  tidy.daily.tests.mx.l2 = raw_mx_mun %>% 
    left_join(select(xwalk_sal_mx,mun, salid2, salid2_name, country)) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Mexico Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("MX","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name) %>% 
    arrange(salid2, date) %>% 
    select(country,loc,salid = salid2, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na()%>% 
    left_join(pop_l2 %>% select(salid = loc, pop)) %>% 
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L2") %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  tidy.daily.pos.mx.l2 = raw_mx_mun %>% 
    left_join(select(xwalk_sal_mx,mun, salid2, salid2_name, country)) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Mexico Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("MX","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name) %>% 
    arrange(salid2, date) %>% 
    select(country,loc,salid = salid2, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    mutate(daily_positivity_pct =daily_cases/daily_tests*100 %>% round(2),
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L2")  %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  print("Okay")
})

# 5. Chile  ####
try_CL = try({
  #___5.1 Get raw Data ####
  
  ## Cases at Municipal Level
  xwalk_state_cl = fread("raw_files/chile_raw_counts_tmp.csv") %>% as_tibble() %>% 
    select(state = Region, state_id = "Codigo region") %>% distinct() %>% 
    mutate(state_id = str_pad(state_id, width = 2, pad = "0")) %>% 
    mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    mutate(state = case_when(
      str_detect(state,"TarapacA") ~ "Tarapaca",
      str_detect(state,"Higgins") ~ "O'Higgins",
      state == "ValparaA-so" ~ "Valparaiso",
      state == "A'uble" ~ "Nuble",
      state == "BiobA-o" ~ "Biobio",
      state == "Los RA-os" ~ "Los Rios",
      state == "AysA(C)n" ~ "Aysen",
      TRUE~state)) %>% 
    mutate(state = str_to_lower(state)) %>% 
    mutate(state =  paste0(toupper(substr(state, 1, 1)), substr(state, 2, nchar(state))))
  
  
  raw_cl_counts_mun_file =  fread("raw_files/chile_raw_counts_tmp.csv") %>% 
    as_tibble() %>% 
    select(-c(Tasa,Region,"Codigo region",Comuna, Poblacion)) %>% 
    rename(mun = "Codigo comuna") %>% 
    filter(!is.na(mun)) %>% 
    pivot_longer(cols = -mun) %>% 
    rename(date = name, confirmed = value) %>% 
    mutate(mun = str_pad(mun, width = 5, pad = "0"),
           date = lubridate::ymd(date)) %>% 
    select(mun, date, confirmed)%>% 
    arrange(mun, date)
  cl_min_date = mdy("03-15-2020")
  cl_max_date = max(raw_cl_counts_mun_file$date)
  cl_dates = seq(cl_min_date,cl_max_date, by ='day')
  
  raw_cl_counts_mun =  tibble(mun = list(raw_cl_counts_mun_file$mun %>% unique()),
                              date =cl_dates) %>% 
    unnest() %>% 
    left_join(raw_cl_counts_mun_file) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(confirmed)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Chile") %>% 
        fill(confirmed)
    }) %>% 
    ungroup()
  
  mun_daily_smoothed_cases_chile = raw_cl_counts_mun %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    select(mun, date, rollsum_cases)
  
  raw_cl_tests_mun  =  fread("raw_files/chile_raw_tests_tmp.csv") %>% 
    as_tibble() %>% 
    mutate(mun = str_pad(`Codigo comuna`, width = 5, pad = '0')) %>% 
    select(mun, date = fecha, pos= positividad ) %>% 
    mutate( date = lubridate::ymd(date)) %>% 
    filter(date%in%cl_dates) %>% 
    left_join(mun_daily_smoothed_cases_chile) %>% 
    filter(!is.na(rollsum_cases)) %>% 
    mutate(tests_rollsum = rollsum_cases/pos) 
  
  raw_cl_deaths_mun_file  =  fread("raw_files/chile_raw_deaths_tmp.csv") %>% 
    as_tibble() %>% 
    select(mun = "Codigo comuna",
           date = "Fecha",
           deaths = "Casos fallecidos") %>% 
    filter(!is.na(mun)) %>% 
    mutate(mun = str_pad(mun,5,"left","0"),
           date = lubridate::ymd(date)) %>% 
    arrange(mun, date)
  
  raw_cl_deaths_mun =  tibble(mun = list(raw_cl_deaths_mun_file$mun %>% unique()),
                              date =cl_dates) %>% 
    unnest() %>% 
    left_join(raw_cl_deaths_mun_file) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(deaths)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Chile") %>% 
        fill(deaths)
    }) %>% 
    ungroup()
  
  raw_cl_mun = full_join(raw_cl_counts_mun,raw_cl_deaths_mun) %>% 
    select(country, mun, date, confirmed, deaths) %>% 
    arrange(mun, date) %>% 
    left_join(xwalk_sal_cl) %>%
    fill(country) %>%
    fill(iso2c) %>%
    mutate(date = ymd(date),
           confirmed=as.integer(confirmed),
           deaths = as.integer(deaths)) %>%
    filter(!is.na(mun))
  
  
  #___5.2 Cumulative ####
  full_cl_state = raw_cl_mun  %>% 
    select(mun, date, confirmed, deaths) %>% 
    mutate(level = "state",
           state_id = str_sub(mun, 1,2),
           country = "Chile") %>% 
    left_join(xwalk_state_cl) %>% 
    select(-state_id, - mun ) %>% 
    rename(loc = state) %>% 
    group_by(loc, date, country, level) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths, na.rm = T)) %>% 
    ungroup() %>% 
    select(date, loc, country,confirmed, deaths, level) %>% 
    arrange(loc, date)
  
  full_cl_l1 = raw_cl_mun  %>% 
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(salid1_name,date) %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Chile Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("CL","888"),
                           salid1)) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>% 
    rename(loc = salid1_name)%>% 
    arrange(salid1, date)
  
  full_cl_l2 = raw_cl_mun %>% 
    mutate(level = "L2") %>% 
    group_by(date, level,salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(salid2_name,date) %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Chile Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("CL","888"),
                           salid2)) %>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>% 
    rename(loc = salid2_name)%>% 
    arrange(salid2, date)
  
  
  
  
  #___5.3 -  Daily #####
  tidy.daily.cl.state = clean_daily_salurbal_smooth(full_cl_state) %>% ungroup() %>% mutate(level = "state")
  tidy.daily.cl.l1 = clean_daily_salurbal_smooth(full_cl_l1)%>% ungroup()%>% mutate(level = "L1") %>% mutate(smooth_days =2)
  tidy.daily.cl.l2 = clean_daily_salurbal_smooth(full_cl_l2)%>% ungroup()%>% mutate(level = "L2")%>% mutate(smooth_days =2)
  
  #___5.4 -  Daily Tests #####
  ## Daily positive is already 6 day smooth, 
  # glimpse(tidy.daily.tests.gt.l1)
  tidy.daily.tests.cl.l1 = raw_cl_tests_mun %>% 
    filter(!is.na(pos)) %>%
    filter(pos>0) %>%
    left_join(xwalk_sal_cl %>% 
                select(mun, salid1,salid1_name, country)) %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Chile Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("CL","888"),
                           salid1)) %>%
    rename(loc = salid1_name) %>% 
    group_by(salid1, loc, country, date) %>% 
    summarize(rollsum_cases= sum(rollsum_cases, na.rm = T),
              tests_rollsum = sum(tests_rollsum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(rate = round((rollsum_cases/tests_rollsum*100),2)) %>% 
    mutate(rate = ifelse(is.na(rate),0,rate),
           type ="positivity",
           daily_counts = NA,
           smooth_days = 7,
           level = "L1") %>% 
    mutate(country = "Chile") %>% 
    mutate(daily_counts =tests_rollsum) %>% 
    select(country,loc, salid= salid1, date,type,daily_counts,rollsum = tests_rollsum, rate,smooth_days, level)
  tidy.daily.tests.cl.l1 %>% 
    ggplot(aes(date, rate, col = salid))+
    geom_line()
  tidy.daily.tests.cl.l2 = raw_cl_tests_mun %>% 
    filter(!is.na(pos)) %>% 
    filter(pos>0) %>% 
    left_join(xwalk_sal_cl %>% 
                select(mun, salid2,salid2_name, country)) %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Chile Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("CL","888"),
                           salid2)) %>% 
    ungroup() %>% 
    rename(loc = salid2_name) %>% 
    group_by(salid2, loc, country, date) %>% 
    summarize(rollsum_cases= sum(rollsum_cases, na.rm = T),
              tests_rollsum = sum(tests_rollsum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(rate = round((rollsum_cases/tests_rollsum*100),2)) %>% 
    mutate(rate = ifelse(is.na(rate),0,rate),
           type ="positivity",
           daily_counts = NA,
           smooth_days = 7,
           level = "L2")  %>% 
    mutate(country = "Chile") %>% 
    mutate(daily_counts =tests_rollsum) %>% 
    select(country,loc, salid= salid2, date,
           type,daily_counts,rollsum = tests_rollsum, 
           rate,smooth_days, level)
  
  
  # tidy.daily.tests.cl.l1 = raw_cl_tests_mun %>% 
  #   filter(!is.na(pos)) %>% 
  #   filter(pos>0)
  # 
  # tidy.daily.tests.cl.l2 = raw_cl_tests_mun %>% 
  #   filter(!is.na(pos)) %>% 
  #   filter(pos>0)
  
  print("Okay")
})

# 6. Columbia  ####
try_CO = try({
  #___6.1 Get raw Data ####
  raw_co_mun_confirmed  = fread("raw_files/colombia_tmp.csv") %>% 
    as_tibble() %>%  
    clean_names() %>%
    select(date = contains("fecha_de_notifica"),
           mun = contains("divipola_municipio")) %>% 
    mutate(mun = str_pad(mun, width = 5, pad = "0")) %>% 
    mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    group_by(date, mun) %>% 
    summarise(confirmed =n()) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(index_tmp = str_locate(date," ")[1]-1) %>% 
    mutate(date2 = (str_sub(date,1,index_tmp))) %>% 
    ungroup() %>% 
    mutate(date = dmy(date2) )%>% 
    filter(date>"2020-03-01")%>% 
    select(mun, date, confirmed) %>% 
    arrange(mun, date)
  
  raw_co_mun_deaths  = fread("raw_files/colombia_tmp.csv") %>% 
    as_tibble() %>%  
    clean_names() %>%
    select(date = contains("fecha_de_muer"),
           mun = contains("divipola_municipio")) %>% 
    mutate(mun = str_pad(mun, width = 5, pad = "0")) %>% 
    mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    filter(date != "") %>% 
    rowwise() %>% 
    mutate(index_tmp = str_locate(date," ")[1]-1) %>% 
    mutate(date2 = (str_sub(date,1,index_tmp))) %>% 
    ungroup() %>% 
    mutate(date = dmy(date2) )%>% 
    filter(date>"2020-03-01") %>% 
    group_by(date, mun) %>% 
    summarise(deaths =n()) %>% 
    ungroup() %>% 
    select(mun, date, deaths) %>% 
    arrange(mun, date) 
  
  raw_co_mun_file =full_join(raw_co_mun_confirmed,raw_co_mun_deaths) %>% 
    mutate_at(vars(confirmed, deaths), ~ifelse(is.na(.x),0,.x)) %>% 
    mutate(country = "Colombia") %>% 
    arrange(mun,date) %>% 
    group_by(mun) %>% 
    mutate(confirmed = cumsum(confirmed),
           deaths = cumsum(deaths)) %>% 
    ungroup() %>% 
    filter(!is.na(date))
  
  co_min_date = mdy("03-15-2020")
  co_max_date = max(raw_co_mun_file$date)
  co_dates = seq(co_min_date,co_max_date, by ='day')
  raw_co_mun =  tibble(mun = list(raw_co_mun_file$mun %>% unique()),
                       date =co_dates) %>% 
    unnest() %>% 
    left_join(raw_co_mun_file) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(country)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Colombia") %>% 
        fill(confirmed) %>% 
        fill(deaths)
    }) %>% 
    ungroup()
  
  #___6.2 Cumulative #####
  
  full_co_l1 = raw_co_mun  %>% 
    left_join(select(xwalk_sal_co,mun, salid1, salid1_name )) %>%  
    group_by(country, salid1, salid1_name) %>%
    group_modify(function(x,...){
      all_dates = tibble(date = seq(min(x$date),max(x$date), by = '1 day'))
      x %>% 
        group_by(mun) %>% 
        group_modify(function(z,...){
          all_dates %>% left_join(z) %>% 
            fill(-date, .direction = "down") %>% 
            filter(!(is.na(confirmed)&is.na(deaths)))
        })
    }) %>% 
    ungroup() %>% 
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Colombia Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("CO","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    ungroup() %>% 
    arrange(salid1,date)
  
  full_co_l2 = raw_co_mun %>% 
    left_join(select(xwalk_sal_co,mun, salid2, salid2_name )) %>%  
    group_by(country, salid2, salid2_name) %>%
    group_modify(function(x,...){
      all_dates = tibble(date = seq(min(x$date),max(x$date), by = '1 day'))
      x %>% 
        group_by(mun) %>% 
        group_modify(function(z,...){
          all_dates %>% left_join(z) %>% 
            fill(-date, .direction = "down") %>% 
            filter(!(is.na(confirmed)&is.na(deaths)))
        })
    }) %>% 
    ungroup() %>% 
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Colombia Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("CO","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name)%>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2)) %>% 
    ungroup() %>% 
    arrange(salid2, date)
  
  #___6.3 Daily #####
  #tidy.daily.co.state = clean_daily_salurbal_smooth(full_co_state) %>% ungroup() %>% mutate(level = "state")
  tidy.daily.co.l1 = clean_daily_salurbal_smooth(full_co_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.co.l2 = clean_daily_salurbal_smooth(full_co_l2)%>% ungroup()%>% mutate(level = "L2")
  
  print("Okay")
})

# 7. Peru (Micro) ####
try_PE = try({
  #___7.1 Get raw data  #####
  ##Clean mun name to mun id xwalk 
  library(foreign)
  xwalk_peru_mun = read.dbf("helper_files/peru_distritos.dbf") %>% as_tibble() %>% 
    clean_names() %>% 
    select(state = nombdep , 
           mun = nombdist,
           mun_id = iddist) %>% 
    mutate_all(~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    group_by(mun_id ) %>% 
    mutate(mun = str_replace(mun,"A'","N")) %>% 
    ungroup() 
  
  
  ## Download Data
  peru_mun_cases_raw = fread("raw_files/url_peru_cases_tmp.csv") %>% 
    as_tibble() %>% 
    filter(METODODX=="PCR") %>% 
    filter(DISTRITO!="EN INVESTIGACIÃN") %>% 
    select(date = FECHA_RESULTADO,
           state = DEPARTAMENTO,
           mun = DISTRITO) %>% 
    mutate(state = ifelse(state == "LIMA REGION","LIMA",state))%>% 
    mutate_all(~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    mutate(mun = 
             case_when(mun == "CORONEL GREGORIO ALBARRACIN L."~"CORONEL GREGORIO ALBARRACIN LANCHIPA",
                       mun == "PANGOA"~"MAZAMARI - PANGOA",
                       mun == "MAZAMARI"~"MAZAMARI - PANGOA",
                       mun == "NAZCA"~"NASCA",
                       mun == "ANDRES AVELINO CACERES D."~"ANDRES AVELINO CACERES DORREGARAY",
                       TRUE~mun
             )
    ) %>% left_join(xwalk_peru_mun)
  
  peru_mun_cases = peru_mun_cases_raw %>% 
    count(state,date, mun_id, name = "confirmed")
  
  peru_mun_deaths_raw = fread("raw_files/url_peru_deaths_tmp.csv") %>% as_tibble()%>% 
    filter(DISTRITO!="EN INVESTIGACIÃN") %>% 
    filter(DISTRITO!="") %>% 
    select(date = FECHA_FALLECIMIENTO,
           state = DEPARTAMENTO,
           mun = DISTRITO) %>% 
    mutate(state = ifelse(state == "LIMA REGION","LIMA",state))%>% 
    mutate_all(~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
    mutate(mun = 
             case_when(mun == "CORONEL GREGORIO ALBARRACIN L."~"CORONEL GREGORIO ALBARRACIN LANCHIPA",
                       mun == "PANGOA"~"MAZAMARI - PANGOA",
                       mun == "MAZAMARI"~"MAZAMARI - PANGOA",
                       mun == "NAZCA"~"NASCA",
                       mun == "ANDRES AVELINO CACERES D."~"ANDRES AVELINO CACERES DORREGARAY",
                       mun == "CAMPOREDONDO"~"CAMPORREDONDO",
                       mun == "GUADALUPITO"~"NASCA",
                       mun == "CHEP+N"~"CHEPEN",
                       mun == "CARMEN DE LA LEGUA-REYNOSO"~"CARMEN DE LA LEGUA REYNOSO",
                       mun == "VIR+"~"VIRU",
                       mun == "ANDRES AVELINO CACERES DORREGA"~"ANDRES AVELINO CACERES DORREGARAY",
                       mun == "NAZCA"~"NASCA",
                       mun == "NAZCA"~"NASCA",
                       
                       TRUE~mun
             )
    ) %>% 
    mutate(state = 
             case_when(mun == "GUADALUPITO" ~ "LA LIBERTAD",
                       mun == "HUARAL"~"LIMA",
                       mun == "NASCA"~"ICA",
                       
                       
                       TRUE ~ state
                       
             )) %>% 
    left_join(xwalk_peru_mun) %>% 
    filter(!is.na(mun_id))
  
  peru_mun_deaths = peru_mun_deaths_raw %>% 
    count(state,date,mun_id , name = "deaths") 
  
  
  peru_mun_file_raw= full_join(peru_mun_cases,peru_mun_deaths)%>% 
    mutate_at(vars(confirmed, deaths),~ifelse(is.na(.x),0,.x)) %>% 
    rename(mun = mun_id) %>%
    mutate(date = ymd(as.character(date)))%>% 
    arrange(mun,date) %>% 
    group_by(state,mun) %>% 
    mutate(confirmed = cumsum(confirmed), deaths = cumsum(deaths)) %>% 
    ungroup()
  pe_min_date = mdy("03-15-2020")
  pe_max_date = max(raw_mx_mun_file$date)
  pe_dates = seq(mx_min_date,mx_max_date, by ='day')
  
  peru_mun_file =  tibble(mun = list(peru_mun_file_raw$mun %>% unique()),
                          date =pe_dates) %>% 
    unnest() %>% 
    left_join(peru_mun_file_raw %>% select(-state)) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~{
      first_value_date = .x %>% filter(!is.na(confirmed)) %>% pull(date) %>% min()
      .x %>% 
        mutate_at(vars(confirmed,deaths),~ifelse(is.na(.x)&date<first_value_date,0,.x)) %>% 
        mutate(country = "Peru") %>% 
        fill(confirmed) %>% 
        fill(deaths)
    }) %>% 
    ungroup() %>% 
    left_join(peru_mun_file_raw %>% select(mun, state) %>% distinct(), by = 'mun' )
  
  raw_pe_mun = peru_mun_file  %>% 
    left_join(xwalk_sal_pe) %>% 
    mutate(confirmed=as.integer(confirmed),
           deaths = as.integer(deaths)) %>% 
    mutate(country = "Peru",
           iso2c = "PE")%>% 
    arrange(mun, date)
  
  #___7.2 Cumulative ####
  
  full_pe_state = peru_mun_file  %>% 
    group_by(mun,date) %>% 
    mutate(state = str_to_lower(state) ) %>% 
    ungroup()  %>% 
    rename(loc = state) %>% 
    group_by(loc,date) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    filter(!is.na(date)) %>% 
    select(date, loc, confirmed,deaths) %>% 
    arrange(loc, date) %>% 
    mutate(level = "state",
           country = "Peru")
  
  
  
  full_pe_l1 = raw_pe_mun  %>% 
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths  = sum(deaths)) %>% 
    ungroup() %>% 
    arrange(salid1_name,date) %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Peru Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("PE","888"),
                           salid1)) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>% 
    rename(loc = salid1_name)%>% 
    arrange(salid1, date)
  
  full_pe_l2 = raw_pe_mun  %>% 
    mutate(level = "L2") %>% 
    group_by(date, level,salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed),
              deaths = sum(deaths)) %>% 
    ungroup() %>% 
    arrange(salid2_name,date) %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Peru Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("PE","888"),
                           salid2)) %>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>% 
    rename(loc = salid2_name) %>% 
    arrange(salid2, date)
  
  #___7.3 Daily ####
  tidy.daily.pe.state = clean_daily_salurbal_smooth(full_pe_state) %>% ungroup() %>% mutate(level = "state")
  tidy.daily.pe.l1 = clean_daily_salurbal_smooth(full_pe_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.pe.l2 = clean_daily_salurbal_smooth(full_pe_l2)%>% ungroup()%>% mutate(level = "L2")
  print("Okay")
})

# 8. Guatemala ####
try_GT = try({
  #___8.1  Get raw data  #####
  ## Raw cases (raw_gt_mun_cases)
  raw_gt_mun_cases = fread("raw_files/gt_cases_tmp.csv") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    select(-c(departamento,municipio, poblacion, codigo_departamento)) %>% 
    rename(mun = codigo_municipio) %>% 
    filter(mun!=99) %>% 
    mutate(mun = str_pad(mun, width = 4, pad = "0")) %>% 
    pivot_longer(-mun,"date", values_to = "confirmed") %>% 
    mutate(date = str_sub(date,2,-1L) %>% ymd()) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~.x %>% 
                   arrange(date) %>% 
                   mutate(confirmed = cumsum(confirmed))) %>% 
    ungroup()
  
  ## Raw Tests (raw_gt_mun_tests)
  raw_gt_mun_tests = fread("raw_files/gt_tests_tmp.csv") %>% 
    as_tibble() %>% 
    clean_names() %>% 
    select(-c(departamento,municipio, poblacion, codigo_departamento)) %>% 
    rename(mun = codigo_municipio) %>% 
    filter(mun!=99) %>% 
    mutate(mun = str_pad(mun, width = 4, pad = "0")) %>% 
    pivot_longer(-mun,"date", values_to = "tests") %>% 
    mutate(date = str_sub(date,2,-1L) %>% ymd()) %>% 
    arrange(mun, date) %>% 
    group_by(mun) %>% 
    group_modify(~.x %>% 
                   arrange(date) %>% 
                   mutate(tests = cumsum(tests))) %>% 
    ungroup()
  raw_gt_mun_tests %>% 
    ggplot(aes(date, tests, col = mun))+
    geom_line()+
    theme(legend.position = 'none')
  ## Raw deaths (raw_gt_mun_deaths)
  raw_gt_mun_deaths = fread("raw_files/gt_deaths_tmp.csv" ) %>% 
    as_tibble() %>% 
    clean_names() %>% 
    select(-c(departamento,municipio, poblacion, codigo_departamento)) %>% 
    rename(mun = codigo_municipio) %>% 
    filter(mun!=99) %>% 
    mutate(mun = str_pad(mun, width = 4, pad = "0")) %>% 
    pivot_longer(-mun,"date", values_to = "deaths") %>% 
    mutate(date = str_sub(date,2,-1L) %>% ymd()) %>% 
    arrange(mun, date)  %>% 
    group_by(mun) %>% 
    group_modify(~.x %>% 
                   arrange(date) %>% 
                   mutate(deaths = cumsum(deaths))) %>% 
    ungroup()
  
  ## Final Processed File (raw_gt_mun)
  raw_gt_mun_raw = raw_gt_mun_cases %>% 
    left_join(raw_gt_mun_deaths)%>% 
    left_join(raw_gt_mun_tests) %>% 
    mutate_at(vars(confirmed, tests,deaths), ~ifelse(is.na(.x),0,.x)) %>% 
    mutate(country = "Guatemala") %>% 
    arrange(mun,date)
  min_date_tmp = mdy("03-15-2020")
  max_date_tmp = max(raw_gt_mun_raw$date)
  raw_gt_mun =  tibble(mun = list(raw_gt_mun_raw$mun %>% unique()),
                       date = seq(min_date_tmp,max_date_tmp, by = 'day') ) %>% 
    unnest() %>% 
    left_join(raw_gt_mun_raw) %>% 
    arrange(mun, date) 
  
  #___8.2 Cumulative #####
  
  full_gt_l1 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid1, salid1_name )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              deaths = sum(deaths,na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Guatemala Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("GT","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    left_join(pop_df %>% filter(level == "L1") %>% select(-level), 
              by = c("salid1"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    ungroup() %>% 
    arrange(salid1, date)
  
  full_gt_l2 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid2, salid2_name, )) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              deaths = sum(deaths,na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Guatemala Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("GT","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name)%>% 
    left_join(pop_df %>% filter(level == "L2") %>% select(-level), 
              by = c("salid2"="loc")) %>% 
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))  %>% 
    ungroup() %>% 
    arrange(salid2, date)
  
  
  #___8.3 Daily #####
  tidy.daily.gt.l1 = clean_daily_salurbal_smooth(full_gt_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.gt.l2 = clean_daily_salurbal_smooth(full_gt_l2)%>% ungroup()%>% mutate(level = "L2")
  
  #___8.4 Daily Tests #####
  tidy.daily.tests.gt.l1 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid1, salid1_name )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Guatemala Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("GT","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    arrange(salid1, date) %>% 
    select(country,loc,salid = salid1, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na()%>% 
    left_join(pop_l1 %>% select(salid = loc, pop)) %>% 
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L1") %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  tidy.daily.pos.gt.l1 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid1, salid1_name )) %>%  
    mutate(level = "L1") %>% 
    group_by(date, level,salid1, salid1_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Guatemala Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("GT","888"),
                           salid1))%>% ungroup() %>% 
    rename(loc = salid1_name) %>% 
    arrange(salid1, date) %>% 
    select(country,loc,salid = salid1, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    mutate(daily_positivity_pct =daily_cases/daily_tests*100 %>% round(2),
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L1") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  tidy.daily.tests.gt.l1 %>% 
    ggplot(aes(date, daily_counts, col = salid))+
    geom_line()+
    theme(legend.position = 'none')
  
  
  tidy.daily.tests.gt.l2 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid2, salid2_name, )) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Guatemala Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("GT","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name) %>% 
    arrange(salid2, date) %>% 
    select(country,loc,salid = salid2, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    left_join(pop_l2 %>% select(salid = loc, pop)) %>% 
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L2") %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  tidy.daily.pos.gt.l2 = raw_gt_mun %>% 
    left_join(select(xwalk_sal_gt,mun, salid2, salid2_name, )) %>%
    mutate(level = "L2") %>% 
    group_by(date,level, salid2, salid2_name, country) %>% 
    summarise(confirmed = sum(confirmed,na.rm = T),
              tests = sum(tests, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Guatemala Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("GT","888"),
                           salid2))%>% ungroup() %>% 
    rename(loc = salid2_name) %>% 
    arrange(salid2, date) %>% 
    select(country,loc,salid = salid2, date, confirmed, tests) %>% 
    group_by(country, loc, salid ) %>% 
    mutate(daily_cases = confirmed - lag(confirmed),
           rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>% 
    mutate(daily_tests = tests - lag(tests),
           rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>% 
    ungroup() %>%
    drop_na() %>% 
    mutate(daily_positivity_pct =daily_cases/daily_tests*100 %>% round(2),
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L2")  %>% 
    select(country, loc, salid, date, type,
           daily_counts = daily_tests, 
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>% 
    mutate(rate = ifelse(is.na(rate),0,rate))
  tidy.daily.tests.gt.l2 %>% 
    ggplot(aes(date, daily_counts, col = salid))+
    geom_line()+
    theme(legend.position = 'none')
  
  print("Okay")
})

# 9. Argentina  ####
try_AR = try({
  #___9.1  Get raw data  #####
  ## Raw BA Data
  ar_micro_BA_raw = fread("raw_files/casos_covid19.csv") %>%
    as_tibble() %>%
    filter(provincia== "CABA") %>%
    filter(clasificacion=="confirmado") %>%
    filter(!is.na(comuna)) %>%
    select(comuna,
           date = fecha_apertura_snvs,
           death=fallecido) %>%
    mutate(death = case_when(death == "si"~"SI",
                             is.na(death)~"NO",
                             TRUE~"NO")) %>%
    mutate(date = date %>% str_sub(1,9) %>% dmy()) %>%
    mutate(salid2_name = paste("Comuna",comuna)) %>%
    left_join(xwalk_sal_ar %>% select(salid2_name, mun)) %>%
    select(mun, date, death)
  
  
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
           death = fallecido) %>%
    mutate(prov = str_pad(prov,2, "left","0"),
           dept = str_pad(dept,3, "left","0"),
           mun = paste0(prov, dept),
           date = ymd(date)) %>%
    filter(type == "Confirmado") %>%
    filter(!mun%in%ar_micro_BA_raw$mun) %>%
    bind_rows(ar_micro_BA_raw) %>%
    filter(!is.na(date))
  
  ## Raw Cases (ar_mun_cases)
  ar_mun_cases_raw = ar_micro_raw %>%
    select(mun, date) %>%
    count(mun, date, name = "confirmed")
  min_date =  mdy("03-15-2020")
  max_date = max(ar_mun_cases_raw$date)
  ar_mun_cases = tibble(mun= list(unique(ar_mun_cases_raw$mun)),
                        date = seq(min_date,max_date, by = "day")) %>%
    unnest() %>%
    left_join(ar_mun_cases_raw) %>%
    mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>%
    group_by(mun) %>%
    group_modify(~.x %>%
                   arrange(date) %>%
                   mutate(confirmed = cumsum(confirmed))) %>%
    ungroup() %>%
    arrange(mun, date)
  # ar_mun_cases %>%
  #   filter(mun =="06056") %>%
  #   ggplot(aes(date, confirmed))+
  #   geom_line()
  ar_mun_cases_daily_tmp = tibble(mun= list(unique(ar_mun_cases_raw$mun)),
                                  date = seq(min_date,max_date, by = "day")) %>%
    unnest() %>%
    left_join(ar_mun_cases_raw) %>%
    mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>%
    arrange(mun, date)
  # ar_mun_cases_daily_tmp %>%
  #   ggplot(aes(date, confirmed, col = mun))+
  #   theme(legend.position = "none")+
  #   geom_line()
  
  ## Raw Testing (ar_mun_tests)
  ar_BA_testing  = fread("raw_files/casos_covid19.csv") %>%
    as_tibble() %>%
    filter(provincia== "CABA") %>%
    filter(clasificacion%in%c("confirmado","descartado")) %>%
    filter(!is.na(comuna)) %>%
    select(comuna,
           date = fecha_apertura_snvs) %>%
    mutate(date = date %>% str_sub(1,9) %>% dmy()) %>%
    mutate(salid2_name = paste("Comuna",comuna)) %>%
    left_join(xwalk_sal_ar %>% select(salid2_name, mun)) %>%
    count(mun, date, name = "tests") %>%
    arrange(mun, date) %>%
    # group_by(mun) %>%
    # group_modify(~.x %>%
    #                arrange(date) %>%
    #                mutate(tests = cumsum(tests))) %>%
    # ungroup() %>%
    left_join(ar_mun_cases_daily_tmp %>%
                rename(pos = confirmed) )%>%
    mutate(pos = ifelse(is.na(pos),0,pos))
  ar_BA_testing %>%
    filter(date > ymd("2020-08-29 ")) %>%
    arrange(date) %>%
    ggplot(aes(date, tests, col = mun))+geom_line()+
    theme(legend.position = 'none')
  ar_mun_tests = fread("raw_files/argentina_testing_tmp.csv") %>%
    as_tibble() %>%
    select(date = fecha,
           prov=codigo_indec_provincia,
           dept = codigo_indec_departamento,
           pos = positivos,
           tests = total) %>%
    mutate(prov = str_pad(prov,2, "left","0"),
           dept = str_pad(dept,3, "left","0"),
           mun = paste0(prov, dept),
           date = ymd(date),
           pos = ifelse(is.na(pos),0,pos)) %>%
    select(mun, date, tests, pos) %>%
    arrange(mun, date) %>%
    group_by(mun,date) %>%
    summarize(tests = sum(tests, na.rm = T),
              pos = sum(pos, na.rm = T)) %>%
    ungroup() %>%
    filter(!mun%in%ar_micro_BA_raw$mun) %>%
    bind_rows(ar_BA_testing)
  ar_mun_tests %>%
    filter(date > ymd("2020-08-29 ")) %>%
    arrange(date) %>%
    ggplot(aes(date, pos, col = mun))+geom_line()+
    theme(legend.position = 'none')
  
  # xwalk_sal_ar %>%
  #   filter(mun%in%xwalk_sal_ar$mun[!xwalk_sal_ar$mun%in%ar_mun_tests$mun])
  # xwalk_sal_ar %>%
  #   filter(salid1%in%c('101106','101124','101128'))
  ## Note: This testing file is missing some municipalities. For SALURBAL cities in Argentina,
  ## there are missing total testing numbers for '101106','101124','101128'. We will omit these cities
  ## from the positivity dataset due to missing/incomplete denominator (total tests).
  
  ## Raw Deaths (ar_mun_deaths)
  ar_mun_deaths_raw = ar_micro_raw %>%
    filter(death == "SI") %>%
    select(mun, date) %>%
    count(mun, date, name = "deaths")
  
  ar_mun_deaths = tibble(mun= list(unique(ar_mun_cases_raw$mun)),
                         date = seq(min_date,max_date, by = "day")) %>%
    unnest() %>%
    left_join(ar_mun_deaths_raw) %>%
    mutate(deaths = ifelse(is.na(deaths),0,deaths)) %>%
    group_by(mun) %>%
    group_modify(~.x %>%
                   arrange(date) %>%
                   mutate(deaths = cumsum(deaths))) %>%
    ungroup() %>%
    arrange(mun, date)
  
  
  ## Final Processed File (ar_mun_file)
  ar_mun_file_raw  = left_join(ar_mun_cases,ar_mun_deaths)%>%
    arrange(mun, date) %>%
    left_join(ar_mun_tests)
  min_date_tmp = mdy("03-15-2020")
  max_date_tmp = max(ar_mun_file_raw$date)
  ar_mun_file = tibble(mun = list(unique(ar_mun_file_raw$mun)),
                       date = seq(min_date_tmp,max_date_tmp, by = 'day') ) %>%
    unnest(cols = c(mun)) %>%
    arrange(mun, date) %>%
    left_join(ar_mun_file_raw, by = c('mun',"date")) %>%
    mutate(country = "Argentina")
  
  #___9.2 -  Cumulative ####
  full_ar_l1 = ar_mun_file  %>%
    left_join(xwalk_sal_ar) %>%
    mutate(level = "L1") %>%
    group_by(date, level,salid1, salid1_name, country) %>%
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
    left_join(pop_df %>% filter(level == "L1") %>% select(-level),
              by = c("salid1"="loc")) %>%
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>%
    rename(loc = salid1_name) %>%
    mutate(country = "Argentina")%>%
    arrange(salid1, date)
  
  full_ar_l2 = ar_mun_file  %>%
    left_join(xwalk_sal_ar) %>%
    mutate(level = "L2") %>%
    group_by(date, level,salid2, salid2_name, country) %>%
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
    left_join(pop_df %>% filter(level == "L2") %>% select(-level),
              by = c("salid2"="loc")) %>%
    mutate(confirmed_rate = round((confirmed/pop)*10^6,2),
           deaths_rate = round((deaths/pop)*10^7,2))   %>%
    rename(loc = salid2_name) %>%
    mutate(country = "Argentina")%>%
    arrange(salid2, date)
  
  #___9.3 -  Daily ####
  tidy.daily.ar.l1 = clean_daily_salurbal_smooth(full_ar_l1)%>% ungroup()%>% mutate(level = "L1")
  tidy.daily.ar.l2 = clean_daily_salurbal_smooth(full_ar_l2)%>% ungroup()%>% mutate(level = "L2")
  
  #___9.3 -  Daily Testing ####
  tidy.daily.tests.ar.l1 = ar_mun_tests %>%
    left_join(select(xwalk_sal_ar,mun, salid1, salid1_name,country ) %>%
                filter(!salid1%in%c('101106','101124','101128') ) ) %>%
    mutate(level = "L1") %>%
    group_by(date, level,salid1, salid1_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Argentina Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("AR","888"),
                           salid1))%>% ungroup() %>%
    rename(loc = salid1_name) %>%
    arrange(salid1, date) %>%
    select(country,loc,salid = salid1, date,
           daily_cases=confirmed,
           daily_tests=tests) %>%
    group_by(country, loc, salid ) %>%
    mutate(rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>%
    mutate(rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>%
    ungroup() %>%
    drop_na()%>%
    left_join(pop_l1 %>% select(salid = loc, pop)) %>%
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L1") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests,
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>%
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  tidy.daily.pos.ar.l1 = ar_mun_tests %>%
    left_join(select(xwalk_sal_ar,mun, salid1, salid1_name,country ) %>%
                filter(!salid1%in%c('101106','101124','101128') ) ) %>%
    mutate(level = "L1") %>%
    group_by(date, level,salid1, salid1_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Argentina Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("AR","888"),
                           salid1))%>% ungroup() %>%
    rename(loc = salid1_name) %>%
    arrange(salid1, date) %>%
    select(country,loc,salid = salid1, date,
           daily_cases=confirmed,
           daily_tests=tests) %>%
    group_by(country, loc, salid ) %>%
    mutate(rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>%
    mutate(rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(daily_positivity_pct = NA,
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L1") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests,
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>%
    mutate(rate = ifelse(is.na(rate),0,rate))
  tidy.daily.pos.ar.l1
  tidy.daily.tests.ar.l2 = ar_mun_tests %>%
    left_join(select(xwalk_sal_ar,mun, salid2, salid2_name,country ) %>%
                filter(!salid2%in%c('101106','101124','101128') ) ) %>%
    mutate(level = "L2") %>%
    group_by(date, level,salid2, salid2_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Argentina Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("AR","888"),
                           salid2))%>% ungroup() %>%
    rename(loc = salid2_name) %>%
    arrange(salid2, date) %>%
    select(country,loc,salid = salid2, date,
           daily_cases=confirmed,
           daily_tests=tests) %>%
    group_by(country, loc, salid ) %>%
    mutate(rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>%
    mutate(rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>%
    ungroup() %>%
    drop_na() %>%
    left_join(pop_l2 %>% select(salid = loc, pop)) %>%
    mutate(rate = (rollsum_tests/pop)*10^6,
           type = 'tests',
           smooth_days = 7,
           level = "L2") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests,
           rollsum = rollsum_tests,
           rate,
           smooth_days,
           level)%>%
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  tidy.daily.pos.ar.l2 = ar_mun_tests %>%
    left_join(select(xwalk_sal_ar,mun, salid2, salid2_name,country ) %>%
                filter(!salid2%in%c('101106','101124','101128') ) ) %>%
    mutate(level = "L2") %>%
    group_by(date, level,salid2, salid2_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Argentina Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("AR","888"),
                           salid2))%>% ungroup() %>%
    rename(loc = salid2_name) %>%
    arrange(salid2, date) %>%
    select(country,loc,salid = salid2, date,
           daily_cases=confirmed,
           daily_tests=tests) %>%
    group_by(country, loc, salid ) %>%
    mutate(rollsum_cases = rollmean(daily_cases, 7, align = "right", fill = NA)) %>%
    mutate(rollsum_tests = rollmean(daily_tests, 7, align = "right", fill = NA)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(daily_positivity_pct = NA,
           rollsum_daily_positivity_pct = rollsum_cases/rollsum_tests*100 %>% round(2),
           type = "positivity",
           smooth_days = 7,
           level = "L2") %>%
    select(country, loc, salid, date, type,
           daily_counts = daily_tests,
           rollsum = rollsum_tests,
           rate = rollsum_daily_positivity_pct,
           smooth_days,
           level)%>%
    mutate(rate = ifelse(is.na(rate),0,rate))
  
  print("Okay")
})


#### 10. Compile   ####
try_compile = try({
  #___10.1 -  full ####
  full_global =  list(full, 
                      full_br_state,
                      full_br_l1,
                      full_br_l2,
                      full_mx_state,
                      full_mx_l1,
                      full_mx_l2,
                      #full_co_state,
                      full_co_l1,
                      full_co_l2,
                      full_cl_state,
                      full_cl_l1,
                      full_cl_l2,
                      full_pe_state,
                      full_pe_l1,
                      full_pe_l2,
                      full_gt_l1,
                      full_gt_l2,
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
  
  
  
  
  
  #___10.2 -  Choices: top and rest  ####
  choices_df =full_global %>% 
    select(level, country,loc,  type, rate,date, n ) %>% 
    group_by(level, country,loc,  type, rate) %>% 
    group_modify(~.x %>% filter(date == max(date))) %>% 
    ungroup() %>% 
    # filter(level%in%c("L1")) %>%
    # filter(country%in%c("Guatemala")) %>%
    # filter(country=="Guatemala") %>%
    # filter(type == "cases", rate =="count") %>% 
    group_by(rate,country,level,type) %>% 
    group_modify(function(x,...){
      subset_non = x %>% filter(str_detect(loc, "Non")) 
      subset_tmp = x %>% filter(!str_detect(loc, "Non")) 
      #print(x$loc)
      if (nrow(subset_non)>0&nrow(subset_tmp)<11){
        br_non_sal_str=subset_non %>% pull(loc) %>% unique()
        df_tmp = x %>% filter(loc!=br_non_sal_str)
        n_tmp_top = nrow(df_tmp)
        top_1 = top10_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1) %>% pull(loc) %>% sort()
        top_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1:n_tmp_top) %>% pull(loc) %>% sort()
        tibble(top = c(rep("top",n_tmp_top),'non'),
               state = c(top_tmp,br_non_sal_str)) %>% 
          mutate(top1 = ifelse(state==top_1,"best","rest"))
      }
      else if (nrow(subset_non)>0){
        br_non_sal_str=subset_non %>% pull(loc) %>% unique()
        df_tmp = x %>% filter(loc!=br_non_sal_str)
        top_1 = top10_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1) %>% pull(loc) %>% sort()
        top10_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1:10) %>% pull(loc) %>% sort()
        rest_tmp = df_tmp %>% arrange(desc(n)) %>% slice(11:n()) %>% pull(loc) %>% sort()
        tibble(top = c(rep("top",10),rep("rest",length(rest_tmp)),'non'),
               state = c(top10_tmp,rest_tmp,br_non_sal_str)) %>% 
          mutate(top1 = ifelse(state==top_1,"best","rest"))
      }
      else {
        #print("else")
        df_tmp = x 
        top_1 = top10_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1) %>% pull(loc) %>% sort()
        top10_tmp = df_tmp %>% arrange(desc(n)) %>% slice(1:10) %>% pull(loc) %>% sort()
        rest_tmp = df_tmp %>% arrange(desc(n)) %>% slice(11:n()) %>% pull(loc) %>% sort()
        tibble(top = c(rep("top",10),rep("rest",length(rest_tmp))),
               state = c(top10_tmp,rest_tmp))  %>% 
          mutate(top1 = ifelse(state==top_1,"best","rest"))
      }
      #print("end")
    }) %>% 
    ungroup()
  
  
  
  
  
  #___10.3 - Map data at the L1 level  ####
  
  ## clean daily
  map_daily_data_salurbal_l1_tmp = full_global %>% 
    filter(level == "L1") %>% 
    mutate(date_label = paste0(lubridate::month(date,label=T, abbr = T),", ",day(date))) %>% 
    mutate(rate2 = paste0(type,"_",rate)) %>% 
    select(-rate) %>% 
    select(loc, salid1,country, date, n,rate=rate2, date_label) %>% 
    mutate(rate = str_remove(rate,"_count")) %>% mutate(loc2 = paste(loc,",",country))
  
  map_daily_data_salurbal_l1_tmp %>% 
    filter(country == "Brazil", rate =="deaths_rate", date == max(subset_dates_tmp))
  # library(ggmap);register_google(key='AIzaSyAq5p9yGsRT8J3ePZRWYtTL5-apfgYT3aw')
  # coords_google_l1 = map_daily_data_salurbal_l1_tmp %>%
  #   select(loc2) %>%
  #   distinct() %>%
  #   filter(!str_detect(loc2,"Non-salurbal")) %>%
  #   mutate_geocode(loc2) %>%
  #   rename(lng = lon)
  # save(coords_google_l1, file = "coords_google_l1.csv")
  
  load("coords_google_l1.csv")
  all_dates_tmp = map_daily_data_salurbal_l1_tmp %>% 
    select(date, date_label) %>% 
    distinct() %>% 
    arrange(date)
  
  
  map_daily_data_salurbal_l1_tmp2 = map_daily_data_salurbal_l1_tmp %>% 
    select(loc, salid1, country, rate, loc2) %>% 
    distinct() %>%
    mutate(date =list(all_dates_tmp)) %>% 
    unnest(cols = c(date))  %>% 
    left_join(map_daily_data_salurbal_l1_tmp) %>% 
    group_by(loc, salid1, country, rate, loc2) %>% 
    group_modify(function(x,...){
      first_date = x %>% filter(!is.na(n)) %>% pull(date) %>% min()
      x %>% 
        filter(date>=first_date) %>% 
        arrange(date) %>% 
        fill(n, .direction = "up") %>% 
        fill(n, .direction = "down")
    }) %>% 
    ungroup() %>% 
    left_join(coords_google_l1) %>% 
    filter(!is.na(lng)) %>% 
    mutate(rate = str_replace(rate,"cases","confirmed")) %>% 
    filter(date%in%subset_dates_tmp)
  map_daily_data_salurbal_l1_tmp2 %>% 
    filter(country == "Brazil", rate =="deaths_rate", date == max(subset_dates_tmp)) 
  min_radius = 2
  map_daily_salurbal_radius = map_daily_data_salurbal_l1_tmp2 %>% 
    filter(date == max(date))%>% 
    group_by(date,rate) %>% 
    top_n(1,n) %>% 
    mutate(max_radius_tmp = ifelse(str_detect(rate,"rate"),
                                   20,
                                   20)) %>% 
    mutate(radius_factor = log(n)/log(max_radius_tmp)) %>% 
    ungroup() %>% 
    select(rate,radius_factor )
  
  map_daily_data_salurbal_l1 = map_daily_data_salurbal_l1_tmp2 %>% 
    filter(n>0) %>% 
    mutate(log_n = log10(n))%>% 
    left_join(map_daily_salurbal_radius) %>% 
    mutate(radius = (n)^(1/radius_factor)) %>% 
    mutate(radius =ifelse(radius<min_radius,min_radius,radius)) 
  # map_daily_data_salurbal_l1 %>% 
  #   filter(country == "Brazil", rate =="deaths_rate", date == max(subset_dates_tmp)) 
  df_map_data_raw = map_daily_data %>% 
    mutate(level = "country") %>% 
    bind_rows(
      map_daily_data_salurbal_l1 %>% 
        mutate(level = "L1")
    ) %>% 
    filter(loc!="Qatar")
  # df_map_data_raw %>% 
  #   filter(level == "L1",country == "Brazil", rate =="deaths_rate", date == max(subset_dates_tmp)) 
  # .x = df_map_data %>% 
  #   filter(level =="country", rate == "confirmed_rate", date == max(df_map_data$date) )
  df_map_data=df_map_data_raw %>% 
    mutate(rate2 = ifelse(str_detect(rate,"rate"),"rate","count")) %>% 
    group_by(level, rate, date) %>% 
    group_modify(~{
      if (unique(.x$rate2)=="count"){.x %>% mutate(fill = NA)}
      else {
        sf_tmp = .x
        pal = colorNumeric("OrRd",
                           domain = sf_tmp$n, 
                           reverse = F)
        .x %>% mutate(fill = pal(n))
      }
    }) %>% 
    ungroup() %>% 
    select(-c(rate2,pop,radius_factor,log_n,loc2)) %>% 
    arrange(level, loc,rate)
  
  
  #___10.4 - Shape Files  ####
  
  library("rnaturalearth")
  library("rnaturalearthdata")
  library(rmapshaper)
  
  sf_world_raw <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    select(loc = name) %>% 
    mutate(loc = case_when(
      loc == "Antigua and Barb."~"Antigua and Barbuda",
      loc == "Cape Verde"~"Cabo Verde",
      loc == "Korea"~"Korea, South",
      loc == "Dominican Rep."~"Dominican Republic",
      loc == "Taiwan"~"Taiwan*",
      loc == "United States"~"US",
      TRUE~loc
    ))
  
  sf_world = sf_world_raw %>% ms_simplify(keep= 0.05)
  
  load("salurbal_l1_sf.rdata")
  salurbal_l1_sf= ms_simplify(salurbal_l1_sf, keep = 0.05) %>% 
    mutate(country = ifelse(country == "Brasil","Brazil",country))
  
  
  load("sf_salurbal_0.8.rdata")
  sf_salurbal_0.8 = sf_salurbal_0.8 %>% 
    mutate(country = ifelse(country == "Brasil","Brazil",country))
  print("Okay")
})
#### 11. Saving Rdata (covid19_processed_data.rdata) ####
{
  #### ___11.1 -  tidy.daily  ####
  tidy.daily.subnational = list(tidy.daily.br.state,
                                tidy.daily.br.l1 %>% filter(loc!= "Vitoria"),
                                tidy.daily.br.l2,
                                tidy.daily.mx.state,
                                tidy.daily.mx.l1,
                                tidy.daily.mx.l2,
                                #tidy.daily.co.state,
                                tidy.daily.co.l1,
                                tidy.daily.co.l2,
                                tidy.daily.cl.state,
                                tidy.daily.pe.state,
                                tidy.daily.pe.l1,
                                tidy.daily.pe.l2,
                                tidy.daily.gt.l1,
                                tidy.daily.gt.l2,
                                tidy.daily.ar.l1,
                                tidy.daily.ar.l2
  ) %>% bind_rows() %>% 
    filter(smooth_days == 7) %>% 
    bind_rows(
      bind_rows(tidy.daily.cl.l1,
                tidy.daily.cl.l2) 
    ) %>% 
    mutate(rate = round(rate,0)) %>% 
    bind_rows(tidy.daily.tests.mx.l1,
              tidy.daily.pos.mx.l1,
              tidy.daily.tests.mx.l2,
              tidy.daily.pos.mx.l2,
              tidy.daily.tests.gt.l1,
              tidy.daily.pos.gt.l1,
              tidy.daily.tests.gt.l2,
              tidy.daily.pos.gt.l2,
              tidy.daily.tests.ar.l1,
              tidy.daily.pos.ar.l1,
              tidy.daily.tests.ar.l2,
              tidy.daily.pos.ar.l2
              # tidy.daily.tests.cl.l1,
              # tidy.daily.tests.cl.l2,
    ) 
  
  tidy.daily = tidy.daily.country %>% 
    select(level,country, type, loc, date,rollsum, rate,start, end) %>% 
    bind_rows(tidy.daily.subnational %>% 
                select(level,country, type, loc, salid,date, rollsum, rate)) %>% 
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
    select(-start, -end) %>% 
    filter(!is.na(value)) %>% 
    mutate(type = ifelse(type =="confirmed","cases",type)) %>% 
    mutate(value = ifelse(value<0,0,value))
  
  tidy.daily %>% 
    filter(type == "positivity",level == "L1", country == "Argentina") %>% 
    filter(rate == "rate") %>% 
    pull(value) %>% max()
  
  tidy.daily.data = tidy.daily %>% 
    select(level, country, type, rate, salid, loc, date, value) %>% 
    filter(level%in%c("L1","L2"))
  
  
  
  #### ___11.2 - tidy.cumulative   ####
  dfa = tidy.daily.country %>% 
    select(level,country, type, loc, date,daily_counts) %>% 
    bind_rows(tidy.daily.subnational %>% 
                select(level,country, type, loc, salid,date, daily_counts)) %>% 
    mutate(type = type %>% recode("confirmed"="cases")) %>% 
    group_by(level, country, loc, salid,type) %>% 
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
    left_join(pop_df %>% rename(loc2 = loc)) %>% 
    filter(!is.na(pop)) %>% 
    select(-loc2, -daily_counts) %>% 
    mutate(rate = case_when(
      type%in%c("cases", "tests")~(cum_value/pop)*10^6,
      type%in%c("deaths")~(cum_value/pop)*10^7
    )) %>% 
    select(-pop)
  
  df2_positivity = dfa %>%   
    filter(type=="positivity") %>%
    filter(country!= "Argentina") %>% 
    arrange(salid, date) %>% 
    left_join(
      dfa %>% 
        filter(type == "cases") %>% 
        filter(level%in%c("L1","L2")) %>% 
        select(salid, date, daily_cases=daily_counts) %>% 
        distinct() ,
      by = c("salid","date")
    ) %>% 
    group_by(level, country, loc, salid,type) %>% 
    group_modify(~.x %>% 
                   arrange(date) %>% 
                   mutate(cum_cases= cumsum(daily_cases) ) ) %>% 
    ungroup() %>% 
    mutate(rate = round((cum_cases/cum_value)*100,2)) %>% 
    select(level, country, loc, salid, type, date, cum_value, rate) %>% 
    drop_na()
  ## Argentina cumlative has to be done manually from raw data, because the data for testing is different from the actual cases/deaths
  df2_positivity_Argentina_l1 = ar_mun_tests %>%
    left_join(select(xwalk_sal_ar,mun, salid1, salid1_name,country ) %>%
                filter(!salid1%in%c('101106','101124','101128') ) ) %>%
    mutate(level = "L1") %>%
    group_by(date, level,salid1, salid1_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid1_name = ifelse(is.na(salid1),
                                "Argentina Non-salurbal",
                                salid1_name),
           salid1 = ifelse(is.na(salid1),
                           paste0("AR","888"),
                           salid1))%>% ungroup() %>%
    rename(loc = salid1_name) %>%
    arrange(salid1, date) %>%
    select(country,loc,salid = salid1, date,
           daily_cases=confirmed,
           daily_counts=tests) %>%
    mutate(level = "L1", type = "positivity") %>%
    group_by(level, country, loc, salid,type) %>%
    group_modify(~.x %>%
                   arrange(date) %>%
                   mutate(cum_cases= cumsum(daily_cases) ,
                          cum_value = cumsum(daily_counts)) ) %>%
    ungroup()%>%
    mutate(rate = round((cum_cases/cum_value)*100,2)) %>%
    select(level, country, loc, salid, type, date, cum_value, rate) %>%
    drop_na()
  df2_positivity_Argentina_l2 = ar_mun_tests %>%
    left_join(
      select(xwalk_sal_ar,mun, salid1, salid2, salid2_name,country ) %>%
        filter(!salid1%in%c('101106','101124','101128')) %>%
        select(-salid1)
    ) %>%
    mutate(level = "L2") %>%
    group_by(date, level,salid2, salid2_name, country) %>%
    summarise(confirmed = sum(pos,na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    ungroup() %>%
    mutate(salid2_name = ifelse(is.na(salid2),
                                "Argentina Non-salurbal",
                                salid2_name),
           salid2 = ifelse(is.na(salid2),
                           paste0("AR","888"),
                           salid2))%>% ungroup() %>%
    rename(loc = salid2_name) %>%
    arrange(salid2, date) %>%
    select(country,loc,salid = salid2, date,
           daily_cases=confirmed,
           daily_counts=tests) %>%
    mutate(level = "L2", type = "positivity") %>%
    group_by(level, country, loc, salid,type) %>%
    group_modify(~.x %>%
                   arrange(date) %>%
                   mutate(cum_cases= cumsum(daily_cases) ,
                          cum_value = cumsum(daily_counts)) ) %>%
    ungroup() %>%
    mutate(rate = round((cum_cases/cum_value)*100,2)) %>%
    select(level, country, loc, salid, type, date, cum_value, rate) %>%
    drop_na()
  
  df2 = bind_rows(df2_cases_tests_deaths,
                  df2_positivity,
                  df2_positivity_Argentina_l1,
                  df2_positivity_Argentina_l2
  )
  
  
  
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
    select(level, country,type, rate, salid, loc, date, value = n)%>% 
    filter(level%in%c("L1","L2"))
  
  #### ___11.4 - App Data ####
  ## Subset to every 3 days 
  n_days = 3
  tidy.data.all = tidy.daily   %>% 
    select(level, country, 
           loc, date, 
           type,rate, 
           daily_value = value) %>% 
    left_join(
      tidy.cumulative %>% 
        select(level, country, 
               loc, date, 
               type,rate, 
               cum_value = n)
    ) %>% 
    mutate(type_rate = paste(type,rate))  %>% 
    select(level, country, 
           loc, date, 
           type_rate, 
           daily_value,
           cum_value) %>% 
    group_by(level, country ) %>% 
    group_modify(~{
      dates_tmp = .x %>% pull(date) %>% unique()
      max_date_tmp = max(dates_tmp)
      min_date_tmp = min(dates_tmp)
      n_weeks_tmp = floor(length(dates_tmp)/n_days)
      subset_dates = c(max_date_tmp,max_date_tmp-((1:n_weeks_tmp)*n_days))
      .x %>% 
        filter(date%in%subset_dates)
    }) %>% 
    ungroup() %>% 
    mutate_at(vars(daily_value),~round(.x,0)) %>% 
    mutate(date = as.integer(date))
  
  tidy.data.all %>% 
    filter(str_detect(type_rate,"positivity")) %>% 
    count(country,is.na(cum_value))
  tidy.data.all %>% 
    filter(str_detect(type_rate,"positivity")) %>%
    filter(country == "Guatemala") %>% 
    arrange(loc, type_rate, date) 
  
  
  ##make cross walks
  
  xwalk_data_rate = tidy.daily %>% 
    count(rate, type) %>% 
    select(-n) %>% 
    mutate(type_rate = paste(type,rate))
  xwalk_data_rate_cleaned = tidy.daily %>% 
    count(level,  type, rate, ylabs, rate_cleaned) %>% 
    select(-n)
  xwalk_data_cum_rate_cleaned = tidy.cumulative %>% 
    count(level,  type, rate, rate_cleaned) %>% 
    select(-n)
  xwalk_data_titles = tidy.daily %>% 
    count(level, country, type, rate, title) %>% 
    select(-n)
  xwalk_salid = tidy.daily %>% 
    filter(level%in%c("L1","L2")) %>% 
    select(level,country, loc, salid) %>% 
    distinct()
  
  ### Subset to old (Before Cutoff) and new (Cutoff and after); Cutoff: Dec 15, 2020
  tidy.data.all.old = tidy.data.all %>% 
    filter(date<as.integer(mdy(cutoff_date)))
  
  tidy.data.all.new = tidy.data.all %>% 
    filter(date>=as.integer(mdy(cutoff_date)))
  
  #### ___11.5 - Save Image ####
  save.image(file = "raw_files/space.RData")
  print("Okay")
  
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
      select(level,country, type, rate, salid, loc, date,value = daily_counts )
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
      select(level,country, salid, loc, type, date, rate_7dayMA = value,rate_label )
    tidy.daily.data.output_1 = tidy.daily.data %>%
      bind_rows(raw.daily.subnational) %>%
      filter(rate !="rate") %>%
      filter(!type%in%c( "positivity","tests")) %>%
      rename(count_smoothed = rate) %>%
      mutate(count_smoothed = count_smoothed %>%
               recode("count"="count_7dayMA",
                      "raw count"="count_raw")) %>%
      pivot_wider(names_from = count_smoothed, values_from = value) %>%
      left_join(bind_rows(pop_l1,pop_l2) %>% select(salid = loc, pop)) %>%
      select(level, country, salid,loc, type, date,count_raw,count_7dayMA)%>%
      arrange(level, country, salid, loc, type,date) %>%
      mutate_at(vars(count_raw, count_7dayMA), ~ifelse(.x<0,NA,.x))  %>%
      left_join(raw.daily.subnational.rates %>%
                  select(level, salid, type, date,rate_7dayMA, rate_label ),
                by =c("level", "salid","type", "date")) %>% 
      left_join(xwalk_countries) %>% 
      select(country= iso2, level, salid, type, date, 
             count_raw, count_7dayMA,
             rate_7dayMA, rate_label) %>% 
      distinct()
    
    tidy.daily.data.output = tidy.daily.data.output_1 %>% 
      select(-rate_label) %>% 
      pivot_wider(names_from = type, values_from = c(count_raw, count_7dayMA, rate_7dayMA)) %>% 
      select(country, level, salid, date, 
             cases_raw = count_raw_cases,
             cases_7dayMA = count_7dayMA_cases,
             cases_per_1M = rate_7dayMA_cases, 
             deaths_raw = count_raw_deaths,
             deaths_7dayMA = count_7dayMA_deaths,
             deaths_per_10M = rate_7dayMA_deaths) %>% 
      mutate_at(vars(cases_raw,cases_7dayMA,cases_per_1M,deaths_raw,deaths_7dayMA,deaths_per_10M),
                ~round(.x,0))
    
  }
  
  # ___12.2 trends_tests_positivity.csv -----
  {
    tidy.daily.testing.output = tidy.daily.data %>% 
      filter(type%in%c( "positivity","tests")) %>% 
      pivot_wider(values_from = value, names_from = c(type,rate))%>% 
      left_join(xwalk_countries) %>% 
      select(-loc)%>% 
      distinct() %>% 
      select(country = iso2, level, salid, date, 
             tests = tests_count,
             tests_per_1M = tests_rate,
             pct_positive = positivity_rate) %>% 
      mutate_at(vars(tests,tests_per_1M,pct_positive),
                ~round(.x,0))
    
  }
  
  # ___12.3 cumulative_cases_death.csv -----
  {
    tidy.cumulative.output =  tidy.cumulative %>%
      filter(!type%in%c("positivity","tests")) %>% 
      filter(level%in%c("L1","L2")) %>% 
      select(level, country, salid,type, rate, rate_cleaned,date, n) %>% 
      group_by(level, country, salid,type, rate, rate_cleaned) %>% 
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
      select(level, country = iso2, salid,rate_cleaned, n) %>% 
      pivot_wider(names_from = rate_cleaned, values_from = n) %>% 
      drop_na()
  }
  
  # ___12.4 cumulative_tests_positivity.csv -----
  {
    tidy.cumulative.testing.output =  tidy.cumulative %>%
      filter(type%in%c("positivity","tests")) %>% 
      filter(level%in%c("L1","L2")) %>% 
      select(level, country, salid,type, rate, rate_cleaned,date, n) %>% 
      group_by(level, country, salid,type, rate, rate_cleaned) %>% 
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
      select(level, country = iso2, salid,rate_cleaned, n) %>% 
      pivot_wider(names_from = rate_cleaned, values_from = n) %>% 
      drop_na()
  }
  
  {
    ## Trends in cases/deaths
    fwrite(tidy.daily.data.output,"../Outputs/SALURBAL_COVID19_trends_cases_death.csv")
    zip::zip(files= c("../Outputs/SALURBAL_COVID19_trends_cases_death.csv"),
             zipfile = "../Outputs/SALURBAL_COVID19_trends_cases_death.zip",
             mode = "cherry-pick")
    ## Trends in testing
    fwrite(tidy.daily.testing.output,"../Outputs/SALURBAL_COVID19_trends_tests_positivity.csv")
    zip::zip(files ="../Outputs/SALURBAL_COVID19_trends_tests_positivity.csv",
             zipfile = "../Outputs/SALURBAL_COVID19_trends_tests_positivity.zip",
             mode = "cherry-pick")
    ## Cumulative outputs
    fwrite(tidy.cumulative.output,"../Outputs/SALURBAL_COVID19_cumulative_cases_death.csv")
    fwrite(tidy.cumulative.testing.output,"../Outputs/SALURBAL_COVID19_cumulative_tests_positivity.csv")
    
  }
}


