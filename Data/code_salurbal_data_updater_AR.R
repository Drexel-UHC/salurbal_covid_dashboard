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
}


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
           between(edad,0,140))%>% 
    mutate( 
      age_group = cut(edad, 
                      breaks=seq(0,140,10), 
                      labels = paste0(seq(1,136,10),"-",seq(10,140,10)),
                      right = T ) %>% as.character(),
      age_group = case_when(
        edad <11~"0-10",
        edad >70~"70+",
        TRUE~age_group
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
    mutate(country = "Argentina")
  
  #___9.2 -  Cumulative ####
  full_ar_l1 = ar_mun_file  %>%
    group_by(age_group) %>% 
    group_modify(~.x %>% 
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
                   ) %>% 
    ungroup()
   
  
  full_ar_l2 = ar_mun_file  %>%
    group_by(age_group) %>% 
    group_modify(~.x %>% 
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
                   arrange(salid2, date)) %>% 
    ungroup()
                   
    
  
  #___9.3 -  Daily ####
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
    ungroup()
  
  
})


#### 10. Compile   ####
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
    filter(level%in%c("L1","L2"))
  
  
  
  #### ___11.2 - tidy.cumulative   ####
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
    left_join(pop_df %>% rename(loc2 = loc)) %>% 
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
    filter(level%in%c("L1","L2"))
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
      left_join(bind_rows(pop_l1,pop_l2) %>% select(salid = loc, pop)) %>%
      select(level, country, salid,loc, age_group, type, date,count_raw,count_7dayMA)%>%
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


