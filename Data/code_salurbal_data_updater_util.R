rm(list=ls())
library(git2r)
library(passport)
library(httr)
library(cowplot)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(zoo)
library(stringr)
library(lubridate)
library(sas7bdat)
library(janitor)
library(scales)
library(gridExtra)
library(DT)
library(sparkline)
library(lubridate)
library(ggrepel)
library(readxl)
library(googlesheets4)
library(RColorBrewer)
library(tidyverse) 

#### 0. Global Variables ####

#### 0.1 - LAC Countries #####
load("helper_files/lac_country_name_csse_xwalk.rdata")
countries_salurbal<-c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica",
                      "Mexico", "Peru", "Panama", "Nicaragua","Guatemala", "El Salvador")
lac_pais = read_excel("helper_files/paises_lac.xlsx") %>% 
  filter(!country%in%countries_salurbal) %>% 
  left_join(lac_country_name_xwalk) %>% 
  filter(!((!is.na(missing))&is.na(loc))) %>% 
  mutate(country = ifelse(!is.na(missing),loc,country)) %>% 
  select(area, country) %>% 
  filter(country!="Cayman Islands")

other_central_countries = lac_pais %>% filter(area == "central") %>% pull(country)
other_south_countries = lac_pais %>% filter(area == "south") %>% pull(country)
other_carribean_countries = lac_pais %>% filter(area == "caribbean") %>% pull(country)
other_lac_countries = c(other_central_countries,other_south_countries,other_carribean_countries)
lac_countries = c(countries_salurbal,other_lac_countries)

## Check other lAC country names via REGEX with CSSE data (check caymen islands)
# csse_loc = counts %>% select(loc) %>% distinct()%>%
#   mutate(iso2 = parse_country(loc,to = "iso2c"))
# lac_country_name_xwalk = lac_pais %>% filter(missing == "check") %>%
#   mutate(iso2 = parse_country(country,to = "iso2c")) %>%
#   filter(!is.na(iso2)) %>%
#   left_join(csse_loc) %>%
#   select(area, country, loc)
# save(lac_country_name_xwalk, file = "lac_country_name_csse_xwalk.rdata")

##### 0.2 - Lock down dates ######
load("helper_files/lock.dates.state.rdata")
load("helper_files/lock_dates_country.rdata")
lock.dates.state = lock.dates.state %>% 
  mutate_at(vars(place,loc,level),~stringi::stri_trans_general(.x, "Latin-ASCII") %>% str_trim()) %>% 
  mutate_at(vars(start, end),~.x %>% str_sub(1,10) %>% ymd()) %>% 
  rename(state = place)
lock.dates.country = lock.dates.country %>% 
  filter(!loc%in%c("Chile",
                   "Costa Rica",
                   "Panama")) %>% 
  mutate(end =case_when(
    loc == "Colombia"~end+48,
    loc == "Peru"~end+65,
    TRUE~end
  ))


##### 0.3 - SALURBAL Crosswalks  ######

## L1 names
sal_l1_names = read.sas7bdat("../../SALURBAL Covid19 Files/l1names_11_3_17.sas7bdat") %>% as_tibble() %>% 
  clean_names() %>% 
  select(salid1, salid1_name = level1_name1, country = country_name ) %>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII"))

## Brazil
xwalk_sal_br_raw = read.sas7bdat("../../SALURBAL Covid19 Files/br_salurbal_l2_11_29_18.sas7bdat") %>% as_tibble() %>% 
  clean_names() %>% 
  select(mun = local_id10, salid1, salid2,salid2_name = localname1, state_name) %>% 
  filter(!is.na(salid1)) %>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  left_join(sal_l1_names) %>% 
  mutate(iso2c = "BR") 
br_redundant_names_l2 = xwalk_sal_br_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_br = xwalk_sal_br_raw  %>% 
  mutate(salid2_name = ifelse(salid2%in%br_redundant_names_l2, 
                              paste0(salid2_name,",",state_name),
                              salid2_name))
## Mexico
xwalk_sal_mx_raw = read.sas7bdat("../../SALURBAL Covid19 Files/mx_salurbal_l2_1_4_19.sas7bdat")%>% as_tibble() %>% 
  clean_names() %>% 
  select(mun = l2local_id, salid1, salid2, 
         salid2_name = localname1,adm1_name)%>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  left_join(sal_l1_names) %>% 
  mutate(iso2c = "MX") 
mx_redundant_names_l1 = xwalk_sal_mx_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
mx_redundant_names_l2 = xwalk_sal_mx_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_mx = xwalk_sal_mx_raw  %>% 
  mutate(salid2_name = ifelse(salid2%in%mx_redundant_names_l2, 
                              paste0(salid2_name,",",adm1_name),
                              salid2_name))


## Chile
xwalk_state_cl = read.sas7bdat("../../SALURBAL Covid19 Files/cl_salurbal_l2_12_3_18.sas7bdat")%>% as_tibble() %>% 
  clean_names() %>% 
  select(state = regionname16, state_id = region16 ) %>% distinct() %>%
  mutate_all(~as.character(.x)) %>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  mutate(state = ifelse(state == "REGION METROPOLITANA DE SANTIAGO",
                        "METROPOLITANA DE SANTIAGO",
                        str_sub(state, 11, -1L)) %>% str_trim())%>% 
  mutate(state_id = str_pad(state_id, width = 2, pad = "0"))

xwalk_sal_cl_raw = read.sas7bdat("../../SALURBAL Covid19 Files/cl_salurbal_l2_12_3_18.sas7bdat")%>% as_tibble() %>% 
  clean_names() %>% 
  select(mun = local_id18, salid1, salid2, 
         salid2_name = comunname,salid1_name = l1name)%>% 
  filter(!is.na(salid1)) %>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  mutate(iso2c = "CL",
         country = "Chile") %>% 
  mutate(mun = str_pad(mun, width = 5, pad = "0"))

cl_redundant_names_l1 = xwalk_sal_cl_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
cl_redundant_names_l2 = xwalk_sal_cl_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_cl = xwalk_sal_cl_raw %>% 
  mutate(salid2_name = str_to_lower(salid2_name)) %>% 
  mutate(salid2_name =  paste0(toupper(substr(salid2_name, 1, 1)), substr(salid2_name, 2, nchar(salid2_name))))

## Colombia

xwalk_sal_co_raw = read.sas7bdat("../../SALURBAL Covid19 Files/co_salurbal_l2_1_8_19.sas7bdat")%>% as_tibble() %>% 
  clean_names() %>% 
  select(mun = mpio_ccnct, salid1, salid2, 
         salid2_name = mpio_cnmbr)%>% 
  filter(!is.na(salid1)) %>% mutate_all(~as.character(.x)) %>% 
  left_join(sal_l1_names) %>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  mutate(iso2c = "CO",
         country = "Colombia") %>% 
  mutate(mun = str_pad(mun, width = 5, pad = "0"))

co_redundant_names_l1 = xwalk_sal_co_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
co_redundant_names_l2 = xwalk_sal_co_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_co = xwalk_sal_co_raw %>% 
  mutate(salid2_name = str_to_lower(salid2_name)) %>% 
  mutate(salid2_name =  paste0(toupper(substr(salid2_name, 1, 1)), substr(salid2_name, 2, nchar(salid2_name))))


##  Peru
to_upper_all = function(x){
  s =  str_split(x, " ")[[1]]
  paste0(ifelse(s=="y",
                "y",
                str_to_upper(str_sub(s,1,1))),
         str_sub(s,2), collapse = " ")
}
to_upper_all("jose luis bustamante y rivero")
to_upper_all("yrivero")

xwalk_sal_pe_raw = read.sas7bdat("../../SALURBAL Covid19 Files/pe_salurbal_l2_1_4_19.sas7bdat")%>% as_tibble() %>% 
  clean_names() %>% 
  select(mun = l2local_id, salid1, salid2, 
         salid2_name = nombdist) %>% 
  filter(!is.na(salid1)) %>% mutate_all(~as.character(.x)) %>% 
  left_join(sal_l1_names) %>% 
  mutate_all(~as.character(.x))%>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  mutate(iso2c = "PE") %>% 
  mutate(mun = str_pad(mun, width = 6, pad = "0")) %>%
  group_by(salid2) %>% 
  mutate(salid2_name = str_to_lower(salid2_name) %>% to_upper_all()) %>% 
  ungroup()

redundant_names_l1 = xwalk_sal_pe_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
redundant_names_l2 = xwalk_sal_pe_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_pe = xwalk_sal_pe_raw %>% 
  mutate(salid2_name = ifelse(salid2%in%redundant_names_l2, 
                              paste0(salid2_name,", ",salid1_name),
                              salid2_name))


##  Argentina
library(rgdal)
library(sf)
sf_l2 = readOGR("../../SALURBAL Covid19 Files/Spatial Files/L2/SALURBAL_L2_9_16_19.shp")
xwalk_l2_name = sf::st_as_sf(sf_l2) %>% 
  select(salid1 = SALID1,
         salid2 = SALID2,
         l1_name=Adm1Name,
         l2_name= L2Name) %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  group_by(salid1,salid2) %>% 
  mutate_at(vars(l1_name,l2_name),~.x %>% 
              stringi::stri_trans_general(., "Latin-ASCII") %>% 
              tolower() %>% 
              to_upper_all()) %>% 
  ungroup() %>% 
  select(salid2,salid2_name= l2_name)
xwalk_sal_ar_raw = read.sas7bdat("../../SALURBAL Covid19 Files/arfc_salurbal_3_25_20.sas7bdat") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(mun = str_sub(l2_5localid,1,5)) %>% 
  select(mun, 
         salid1, 
         salid2) %>% 
  filter(!is.na(salid1)) %>% mutate_all(~as.character(.x)) %>% 
  left_join(sal_l1_names, by = "salid1") %>% 
  left_join(xwalk_l2_name, by = "salid2") %>% 
  mutate_all(~as.character(.x))%>% 
  mutate(iso2c = "AR",
         country = "Argentina") %>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  distinct()
ar_redundant_names_l1 = xwalk_sal_ar_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
ar_redundant_names_l2 = xwalk_sal_ar_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_ar = xwalk_sal_ar_raw %>% 
  mutate(salid2_name = ifelse(salid2%in%ar_redundant_names_l2, 
                              paste0(salid2_name,", ",salid1_name),
                              salid2_name)) %>% 
  select(mun, salid1, salid2, salid1_name, salid2_name, country)

##  Guatemala
xwalk_sal_gt_raw = read.sas7bdat("../../SALURBAL Covid19 Files/gt_salurbal_l2_12_5_18.sas7bdat") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(mun = local_id11 , 
         salid1, 
         salid2) %>% 
  filter(!is.na(salid1)) %>% mutate_all(~as.character(.x)) %>% 
  left_join(sal_l1_names, by = "salid1") %>% 
  left_join(xwalk_l2_name, by = "salid2") %>% 
  mutate_all(~as.character(.x)) %>% 
  mutate(iso2c = "GT",
         country = "Guatemala") %>% 
  mutate_if(is.character,~stringi::stri_trans_general(.x, "Latin-ASCII")) %>% 
  mutate(mun = str_pad(mun, width =4, pad =  "0"))

gt_redundant_names_l1 = xwalk_sal_gt_raw %>% select(salid1, salid1_name) %>%
  distinct() %>% add_count(salid1_name) %>% filter(n>1) %>% pull(salid1)
gt_redundant_names_l2 = xwalk_sal_gt_raw %>% select(salid2, salid2_name) %>%
  distinct() %>% add_count(salid2_name) %>% filter(n>1) %>% pull(salid2)
xwalk_sal_gt= xwalk_sal_gt_raw %>% 
  mutate(salid2_name = ifelse(salid2%in%gt_redundant_names_l2, 
                              paste0(salid2_name,", ",salid1_name),
                              salid2_name))

## SALID0
##
xwalk_salid0 = sal_l1_names %>% 
  mutate(salid0 = str_sub(salid1, 1,3)) %>% 
  select(salid0, country) %>% 
  distinct() %>% 
  mutate(country = ifelse(country=="Brasil","Brazil",country))
##### 0.4 - Others ######
minc = 100
mind = 10 
minr = 1
col_rolling = c("#1b9e77", "#d95f02")

##### 0.5 -  Population ######
## Country Level
pop_country = read_excel("helper_files/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                         sheet = 1,
                         skip = 16 ) %>% as_tibble() %>% 
  clean_names() %>% 
  select(type, 
         country = region_subregion_country_or_area,
         x2020 ) %>% 
  filter(type == "Country/Area") %>% 
  mutate(pop = parse_number(x2020)*1000) %>% 
  select(loc = country, pop) %>% 
  mutate(level = "country")

## L1 and L2 Level
pop_sal_raw = list.files(path = "../../SALURBAL Covid19 Files/",
                         pattern = "L2", full.names = T) %>%
  map_df(~fread(.x)) %>% 
  clean_names() %>%
  group_by(iso2) %>% 
  
  filter(year == max(year)) %>% as_tibble() %>% 
  ungroup() %>% 
  select(-year) %>% 
  mutate_at(vars(salid1, salid2),~as.character(.x))

pop_l1 = pop_sal_raw %>% 
  group_by(iso2,salid1) %>% 
  summarise(pop = sum(prjl2pop)) %>% ungroup() %>% 
  mutate(level = "L1") %>% 
  rename(loc = salid1)

pop_l2 = pop_sal_raw %>% group_by(iso2,salid2) %>% 
  summarise(pop = sum(prjl2pop)) %>% ungroup() %>% 
  mutate(level = "L2") %>% 
  rename(loc = salid2)

pop888 = pop_l1 %>% mutate(salid0 = str_sub(loc,1,3)) %>% 
  group_by(iso2,salid0) %>% 
  summarise(pop_sal = sum(pop)) %>% 
  ungroup() %>% 
  left_join(xwalk_salid0 ) %>% 
  mutate(country = ifelse(iso2=="PA","Panama",country)) %>% 
  left_join(pop_country, by  =c("country"="loc")) %>% 
  mutate(loc = paste0(iso2,"888"),
         pop = pop - pop_sal) %>% 
  select(loc, pop) %>% 
  nest() %>% bind_rows(.,.) %>% 
  mutate(level = c("L1","L2")) %>% 
  unnest()%>% 
  select(level, loc, pop)



pop_df = list(pop_country, pop_l1,pop_l2) %>% bind_rows() %>% 
  select(level, loc, pop)%>% 
  mutate(loc = case_when(
    loc == "Bolivia (Plurinational State of)"~"Bolivia",
    loc == "Venezuela (Bolivarian Republic of)"~"Venezuela",
    loc == "Iran (Islamic Republic of)"~"Iran",
    loc == "Russian Federation"~"Russia",
    loc == "United States of America"~"US",
    TRUE~loc
  )) %>% bind_rows(pop888)

# check pop of Mexico and Peru
pop_l1 %>% group_by(iso2) %>% summarise(pop = sum(pop)) %>% 
  ungroup() %>% arrange(iso2) %>% 
  filter(iso2%in%c("CL","MX"))
pop_country %>% filter(loc%in%c("Mexico","Chile")) %>% group_by(loc) %>% 
  summarise(pop = sum(pop)) 
# Okay, ~ 60-70% of the country wide population  is the SALURBAL populations. 

##### 0.6 - L1/L2 Population ######

#### ********************************************* ####

#### 1. Functions  ####
# Git status.
gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

# Git add.
gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

# Git push.
gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
gitcommit <- function(msg = paste0("Data Update - ", format(Sys.Date(),"%m-%d-%Y")), dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}
#### 1. clean_cum_csse(): 
clean_cum_csse = function(df_tmp, type_tmp){
  df_tmp %>% 
    filter(`Country/Region`!="Others") %>% 
    select(-Lat, -Long, -`Province/State`) %>%
    rename(loc=`Country/Region`) %>% 
    pivot_longer(-loc, names_to = 'date', values_to = "num" ) %>% 
    mutate(date=as.Date(date, format="%m/%d/%y")) %>% 
    group_by(loc, date) %>% 
    summarise(!!type_tmp :=sum(num)) %>% 
    ungroup()
}


##### 2. get_ref_countries(): calculate top n non-LAC countries
get_ref_countries = function(df_tmp,n_tmp){
  df_tmp %>% 
    filter(!loc%in%c(countries_salurbal,other_lac_countries)) %>% 
    group_by(loc) %>% 
    summarise(max = max(confirmed)) %>% 
    ungroup() %>% arrange(desc(max)) %>%
    slice(1:n_tmp) %>% pull(loc)
}


#### 3. generate_lac_colors():get color pallete for LAC countries 
lac_colors = function(){
  ## 0.2 Prepare LAC Color Pallete
  colourCount = length(lac_countries)
  pal.tmp  = "Dark2"
  max.length = brewer.pal(colourCount, pal.tmp) %>% length()
  getPalette = colorRampPalette(brewer.pal(colourCount-max.length,pal.tmp))
  getPalette(colourCount) %>% length()
  tibble(loc =lac_countries,
         colors = getPalette(colourCount),
         size = 1.25 ) %>% 
    bind_rows(tibble(loc = countries_references,
                     colors = "grey70",
                     size = 0.75)) 
}

# expand_colors_df(raw_br_l1,"Dark2")
# df_tmp = raw_br_l2; pal_tmp = "Dark2"
expand_colors_df = function(df_tmp, pal_tmp){
  colourCount = colourCount = length(unique(df_tmp$loc))
  # if (unique(df_tmp$level) == "L1"){ colourCount = length(unique(df_tmp$loc))}
  # else if (unique(df_tmp$level) == "L2"){ colourCount = length(unique(df_tmp$loc))}
  # else if (unique(df_tmp$level) == "state"){ colourCount = length(unique(df_tmp$loc))}
  max.length = brewer.pal(colourCount, pal_tmp) %>% length()
  getPalette = colorRampPalette(brewer.pal(20-max.length,pal_tmp))
  colors_tmp = getPalette(20) 
  rep_tmp = floor(colourCount/20)
  remainder_tmp = colourCount-rep_tmp*20
  color_vector_tmp = ifelse(remainder_tmp==0,
                            rep(colors_tmp,rep_tmp) %>% list(),
                            c(rep(colors_tmp,rep_tmp),colors_tmp[1:remainder_tmp]) %>% list()) %>% 
    unlist()
  
  df_tmp2 = df_tmp %>% group_by(loc) %>% 
    mutate(max_cases = max(confirmed)) %>% ungroup() %>% 
    arrange(desc(max_cases)) %>% select(-max_cases)
  tibble(loc = unique(df_tmp2$loc),
         colors = color_vector_tmp,
         size = 1.25 ,
         country = unique(df_tmp$country)) 
  
  
}


#### 4. clean_daily_csse(): clean daily counts/deaths 
# counts_tmp=counts;deaths_tmp = deaths; df_dates_tmp =  lock.dates.country;smooth_tmp = 7
clean_daily_csse = function(counts_tmp, deaths_tmp, df_dates_tmp, smooth_tmp){
  dfa = left_join(counts_tmp, deaths_tmp) %>% 
    left_join(df_dates_tmp, by = 'loc') %>% 
    filter(loc%in%c(countries_salurbal,other_lac_countries,other_south_countries,other_carribean_countries,countries_references)) %>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "type", values_to  = "counts") %>% 
    arrange(loc, type, date) %>% 
    group_by(loc, type) %>% 
    mutate(daily_counts = counts - lag(counts),
           rollsum=rollmean(daily_counts, smooth_tmp, align = "right", fill = NA)) %>% 
    ungroup() %>%  
    filter(!is.na(rollsum)) %>%
    filter(!is.na(daily_counts)) %>%
    group_by(loc) %>% 
    group_modify(~{
      start.date =   .x %>%  pull(start) %>% unique()
      first.date =  .x%>% filter(counts>0) %>% pull(date) %>% min()
      if(!is.na(start.date)){  
        if (first.date> start.date){.x%>% filter(date>start.date-smooth_tmp)}
        else{.x%>% filter(date>first.date)}}
      else{.x%>% filter(date>first.date)}
    }) %>% ungroup() %>% 
    select(-counts)
}
#counts_tmp=counts_br;deaths_tmp = deaths_br; df_dates_tmp =  lock.dates.state
clean_daily_csse_state= function(counts_tmp, deaths_tmp, df_dates_tmp){
  left_join(counts_tmp, deaths_tmp) %>% 
    left_join(df_dates_tmp, by = c('loc','state')) %>% 
    filter(loc%in%c(other_lac_countries,countries_salurbal)) %>% 
    drop_na()%>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "type", values_to  = "counts") %>% 
    arrange(state, type, date) %>% 
    group_by(state, type) %>% 
    mutate(daily_counts = counts - lag(counts),
           rollsum=rollmean(daily_counts, 5, align = "right", fill = NA)) %>% 
    ungroup() %>% drop_na()%>% 
    group_by(state) %>%
    group_modify(~{
      first.date = .x %>% filter(counts>0) %>% pull(date) %>% min()
      .x %>% filter(date>first.date)
    }) %>% ungroup() %>% 
    select(-counts)
}

# df_tmp = full_mx_state; df_color_tmp = df.color.mx.state
clean_salurbal_cumulative_cases = function(df_tmp, df_color_tmp){
  level_tmp = case_when(unique(df_tmp$level)=="L1"~"loc",
                        unique(df_tmp$level)=="L2"~"loc",
                        unique(df_tmp$level)=="state"~"loc")
  df_tmp %>% 
    group_by_at(level_tmp) %>% 
    mutate(maxc = max(confirmed)) %>% 
    ungroup() %>% 
    filter(maxc >minc) %>% 
    select(-maxc) %>% 
    group_by_at(level_tmp) %>% 
    group_modify(~{
      .x<-.x %>% filter(confirmed>0)
      mindate<-.x %>% filter(confirmed>=minc) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.100=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) 
}

# df_tmp = mutate(full_pe_l1, deaths_rate = 0, deaths = 0); df_color_tmp = df.color.pe.l1
clean_salurbal_cumulative_cases_with_rates = function(df_tmp, df_color_tmp){
  minc_tmp = 100
  minr_tmp = ifelse(
    unique(df_tmp$country) =="Chile",
    1000,
    10
  )
  df_tmp_count = df_tmp%>% 
    select(-c(pop,confirmed_rate,deaths_rate)) %>% 
    group_by(loc) %>% 
    group_by(loc) %>% 
    mutate(maxc = max(confirmed)) %>% 
    ungroup() %>% 
    filter(maxc >minc_tmp) %>% 
    select(-maxc) %>% 
    group_by(loc) %>% 
    group_modify(~{
      .x<-.x %>% filter(confirmed>0)
      mindate<-.x %>% filter(confirmed>=minc_tmp) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.100=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) %>% 
    mutate(rate = "count")
  
  df_tmp_rate = df_tmp %>% 
    select(-confirmed,-deaths,-deaths_rate) %>% 
    rename(confirmed = confirmed_rate) %>% 
    group_by(loc) %>% 
    mutate(maxc = max(confirmed)) %>% 
    ungroup() %>% 
    filter(maxc > minr_tmp) %>% 
    select(-maxc) %>% 
    group_by(loc) %>% 
    group_modify(~{
      .x<-.x %>% filter(confirmed>0)
      mindate<-.x %>% filter(confirmed>=minr_tmp) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.100=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) %>% 
    mutate(rate = "rate")
  
  bind_rows(df_tmp_count,df_tmp_rate)
}

# df_tmp = full_br_state; df_color_tmp = df.color.br.state
# clean_salurbal_cumulative_deaths = function(df_tmp, df_color_tmp){
#   level_tmp = case_when(unique(df_tmp$level)=="L1"~"loc",
#                         unique(df_tmp$level)=="L2"~"loc",
#                         unique(df_tmp$level)=="state"~"loc")  
#   df_tmp %>% 
#     group_by_at(level_tmp) %>%  
#     mutate(maxd = max(deaths)) %>% 
#     ungroup() %>% 
#     filter(maxd >mind) %>% 
#     select(-maxd) %>% 
#     group_by_at(level_tmp) %>% 
#     group_modify(~{
#       .x<-.x %>% filter(deaths>0)
#       mindate<-.x %>% filter(deaths>=mind) %>% slice(1) %>% pull(date)
#       .x<-.x %>% 
#         mutate(days.since.10=date-mindate) %>% 
#         mutate(lastday = ifelse(date == max(date),1,0))
#     }) %>% 
#     ungroup() %>% 
#     left_join(df_color_tmp) 
#    
#  
# }

# df_tmp = full_br_state; df_color_tmp = df.color.br.state
clean_salurbal_cumulative_deaths = function(df_tmp, df_color_tmp){
  level_tmp = case_when(unique(df_tmp$level)=="L1"~"loc",
                        unique(df_tmp$level)=="L2"~"loc",
                        unique(df_tmp$level)=="state"~"loc")  
  df_tmp %>% 
    group_by_at(level_tmp) %>%  
    mutate(maxd = max(deaths)) %>% 
    ungroup() %>% 
    filter(maxd >mind) %>% 
    select(-maxd) %>% 
    group_by_at(level_tmp) %>% 
    group_modify(~{
      .x<-.x %>% filter(deaths>0)
      mindate<-.x %>% filter(deaths>=mind) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.10=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) 
  
  
}
# df_tmp = full_br_l1; df_color_tmp = df.color.br.state
clean_salurbal_cumulative_deaths_with_rates = function(df_tmp, df_color_tmp){
  mind_tmp = 10
  minr_tmp = 10
  
  
  df_tmp_counts = df_tmp %>% 
    select(-c(pop,confirmed_rate,deaths_rate))  %>% 
    group_by(loc) %>%  
    mutate(maxd = max(deaths)) %>% 
    ungroup() %>% 
    filter(maxd >mind_tmp) %>% 
    select(-maxd) %>% 
    group_by(loc) %>% 
    group_modify(~{
      .x<-.x %>% filter(deaths>0)
      mindate<-.x %>% filter(deaths>=mind_tmp) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.10=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) %>% 
    mutate(rate = "count")
  
  df_tmp_rate = df_tmp%>% 
    select(-c(confirmed,deaths,confirmed_rate))  %>% 
    rename(deaths = deaths_rate) %>%
    group_by(loc) %>%  
    mutate(maxd = max(deaths)) %>% 
    ungroup() %>% 
    filter(maxd >minr_tmp) %>% 
    select(-maxd) %>% 
    group_by(loc) %>% 
    group_modify(~{
      .x<-.x %>% filter(deaths>0)
      mindate<-.x %>% filter(deaths>=minr_tmp) %>% slice(1) %>% pull(date)
      .x<-.x %>% 
        mutate(days.since.10=date-mindate) %>% 
        mutate(lastday = ifelse(date == max(date),1,0))
    }) %>% 
    ungroup() %>% 
    left_join(df_color_tmp) %>% 
    mutate(rate = "rate")
  bind_rows(df_tmp_counts,df_tmp_rate)
}

# df_tmp = full_mx_l1
clean_salurbal_sparkline = function(df_tmp){
  grouping_name_tmp  = case_when(unique(df_tmp$level)=="L1"~"loc",
                                 unique(df_tmp$level)=="L2"~"loc",
                                 unique(df_tmp$level)=="state"~"loc") 
  full_15day_tmp = df_tmp %>% 
    group_by_at(grouping_name_tmp)%>% 
    group_modify(~{
      .x %>% 
        arrange(desc(date)) %>% 
        slice(1:15) %>% 
        arrange(date)
    }) %>% 
    rename(confirm.daily=confirmed, deaths.daily = deaths) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    mutate(
      month_labels = month(date, label = T,abbr = T) %>% as.character(),
      date_labels = day(date),
      last_15_labels = paste0(month_labels, ", ",date_labels ),
      confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
      deaths.labels = paste0(last_15_labels," <br /> ",format(deaths.daily,big.mark=",") )
    ) %>% 
    select(c(grouping_name_tmp, "confirm.daily", "deaths.daily", "confirm.labels","deaths.labels"))
  
  df_tmp %>% 
    filter(date == max(df_tmp$date)) %>% 
    arrange(desc(confirmed)) %>% 
    select(-date) %>% 
    full_join(full_15day_tmp) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    group_by_at(grouping_name_tmp) %>% 
    summarize(Deaths = unique(deaths) ,
              Confirmed = unique(confirmed) ,
              spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 20,
                                     chartRangeMin=0,
                                     tooltipFormatter=spk_tool(confirm.labels)),
              spark_deaths = spk_chr(deaths.daily, type = 'bar',width = 80,height = 20,
                                     chartRangeMin=0,
                                     tooltipFormatter=spk_tool(deaths.labels))
    ) %>% 
    ungroup()%>% 
    arrange(desc(Confirmed)) %>% 
    mutate(country = unique(df_tmp$country))
  
}
# df_tmp  = full_mx_l1
clean_salurbal_sparkline_with_rates = function(df_tmp){
  #vec_tmp = {if (unique(df_tmp$country)=="Mexico"){2:16} else{1:15}}
  # max_date_tmp =  {if (unique(df_tmp$country)=="Mexico"){max(df_tmp$date) - 1} else{max(df_tmp$date) }}
  full_15day_tmp = df_tmp %>% 
    group_by(loc)%>% 
    group_modify(~{
      .x %>% 
        arrange(desc(date)) %>% 
        slice(1:15) %>% 
        arrange(date)
    }) %>% 
    rename(confirm.daily=confirmed, 
           deaths.daily = deaths,
           confirm.daily_rate=confirmed_rate, 
           deaths.daily_rate = deaths_rate) %>% 
    arrange(loc,date) %>% 
    mutate(
      month_labels = month(date, label = T,abbr = T) %>% as.character(),
      date_labels = day(date),
      last_15_labels = paste0(month_labels, ", ",date_labels ),
      confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
      deaths.labels = paste0(last_15_labels," <br /> ",format(deaths.daily,big.mark=",") ),
      confirm_rate.labels = paste0(last_15_labels," <br /> ",format(confirm.daily_rate,big.mark=",") ),
      deaths_rate.labels = paste0(last_15_labels," <br /> ",format(deaths.daily_rate,big.mark=",") )
      
    ) %>% 
    select(loc, confirm.daily, deaths.daily, confirm.daily_rate,deaths.daily_rate,
           confirm.labels,deaths.labels,confirm_rate.labels,deaths_rate.labels)
  
  df_tmp %>% 
    group_by(loc) %>% 
    filter(date == max(date) ) %>% 
    ungroup() %>% 
    arrange(desc(confirmed)) %>% 
    full_join(full_15day_tmp) %>% 
    arrange(loc,date) %>% 
    group_by(loc) %>% 
    summarize(Deaths = unique(deaths) ,
              Confirmed = unique(confirmed),
              "Death Rate" = unique(deaths_rate),
              "Confirmed Rate" = unique(confirmed_rate),
              spark_counts = "spark",
              spark_deaths = "spark",
              spark_counts_rate = "spark",
              spark_deaths_rate = "spark"
    ) %>% 
    ungroup()%>% 
    arrange(desc(Confirmed)) %>% 
    mutate(country = unique(df_tmp$country))
}

# df_tmp = full_mx_l1
clean_salurbal_sparkline_confirmed = function(df_tmp){
  grouping_name_tmp  = case_when(unique(df_tmp$level)=="L1"~"loc",
                                 unique(df_tmp$level)=="L2"~"loc",
                                 unique(df_tmp$level)=="state"~"loc") 
  full_15day_tmp = df_tmp %>% 
    group_by_at(grouping_name_tmp)%>% 
    group_modify(~{
      .x %>% 
        arrange(desc(date)) %>% 
        slice(1:15) %>% 
        arrange(date)
    }) %>% 
    rename(confirm.daily=confirmed) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    mutate(
      month_labels = month(date, label = T,abbr = T) %>% as.character(),
      date_labels = day(date),
      last_15_labels = paste0(month_labels, ", ",date_labels ),
      confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
    ) %>% 
    select(c(grouping_name_tmp, "confirm.daily",  "confirm.labels"))
  
  df_tmp %>% 
    group_by(loc) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup() %>%  
    arrange(desc(confirmed)) %>% 
    select(-date) %>% 
    full_join(full_15day_tmp) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    group_by_at(grouping_name_tmp) %>% 
    summarize(Confirmed = unique(confirmed) ,
              spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 20,
                                     chartRangeMin=0,
                                     tooltipFormatter=spk_tool(confirm.labels)),
              
    ) %>% 
    ungroup()%>% 
    arrange(desc(Confirmed)) %>% 
    mutate(country = unique(df_tmp$country))
  
}

clean_salurbal_sparkline_confirmed_with_rate = function(df_tmp){
  grouping_name_tmp  = case_when(unique(df_tmp$level)=="L1"~"loc",
                                 unique(df_tmp$level)=="L2"~"loc",
                                 unique(df_tmp$level)=="state"~"loc") 
  full_15day_tmp = df_tmp %>% 
    group_by_at(grouping_name_tmp)%>% 
    group_modify(~{
      .x %>% 
        arrange(desc(date)) %>% 
        slice(1:15) %>% 
        arrange(date)
    }) %>% 
    rename(confirm.daily=confirmed,
           confirm.daily_rate=confirmed_rate) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    mutate(
      month_labels = month(date, label = T,abbr = T) %>% as.character(),
      date_labels = day(date),
      last_15_labels = paste0(month_labels, ", ",date_labels ),
      confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
      confirm_rate.labels = paste0(last_15_labels," <br /> ",format(confirm.daily_rate,big.mark=",") )
    ) %>% 
    select(c(grouping_name_tmp, "confirm.daily","confirm.daily_rate",
             "confirm.labels","confirm_rate.labels"))
  
  df_tmp %>% 
    group_by(loc) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup() %>%  
    arrange(desc(confirmed)) %>% 
    select(-date) %>% 
    full_join(full_15day_tmp) %>% 
    arrange_(c(grouping_name_tmp,"date")) %>% 
    group_by_at(grouping_name_tmp) %>% 
    summarize(Confirmed = unique(confirmed) ,
              "Confirmed Rate" = unique(confirmed_rate),
              spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 20,
                                     chartRangeMin=0,
                                     tooltipFormatter=spk_tool(confirm.labels)),
              spark_counts_rate = spk_chr(confirm.daily_rate, type = 'bar',width = 80,height = 20,
                                          chartRangeMin=0,
                                          tooltipFormatter=spk_tool(confirm_rate.labels))
              
    ) %>% 
    ungroup()%>% 
    arrange(desc(Confirmed)) %>% 
    mutate(country = unique(df_tmp$country))
  
}
# full_tmp = full_cl_l1;smoother_tmp=7
clean_daily_salurbal= function(full_tmp, smoother_tmp){
  df_tmp = tibble()
  # full_tmp = full_br_l1
  if (unique(full_tmp$level=="L1")){
    df_tmp = full_tmp %>% select(country,loc,salid = salid1, date, confirmed, deaths)
    df_tmp %>% 
      mutate(deaths = ifelse(is.na(deaths),0,deaths))%>% 
      pivot_longer(cols = c(confirmed, deaths), 
                   names_to = "type", 
                   values_to  = "counts") %>% 
      arrange(loc, type, date) %>% 
      group_by( loc, type) %>% 
      mutate(daily_counts = counts - lag(counts),
             rollsum=rollmean(daily_counts, smoother_tmp, align = "right", fill = NA)) %>% 
      ungroup() %>%
      drop_na() %>% 
      group_by(loc) %>%
      # group_modify(~{
      #   first.date = .x %>% filter(counts>0) %>% pull(date) %>% min()
      #   .x %>% filter(date>first.date)
      # }) %>% 
      ungroup() %>% 
      select(-counts,-country) %>% 
      mutate(country = unique(full_tmp$country)) %>% 
      left_join(pop_df %>% 
                  filter(level == "L1") %>%
                  select(salid = loc,pop ) ) %>% 
      mutate(rate = (rollsum/pop)*10^6 )%>% 
      select(-pop)
  }
  else if (unique(full_tmp$level=="L2")) {
    df_tmp = full_tmp%>% select(country,loc,salid = salid2, date, confirmed, deaths)
    df_tmp %>% 
      mutate(deaths = ifelse(is.na(deaths),0,deaths))%>% 
      pivot_longer(cols = c(confirmed, deaths), 
                   names_to = "type", 
                   values_to  = "counts") %>% 
      arrange(loc, type, date) %>% 
      group_by( loc, type) %>% 
      mutate(daily_counts = counts - lag(counts),
             rollsum=rollmean(daily_counts, smoother_tmp, align = "right", fill = NA)) %>% 
      ungroup() %>%
      drop_na() %>% 
      group_by(loc) %>%
      # group_modify(~{
      #   first.date = .x %>% filter(counts>0) %>% pull(date) %>% min()
      #   .x %>% filter(date>first.date)
      # }) %>% 
      ungroup() %>% 
      select(-counts,-country) %>% 
      mutate(country = unique(full_tmp$country)) %>% 
      left_join(pop_df %>% 
                  filter(level == "L2") %>%
                  select(salid = loc,pop ) ) %>% 
      mutate(rate = (rollsum/pop)*10^6 ) %>% 
      select(-pop)
    }
  else if (unique(full_tmp$level=="state")) {
    df_tmp = full_tmp %>% select(country,loc, date, confirmed, deaths)
    df_tmp %>% 
      mutate(deaths = ifelse(is.na(deaths),0,deaths))%>% 
      pivot_longer(cols = c(confirmed, deaths), 
                   names_to = "type", 
                   values_to  = "counts") %>% 
      arrange(loc, type, date) %>% 
      group_by( loc, type) %>% 
      mutate(daily_counts = counts - lag(counts),
             daily_counts = ifelse(country == "Chile", daily_counts/3,daily_counts),
             rollsum=rollmean(daily_counts, smoother_tmp, align = "right", fill = NA)) %>% 
      ungroup() %>% drop_na()%>% 
      group_by(loc) %>%
      # group_modify(~{
      #   first.date = .x %>% filter(counts>0) %>% pull(date) %>% min()
      #   .x %>% filter(date>first.date)
      # }) %>% 
      ungroup() %>% 
      select(-counts,-country) %>% 
      mutate(country = unique(full_tmp$country)) %>% 
      mutate(rate = NA)
    }
  
}

clean_daily_salurbal_smooth= function(full_tmp_2){
  list(7) %>% 
    map_df(function(x){
      clean_daily_salurbal(full_tmp_2,x) %>% 
        mutate(smooth_days = x)
    })
}


#### 5. plot_lockdown_effect():  
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

lockdown_legend_cities = function(){
  
  mtcars %>% slice(1:6) %>% 
    mutate(Lockdown  =ifelse(am==0, "National Lockdown Start", 
                             "National Lock Down End")) %>% 
    mutate(Lockdown = factor(Lockdown, levels = c( "National Lockdown Start", 
                                                   "National Lock Down End"))) %>% 
    ggplot(aes(mpg, wt, col = Lockdown,lty = Lockdown))+
    geom_line(size =1.5)+
    scale_color_manual(values = c("red","blue"))+
    scale_linetype_manual(values = c(6,6))+ 
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          legend.key = element_rect(fill = NA),
          legend.key.width = unit(2, "cm"),
          legend.text=element_text(size=12))
}
lockdown_legend = extract_legend(lockdown_legend_cities())
# plot_lockdown_effect(tidy.daily.country,"Brazil","Yes")
# df_tmp =tidy.daily.country;loc_tmp="Argentina"; smooth_tmp ="yes"
plot_lockdown_effect = function(df_tmp,loc_tmp, smooth_tmp ){
  x = tibble()
  if (smooth_tmp == "no"){
    x = df_tmp %>% 
      filter(loc == loc_tmp)%>% 
      select(-rollsum) %>% 
      rename(rollsum = daily_counts)
  }
  
  else {x= df_tmp %>% filter(loc == loc_tmp)}
  
  cols = col_rolling
  start_tmp = x$start %>% unique()
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p = x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_line(aes(color=type)) +
    scale_color_manual(values=cols)+
    scale_y_continuous(limits=c(0, NA),
                       sec.axis = sec_axis(trans = ~./factorpeak,
                                           name = "New deaths per day (5 day avg)"))+
    scale_x_date(breaks="2 days")+
    guides(color=F)+
    labs(y="New cases per day (5 day avg)",
         x="",
         title=loc_tmp)+
    theme_bw() +
    theme(plot.title = element_text(size=22),
          axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
  grid.arrange(p,
               lockdown_legend, nrow = 2, heights = c(10, 1))
}



plot_lockdown_effect_state = function(df_tmp, loc_tmp, state_tmp, smooth_tmp  ){
  x = tibble()
  if (smooth_tmp == "no"){
    x = df_tmp %>% 
      filter(loc == loc_tmp,
             state == state_tmp)%>% 
      select(-rollsum) %>% 
      rename(rollsum = daily_counts)
  }
  else {x= df_tmp %>% 
    filter(loc == loc_tmp,
           state == state_tmp)}
  
  cols = col_rolling
  
  start_tmp = x$start %>% unique()
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p=x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_line(aes(color=type)) +
    scale_color_manual(values=cols)+
    scale_y_continuous(limits=c(0, NA),
                       sec.axis = sec_axis(trans = ~./factorpeak,
                                           name = "New deaths per day (5 day avg)"))+
    scale_x_date(breaks="2 days")+
    guides(color=F)+
    labs(y="New cases per day (5 day avg)",
         x="",
         title=state_tmp)+
    theme_bw() +
    theme(plot.title = element_text(size=22),
          axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
  grid.arrange(p,
               lockdown_legend, nrow = 2, heights = c(10, 1))
}
# plot_lockdown_effect_state(tidy.daily.br.state,"Brazil","Sao Paulo","yes")
# df_tmp =tidy.daily.br.state;loc_tmp="Brazil";state_tmp = "Sao Paulo"; smooth_tmp ="yes"

##### 6. Sparkline function
spk_tool <- function(labels) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  return %s[field[0].offset];
}",
      jsonlite::toJSON(labels)
    )
  )
}

#### 7. Doubling Legend
doubling_legend = function() {mtcars %>% slice(1:5) %>%  
    mutate(`Doubles in:` = paste0(c(2,3,4,5,7)," days")) %>% 
    ggplot(aes(mpg, wt, col = `Doubles in:`,lty = `Doubles in:`))+
    geom_line(size =1)+
    scale_color_manual(values = c("black","blue","red","green","grey")) +
    scale_linetype_manual(values = c(rep(6,6)))+ 
    theme(legend.title = element_text(size=12),
          legend.position = 'bottom',
          legend.key = element_rect(fill = NA),
          legend.key.width = unit(1, "cm"),
          legend.text=element_text(size=12))+
    guides(col = guide_legend(title.position="top", title.hjust = 0.5))}
doubling_legend = extract_legend(doubling_legend())


##### Maximize Dates 
# 
#  
# min_date_tmp = min(raw_gt_mun_raw$date)
# max_date_tmp = max(raw_gt_mun_raw$date)
# raw_gt_mun = tibble(mun = list(unique(raw_gt_mun_raw$mun)),
#                     date = seq(min_date_tmp,max_date_tmp, by = 'day') ) %>% 
#   unnest() %>% 
#   arrange(mun, date) %>% 
#   left_join(raw_gt_mun_raw) %>% 
#   # filter(mun%in%xwalk_sal_gt$mun) %>% 
#   mutate(country = "Guatemala")


#### ********************************************* ####

