#### 0. Global Variables ####

## 0.1 LAC Countries
countries_salurbal<-c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica",
                      "Mexico", "Peru", "Panama", "Nicaragua","Guatemala", "El Salvador")
other_lac_countries<-c("Honduras", "Uruguay", "Bolivia", "Venezuela",
                       "Ecuador", "Cuba", "Paraguay", "Dominican Republic")
lac_countries = c(countries_salurbal,other_lac_countries)

## 0.2 Cumulative Plots
minc = 100
mind = 10 


## 0.3 Others
col_rolling = c("#1b9e77", "#d95f02")


#### 1. clean_cum_csse(): Clean Cumulative counts/deaths ####
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


##### 2. get_ref_countries(): calculate top n non-LAC countries ####
get_ref_countries = function(df_tmp,n_tmp){
  df_tmp %>% 
    filter(!loc%in%c(countries_salurbal,other_lac_countries)) %>% 
    group_by(loc) %>% 
    summarise(max = max(confirmed)) %>% 
    ungroup() %>% arrange(desc(max)) %>%
    slice(1:n_tmp) %>% pull(loc)
}


#### 3. generate_lac_colors():get color pallete for LAC countries ####
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

#### 4. clean_daily_csse(): clean daily counts/deaths ####
clean_daily_csse = function(counts_tmp, deaths_tmp, df_dates_tmp){
  left_join(counts_tmp, deaths_tmp) %>% 
    left_join(df_dates_tmp, by = 'loc') %>% 
    drop_na()%>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "type", values_to  = "counts") %>% 
    arrange(loc, type, date) %>% 
    group_by(loc, type) %>% 
    mutate(daily_counts = counts - lag(counts),
           rollsum=rollmean(daily_counts, 5, align = "center", fill = NA)) %>% 
    ungroup() %>% drop_na()%>% 
    group_by(loc) %>%
    group_modify(~{
      first.date = (pull(.x,start) %>% unique()- 10)
      last.date = first.date +40
      .x %>% filter(date>first.date) %>% filter(date<last.date)
    }) %>% ungroup() %>% 
    select(-counts)
}
clean_daily_csse_state= function(counts_tmp, deaths_tmp, df_dates_tmp){
  left_join(counts_tmp, deaths_tmp) %>% 
    left_join(df_dates_tmp, by = c('loc','state')) %>% 
    drop_na()%>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "type", values_to  = "counts") %>% 
    arrange(state, type, date) %>% 
    group_by(state, type) %>% 
    mutate(daily_counts = counts - lag(counts),
           rollsum=rollmean(daily_counts, 5, align = "center", fill = NA)) %>% 
    ungroup() %>% drop_na()%>% 
    group_by(state) %>%
    group_modify(~{
      first.date = (pull(.x,start) %>% unique()- 10)
      last.date = first.date +40
      .x %>% filter(date>first.date) %>% filter(date<last.date)
    }) %>% ungroup() %>% 
    select(-counts)
}
#counts_tmp = counts_br;deaths_tmp = deaths_br;df_dates_tmp = lock.dates.state
#### 5. plot_lockdown_effect():  ######
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
  x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4+4+4, lty=2,size =1.5, color="grey")+
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
    theme(axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
}

# plot_lockdown_effect(tidy.full.rolling.country,"Argentina","No")
# df_tmp =tidy.full.rolling.state;loc_tmp="Brazil";state_tmp = "Sao Paulo"; smooth_tmp ="yes"

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
  x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4+4, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+4+4+4+4+4+4, lty=2,size =1.5, color="grey")+
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
    theme(axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
}
# plot_lockdown_effect_state(tidy.full.rolling.state,"Brazil","Sao Paulo","yes")
# df_tmp =tidy.full.rolling.state;loc_tmp="Brazil";state_tmp = "Sao Paulo"; smooth_tmp ="yes"

##### 6. Sparkline function #####
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


######  DUMP TO FUNCTIONALIZE #######
#### 0.1 - Set up Data ####
## Get Data from Github
raw_count  = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  as_tibble()
raw_deaths =  fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  as_tibble()
## Clean counts and deaths
counts = clean_cum_csse(raw_count,"confirmed");deaths= clean_cum_csse(raw_deaths,"deaths")
## Countries of Interest
countries_references = get_ref_countries(counts,10)
countries_interest =  c(countries_salurbal,other_lac_countries,countries_references)
# Prepare Color Pallete
df.color = lac_colors()
# Country Centroid coordinates  
country_coords = fread("country_google_cord.csv") %>% as_tibble() %>% 
  filter(!loc%in%c("Diamond Princess",
                   "Georgia",
                   "Holy See","Togo")) 

## Process data for plots of cumulative confirmed cases and deaths 

full_all_raw  = full_join(counts,deaths, by = c("loc","date")) %>% 
  left_join(df.color, by = "loc") %>% 
  left_join(country_coords) %>% 
  # mutate(lng = case_when(loc=="US"~-94.8565921,
  #                        loc=="Dominican Republic" ~	-70.50569,
  #                        TRUE~lng),
  #        lat = case_when(loc =="US"~39.0921151,
  #                        loc =="Dominican Republic" ~18.89433,
  #                        TRUE~lat)) %>% 
  mutate(date_label = paste(lubridate::month(date,label=T, abbr = T),",",day(date))) 

full_all= full_all_raw%>% 
  filter(!is.na(lng))

full =  full_all %>%  filter(loc%in%countries_interest)

df.c = full %>% 
  group_by(loc) %>% 
  mutate(maxc = max(confirmed)) %>% 
  ungroup() %>% 
  filter(maxc >minc) %>% 
  select(-maxc) %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x<-.x %>% filter(confirmed>0)
    mindate<-.x %>% filter(confirmed>=minc) %>% slice(1) %>% pull(date)
    .x<-.x %>% 
      mutate(days.since.100=date-mindate) %>% 
      mutate(lastday = ifelse(date == max(date),1,0))
  }) %>% 
  ungroup() %>% 
  left_join(df.color)
df.d  = full %>% 
  group_by(loc) %>% 
  mutate(maxd = max(deaths)) %>% 
  ungroup() %>% 
  filter(maxd >mind) %>% 
  select(-maxd) %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x<-.x %>% filter(deaths>0)
    mindate<-.x %>% filter(deaths>=mind) %>% slice(1) %>% pull(date)
    .x<-.x %>% 
      mutate(days.since.10=date-mindate) %>% 
      mutate(lastday = ifelse(date == max(date),1,0))
  }) %>% 
  ungroup() %>% 
  left_join(df.color)
template_count<-tibble(days.since.100=0:(max(df.c$days.since.100)+5))
template_count$y<-minc*2^(template_count$days.since.100)
template_count$y2<-minc*(2^(1/2))^(template_count$days.since.100)
template_count$y3<-minc*(2^(1/3))^(template_count$days.since.100)
template_count$y4<-minc*(2^(1/4))^(template_count$days.since.100)
template_death<-tibble(days.since.10=0:(max(df.d$days.since.10)+5))
template_death$y<-mind*2^(template_death$days.since.10)
template_death$y2<-mind*(2^(1/2))^(template_death$days.since.10)
template_death$y3<-mind*(2^(1/3))^(template_death$days.since.10)
template_death$y4<-mind*(2^(1/4))^(template_death$days.since.10)

## Latest Date
date_str = paste0("Data last update: ",max(full$date)," 11:59PM" )

#### 0.2 - Set Up Brazil data  ####
## Read raw data
raw_br  = fread("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  as_tibble() %>% 
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
            deaths = sum(deaths)) %>% ungroup()

# Prepare Color Pallete
colourCount = length(unique(raw_br$loc))
pal.tmp  = "Dark2"
max.length = brewer.pal(colourCount, pal.tmp) %>% length()
getPalette = colorRampPalette(brewer.pal(colourCount-max.length,pal.tmp))
getPalette(colourCount) %>% length()
df.color.br = tibble(loc =unique(raw_br$loc),
                     colors = getPalette(colourCount),
                     size = 1.25 ) 
full_br= raw_br %>% left_join(df.color.br)%>%
  arrange(loc,date) 
df.c.br = full_br %>% 
  group_by(loc) %>% 
  mutate(maxc = max(confirmed)) %>% 
  ungroup() %>% 
  filter(maxc >minc) %>% 
  select(-maxc) %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x<-.x %>% filter(confirmed>0)
    mindate<-.x %>% filter(confirmed>=minc) %>% slice(1) %>% pull(date)
    .x<-.x %>% 
      mutate(days.since.100=date-mindate) %>% 
      mutate(lastday = ifelse(date == max(date),1,0))
  }) %>% 
  ungroup() %>% 
  left_join(df.color.br)
df.d.br  = full_br %>% 
  group_by(loc) %>% 
  mutate(maxd = max(deaths)) %>% 
  ungroup() %>% 
  filter(maxd >mind) %>% 
  select(-maxd) %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x<-.x %>% filter(deaths>0)
    mindate<-.x %>% filter(deaths>=mind) %>% slice(1) %>% pull(date)
    .x<-.x %>% 
      mutate(days.since.10=date-mindate) %>% 
      mutate(lastday = ifelse(date == max(date),1,0))
  }) %>% 
  ungroup() %>% 
  left_join(df.color.br)

# 
# full_br %>% ggplot(aes(x = date, y = log(confirmed), group = loc, col = loc))+geom_line()

#### 0.3 - Set up Plot figures ####




#### 0.4 - Set up Interactive Table  ####

## Sparkline
full_15days = full %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x %>% 
      arrange(desc(date)) %>% 
      slice(1:15) %>% 
      arrange(date)
  }) %>% 
  rename(confirm.daily=confirmed, deaths.daily = deaths) %>% 
  arrange(loc,date) %>% 
  mutate(
    month_labels = month(date, label = T,abbr = T) %>% as.character(),
    date_labels = day(date),
    last_15_labels = paste0(month_labels, ", ",date_labels ),
    confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
    deaths.labels = paste0(last_15_labels," <br /> ",format(deaths.daily,big.mark=",") )
  ) %>% 
  select(loc, confirm.daily, deaths.daily, confirm.labels,deaths.labels)

dt1_global =  full %>% filter(date == max(full$date)) %>% 
  arrange(desc(confirmed)) %>% 
  select(-date) %>% 
  mutate(fatality_rate = round(deaths/confirmed*100,1))%>% 
  full_join(full_15days) %>% 
  group_by(loc) %>% 
  summarize(Deaths = unique(deaths) ,
            Confirmed = unique(confirmed) ,
            `Fatality Rate` = unique(fatality_rate),
            spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 20,
                                   chartRangeMin=0,
                                   tooltipFormatter=spk_tool(confirm.labels)),
            spark_deaths = spk_chr(deaths.daily, type = 'bar',width = 80,height = 20,
                                   chartRangeMin=0,
                                   tooltipFormatter=spk_tool(deaths.labels))
  ) %>% 
  rename(Country = loc) %>% 
  ungroup()%>% 
  arrange(desc(Confirmed))

## BR
full_15days_br = full_br %>% 
  group_by(loc) %>% 
  group_modify(~{
    .x %>% 
      arrange(desc(date)) %>% 
      slice(1:15) %>% 
      arrange(date)
  }) %>% 
  rename(confirm.daily=confirmed, deaths.daily = deaths) %>% 
  arrange(loc,date) %>% 
  mutate(
    month_labels = month(date, label = T,abbr = T) %>% as.character(),
    date_labels = day(date),
    last_15_labels = paste0(month_labels, ", ",date_labels ),
    confirm.labels = paste0(last_15_labels," <br /> ",format(confirm.daily,big.mark=",") ),
    deaths.labels = paste0(last_15_labels," <br /> ",format(deaths.daily,big.mark=",") )
  ) %>% 
  select(loc, confirm.daily, deaths.daily, confirm.labels,deaths.labels)

dt1_global_br =  full_br %>% filter(date == max(full_br$date)) %>% 
  arrange(desc(confirmed)) %>% 
  select(-date) %>% 
  mutate(fatality_rate = round(deaths/confirmed*100,1))%>% 
  full_join(full_15days_br) %>% 
  group_by(loc) %>% 
  summarize(Deaths = unique(deaths) ,
            Confirmed = unique(confirmed) ,
            `Fatality Rate` = unique(fatality_rate),
            spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 20,
                                   chartRangeMin=0,
                                   tooltipFormatter=spk_tool(confirm.labels)),
            spark_deaths = spk_chr(deaths.daily, type = 'bar',width = 80,height = 20,
                                   chartRangeMin=0,
                                   tooltipFormatter=spk_tool(deaths.labels))
  ) %>% 
  rename(Country = loc) %>% 
  ungroup()%>% 
  arrange(desc(Confirmed))

#### 0.5 - Set up Map  ####


## Leaflet Basemap
df_tmp = full_all %>% 
  filter(date == max(full_all$date))
basemap = leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE )) %>%
  setView(lng = -20.11915, lat = 8.517508,zoom = 3)  

##### 0.6 Set up tidy rolling data #####
load("lock.dates.state.rdata")
load("lock_dates_country.rdata")
lock.dates.state = lock.dates.state %>% 
  mutate_at(vars(place,loc,level),~stringi::stri_trans_general(.x, "Latin-ASCII") %>% str_trim()) %>% 
  mutate_at(vars(start, end),~.x %>% str_sub(1,10) %>% ymd()) %>% 
  rename(state = place)

## deaths and confirmed data
tidy.full.rolling.country = clean_daily_csse(counts, deaths,lock.dates.country)

#Clean Brazil States
counts_br = df.c.br %>% 
  select(loc = country, state = loc, date, confirmed) %>% 
  filter(state%in%c("SP","SC")) %>% 
  mutate(state = ifelse(state == "SP","Sao Paulo","Santa Catarina"))
deaths_br = df.c.br %>% 
  select(loc = country, state = loc, date, deaths)%>% 
  filter(state%in%c("SP","SC")) %>% 
  mutate(state = ifelse(state == "SP","Sao Paulo","Santa Catarina"))
tidy.full.rolling.state = clean_daily_csse_state(counts_br, deaths_br,lock.dates.state)

#plot_lockdown_effect_state(tidy.full.rolling.state,"Argentina","Sao Paulo", "Yes")
