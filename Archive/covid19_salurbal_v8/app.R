library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(lubridate)
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
shinyOptions(cache = diskCache("./covid19app-cache"))

#### 0.1 - Set up Data ####
options(scipen = 99)

## Get Data from Github
raw_count  = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  as_tibble()
raw_deaths =  fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  as_tibble()


## Clean counts and deaths
counts = raw_count %>% 
  filter(`Country/Region`!="Others") %>% 
  select(-Lat, -Long, -`Province/State`) %>%
  rename(loc=`Country/Region`) %>% 
  pivot_longer(-loc, names_to = 'date', values_to = "cases" ) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y")) %>% 
  group_by(loc, date) %>% 
  summarise(confirmed=sum(cases)) %>% 
  ungroup()
deaths = raw_deaths%>% 
  filter(`Country/Region`!="Others") %>% 
  select(-Lat, -Long, -`Province/State`) %>%
  rename(loc=`Country/Region`) %>% 
  pivot_longer(-loc, names_to = 'date', values_to = "deaths" ) %>% 
  mutate(date=as.Date(date, format="%m/%d/%y")) %>% 
  group_by(loc, date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

## Countries of Interest
countries_salurbal<-c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica",
                      "Mexico", "Peru", "Panama", "Nicaragua","Guatemala", "El Salvador")
other_lac_countries<-c("Honduras", "Uruguay", "Bolivia", "Venezuela",
                       "Ecuador", "Cuba", "Paraguay", "Dominican Republic")
countries_references = counts %>% 
  filter(!loc%in%c(countries_salurbal,other_lac_countries)) %>% 
  group_by(loc) %>% 
  summarise(max = max(confirmed)) %>% 
  ungroup() %>% arrange(desc(max)) %>%
  slice(1:10) %>% pull(loc)
countries_interest =  c(countries_salurbal,other_lac_countries,countries_references)

# Prepare Color Pallete
lac_sal_countries = c(countries_salurbal,other_lac_countries)
colourCount = length(lac_sal_countries)
pal.tmp  = "Dark2"
max.length = brewer.pal(colourCount, pal.tmp) %>% length()
getPalette = colorRampPalette(brewer.pal(colourCount-max.length,pal.tmp))
getPalette(colourCount) %>% length()
df.color = tibble(loc =lac_sal_countries,
                  colors = getPalette(colourCount),
                  size = 1.25 ) %>% 
  bind_rows(tibble(loc = countries_references,
                   colors = "grey70",
                   size = 0.75)) 
# Country Centroid coordinates  

country_coords = read.csv("country_google_cord.csv") %>% as_tibble() %>% 
  filter(!loc%in%c("Diamond Princess",
                   "Georgia",
                   "Holy See","Togo")) 

## Process data for plots of confirmed cases and deaths 
minc = 100
mind = 10 
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

## Generate Doubling template for count 
template_count<-tibble(days.since.100=0:(max(df.c$days.since.100)+5))
template_count$y<-minc*2^(template_count$days.since.100)
template_count$y2<-minc*(2^(1/2))^(template_count$days.since.100)
template_count$y3<-minc*(2^(1/3))^(template_count$days.since.100)
template_count$y4<-minc*(2^(1/4))^(template_count$days.since.100)

## Generate doubling template for deaths
template_death<-tibble(days.since.10=0:(max(df.d$days.since.10)+5))
template_death$y<-mind*2^(template_death$days.since.10)
template_death$y2<-mind*(2^(1/2))^(template_death$days.since.10)
template_death$y3<-mind*(2^(1/3))^(template_death$days.since.10)
template_death$y4<-mind*(2^(1/4))^(template_death$days.since.10)


#### 0.4 - Set up Interactive Table  ####

## Sparkline
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


#### 1. UI ####
ui <- bootstrapPage(theme = shinytheme("flatly"),
        #shinythemes::themeSelector(),
        navbarPage(title = "COVID19 in Latin America",

           # Tab 1         
           tabPanel("Plots",
                    sidebarLayout(
                      sidebarPanel(width  = 3,
                                   pickerInput("data_level","Level",
                                               choices = c("Country"="country",
                                                           "State"="state")),
                                   conditionalPanel("input.data_level == 'country'",
                                                    pickerInput("countries_interest",label = "Select Countries",
                                                                multiple = TRUE,
                                                                choices = list(
                                                                  SALURBAL = countries_salurbal,
                                                                  LAC = other_lac_countries,
                                                                  Reference = countries_references),
                                                                selected = c(countries_salurbal))),
                                   conditionalPanel("input.data_level == 'state'",
                                                    pickerInput("country_interest",label = "Select Country", 
                                                                selected = "Brazil",
                                                                choices = c("Brazil"))),
                                   pickerInput("data_type","Outcome",
                                               choices = c("Cases"="cases",
                                                           "Deaths"="deaths")),
                                   pickerInput("time_type","Plotting start date:",
                                               choices = c("Since onset" = "onset",
                                                           "Real Time"="true")),
                                   em("Tip: Click rows in the Data Table to highlight countries or cities!")
                      ),
                      
                      mainPanel(width  = 9,
                                column(width = 4, DT::dataTableOutput('dt1')),
                                column(width = 5, plotOutput("plot_count", height = "450" ))
                      )
                    )
           ),
           # Tab 2
           tabPanel("Map",
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("mapper", width="100%", height="100%"),
                        plotOutput('mapper_add_marker'),
                        
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      top = 80, left = 20, width = 250, fixed=TRUE,
                                      draggable = TRUE, height = "auto",
                                      h3("Visualize Cumulative Cases Over Time"),
                                      h4(textOutput("map_text_date")),
                                      h4(textOutput("map_text_num")),
                                      sliderInput("plot_date",
                                                  label = h5("Select mapping date"),
                                                  min = min(full$date),
                                                  max = max(full$date),
                                                  step  = 1,
                                                  value = max(full$date),
                                                  timeFormat = "%d %b", 
                                                  animate=animationOptions(interval =300, loop = FALSE))
                        )
                    )
                    
           ),
           # Tab 2
           tabPanel("About App",
               h4("Background"),
               p("COVID19 has already hit Asia, Europe and North America and is begining to 
                 escalate in Latin America. Interactive data visualizations have become a powerful 
                 tool to track the progression of COVID19 for not only the research community but also
                 for the general public. The goal of this app is to tailor visualizations to highlight 
                 COVID19 in SALURBAL and other Latin American Countries. The data is updated daily to 
                 include reported from the previous day."),
               h4("About Salurbal"),
               p("Salud Urbana en America Latina (SALURBAL), Urban Health in Latin America, is a five-year 
                 project launched in April 2017. The Drexel University Dornsife School of Public Health 
                 and partners throughout Latin America and in the United States are working together 
                 to study how urban environments and urban policies impact the health of city residents 
                 throughout Latin America. Their findings will inform policies and interventions to create 
                 healthier, more equitable, and more sustainable cities worldwide. SALURBAL is funded by 
                 the Wellcome Trust as part of its Our Planet, Our Health initiative, which focuses on 
                 research examining the connections between the environment and human health."),
               h4("Code and Data Sources"),
               "Code:",tags$a(href="https://github.com/rl627/SALURBAL-Covid19-App","https://github.com/rl627/SALURBAL-Covid19-App"),br(),
               "Country Level Cases:",tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                                                             "Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)"),br(),
               "Country Level Deaths:",tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                                                             "Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)"),br(),
               "Brazil Data:",tags$a(href="https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv",
                                                             "https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv"),br(),
               
               h4("Authors"),
               "Ran Li, Data Analyst at SALURBAL",br(),
               "Usama Bilal, Co-Investigator at SALURBAL",br() ,br() ,br() ,
               fluidRow(

               absolutePanel(left = 20, width = 80, fixed=F, draggable = FALSE, height = "auto",
                             tags$a(href='https://drexel.edu/lac/salurbal/overview/', tags$img(src='SALURBAL_logo.png',height='70',width='160'))),
               absolutePanel(left = 220, width = 80, fixed=F, draggable = FALSE, height = "auto",
                             tags$a(href='https://drexel.edu/uhc/', tags$img(src='UHC_logo.png',height='70',width='160'))),
               absolutePanel(left = 420, width = 30, fixed=F, draggable = FALSE, height = "auto",
                             actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:10px',
                                          onclick = sprintf("window.open('%s')",
                                                            "https://twitter.com/intent/tweet?text=%20@LACUrbanHealth%20&url=http://apps.uhcdata.org:3838/rl627@drexel.edu/covid19_salurbal/&hashtags=coronavirus")))
               )
               
               
                    
           )  
                   
        )
)

#### 2. Server ####

server <- function(input, output) {

  ## For Map
  full_date_react = reactive({
    full_all %>% filter(date == input$plot_date)
  })
  full_date_react2 = reactive({
    full_all_raw %>% filter(date == input$plot_date)
  })
  output$map_text_date = renderText(full_date_react() %>% slice(1) %>% pull(date_label))
  output$map_text_num = renderText(full_date_react2() %>% pull(confirmed) %>% sum() %>% format(big.mark=",") %>% paste("Total Global Cases:",.))
  output$mapper = renderLeaflet({
    basemap 
  })
  output$mapper_add_marker = renderPlot({
    leafletProxy("mapper") %>%
      clearMarkers() %>%
  addCircleMarkers(data = full_date_react(), lat = ~lat, lng = ~lng, radius=~(confirmed)^(1/3),
                   label  = ~paste(loc,":",format(confirmed,big.mark=",")),
                   fillOpacity = 0.2,
                   color = "red", weight = 1)
  })

    
  ## For DT
  dt2_global = reactive({
    out_tmp = tibble()
    if(input$data_level == "country"){
      if (input$data_type == "cases"){
        out_tmp=dt1_global %>% 
          select(Country,Trend = spark_counts ,Confirmed) %>% 
          arrange(desc(Confirmed)) %>% 
          mutate(Confirmed = format(Confirmed,big.mark=","))%>% 
          filter(Country%in%input$countries_interest)}
      else {out_tmp=dt1_global %>% 
        select(Country,Trend = spark_deaths,Deaths) %>% 
        arrange(desc(Deaths))%>% 
        mutate(Deaths = format(Deaths,big.mark=","))%>% 
        filter(Country%in%input$countries_interest)}
    } 
    else { # state
      if (input$data_type == "cases"){
        out_tmp=dt1_global_br %>% 
          select(Country,Trend = spark_counts ,Confirmed) %>% 
          arrange(desc(Confirmed)) %>% 
          mutate(Confirmed = format(Confirmed,big.mark=","))}
      else {out_tmp=dt1_global_br %>% 
        select(Country,Trend = spark_deaths,Deaths) %>% 
        arrange(desc(Deaths))%>% 
        mutate(Deaths = format(Deaths,big.mark=","))}
    }
    out_tmp 
  })
  
  output$dt1 = DT::renderDataTable({
    tmp_col_name = ifelse(input$data_level == "country","Country","State")
    dt2_global() %>% 
      rename(!!tmp_col_name := Country) %>% 
      datatable(escape = F,
                rownames = F,
                class = 'cell-border stripe',
                selection = "single",
                options = list(fnDrawCallback = htmlwidgets::JS('function(){
                                                  HTMLWidgets.staticRender();
                                                  }'),
                               sDom  = '<"top">rt<"bottom">lp',
                               pageLength = 10))  %>% 
      spk_add_deps()
  })

  DT_selected_countries = reactive({
    if(length(input$dt1_rows_selected)==0){NA_character_}
    else {if(input$data_level =="Country"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(Country)}
      else{dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(Country)}}
    
  })
  
  df_c_react_level = reactive({if (input$data_level == "country"){df.c} else{df.c.br}  })
  df_d_react_level = reactive({if (input$data_level == "country"){df.d} else{df.d.br}  })
  
  df_c_react = reactive({
    if (input$data_level == "country"){
      if( is.na(DT_selected_countries()) ){df_c_react_level() %>% filter(loc%in%input$countries_interest)}
      else {df_c_react_level() %>% 
          filter(loc%in%input$countries_interest) %>% 
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))
        }
    }
    else {
      if( is.na(DT_selected_countries()) ){df_c_react_level()}
      else {df_c_react_level() %>% 
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))
      }
    }
  })
  
  df_d_react = reactive({
    if (input$data_level == "country"){
      if( is.na(DT_selected_countries()) ){df.d %>% filter(loc%in%input$countries_interest)}
      else {df.d %>% 
          filter(loc%in%input$countries_interest) %>% 
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))
      }
    }
    else {
      if( is.na(DT_selected_countries()) ){df_d_react_level()}
      else {df_d_react_level() %>% 
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))
      }
    }
  })
  
  full_react = reactive({

    if (input$data_level == "country"){
      if( is.na(DT_selected_countries()) ){full %>% filter(loc%in%input$countries_interest)}
      else {full %>% filter(loc%in%input$countries_interest) %>%
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))}
    }
    else { 
      if( is.na(DT_selected_countries()) ){full_br}
      else {full_br %>%
          mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
                 size = ifelse(loc != DT_selected_countries(),0.75,size))}
    }

    })

  
  output$plot_count = renderCachedPlot({
    p_count_onset = df_c_react() %>% 
      ggplot(aes(x=days.since.100, y=confirmed)) +
      annotate("text", label="Doubles\nevery day", x=5, y=10*(2^(1/1))^(12), color="black")+
      geom_line(data=template_count, aes(x=days.since.100, y=y2), lty=2, color="blue")+
      annotate("text", label="Doubles\nevery 2 days", x=15, y=10*(2^(1/2))^(as.numeric(max(df_c_react()$days.since.100))), color="blue")+
      geom_line(data=template_count, aes(x=days.since.100, y=y3), lty=2, color="red")+
      annotate("text", label="Doubles\nevery 3 days", x=max(df_c_react()$days.since.100)+2.5, y=minc*(2^(1/3))^(as.numeric(max(df_c_react()$days.since.100))), color="red")+
      geom_line(data=template_count, aes(x=days.since.100, y=y4), lty=2, color="green")+
      annotate("text", label="Doubles\nevery 4 days", x=max(df_c_react()$days.since.100)+2.5, y=minc*(2^(1/4))^(as.numeric(max(df_c_react()$days.since.100))), color="green")+
      geom_line(data =df_c_react(), aes(group=loc), size = pull(df_c_react(),size), color = pull(df_c_react(), colors))+
      geom_text_repel(data=df_c_react() %>% filter(lastday==1), 
                      aes(label=loc), color=df_c_react() %>% filter(lastday==1) %>% pull(colors),
                      segment.alpha = 0)+
      geom_line(data=template_count, aes(x=days.since.100, y=y), lty=2, color="black")+
      scale_linetype_manual(values=c(2, 1))+
      scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_c_react(),days.since.100))), by=5), limits=c(0, as.numeric(max(pull(df_c_react(),days.since.100)))+5))+
      scale_y_log10(limits=c(100, 10^nchar(trunc(max(pull(df_c_react(),confirmed))))))+
    annotation_logticks(sides="l") +
      labs(title="Confirmed Cases in the LAC Region (countries with >=100 cases)",
           x="Days from epidemic onset (>=100 cases)", y="Confirmed Cases")+
      theme_bw() +
      theme(panel.grid.minor.x = element_line(),
            legend.position = 'none')

    
    p_death_onset =  df_d_react() %>%
        ggplot(aes(x=days.since.10, y=deaths)) +
        geom_line(data=template_death, aes(x=days.since.10, y=y), lty=2, color="black")+
        annotate("text", label="Doubles\nevery day", x=5, y=10*(2^(1/1))^(5), color="black")+
        geom_line(data=template_death, aes(x=days.since.10, y=y2), lty=2, color="blue")+
        annotate("text", label="Doubles\nevery 2 days", x=12, y=10^3, color="blue")+
        geom_line(data=template_death, aes(x=days.since.10, y=y3), lty=2, color="red")+
        annotate("text", label="Doubles\nevery 3 days", x=as.numeric(max(pull(df_d_react(),days.since.10)))+2.5, y=mind*(2^(1/3))^(as.numeric(as.numeric(max(pull(df_d_react(),days.since.10))))), color="red")+
        geom_line(data=template_death, aes(x=days.since.10, y=y4), lty=2, color="green")+
        annotate("text", label="Doubles\nevery 4 days", x=as.numeric(max(pull(df_d_react(),days.since.10)))+2.5, y=mind*(2^(1/4))^(as.numeric(as.numeric(max(pull(df_d_react(),days.since.10))))), color="green")+
        geom_line(data =df_d_react(), aes(group=loc), size = pull(df_d_react(),size), color = pull(df_d_react(), colors))+
        geom_text_repel(data=df_d_react() %>% filter(lastday==1),
                        aes(label=loc), color=df_d_react() %>% filter(lastday==1) %>% pull(colors),
                        segment.alpha = 0)+
        scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_d_react(),days.since.10))), by=5), limits=c(0, as.numeric(max(pull(df_d_react(),days.since.10)))+5))+
        scale_y_log10(limits=c(10, 10^nchar(trunc(max(pull(df_d_react(),deaths))))))+
        annotation_logticks(sides="l") +
        labs(title="Deaths in the LAC Region (countries with >=10 Deaths)",
             x="Days from epidemic onset (>=10 Deaths)", y="Deaths")+
        theme_bw() +
        theme(panel.grid.minor.x = element_line(),
              legend.position = 'none')
    
    
    p_count_date =  full_react() %>%
      ggplot(aes(x=date, y=confirmed)) +
      geom_line(aes(group=loc), col = pull(full_react(), colors), size = pull(full_react(), size))+
      geom_text_repel(data=full_react() %>% arrange(loc, desc(date)) %>%
                        filter(!duplicated(loc), confirmed>1),
                      aes(label=loc, x=date), hjust=0,
                      color= full_react() %>% arrange(loc, desc(date)) %>% filter(!duplicated(loc), confirmed>1) %>% pull(colors),
                      segment.color = "gray",
                      nudge_x = max(full_react()$date)+3,
                      direction     = "y",
                      hjust = 0.5)+
      scale_x_date(limits=c(filter(full_react(),confirmed> 1) %>% pull(date) %>% min(), max(full_react()$date)+10))+
      scale_y_log10(limits=c(1, 10^nchar(trunc(max(pull(full_react(),confirmed))))  )  )+
      annotation_logticks(sides="l") +
      labs(title="Confirmed Cases in the LAC Region",
           x="Date")+
      guides(color=F)+
      theme_bw() +
      theme(panel.grid.minor.x = element_line())
    

    p_death_date = full_react()  %>%
      ggplot(aes(x=date, y=deaths)) +
      geom_line(aes(group=loc), col = pull(full_react(), colors), size = pull(full_react(), size))+
      geom_text_repel(data=full_react() %>% arrange(loc, desc(date)) %>%
                        filter(!duplicated(loc), deaths>1),
                      aes(label=loc, x=date), hjust=0,
                      color= full_react() %>% arrange(loc, desc(date)) %>% filter(!duplicated(loc), deaths>1) %>% pull(colors),
                      segment.color = "gray",
                      nudge_x = max(full_react()$date)+3,
                      direction     = "y",
                      hjust = 0.5)+
      scale_linetype_manual(values=c(2, 1))+
      scale_x_date(limits=c( filter(full_react(),deaths> 1) %>% pull(date) %>% min(), max(full$date)+5))+
      scale_y_log10(limits=c(1, 10^nchar(trunc(max(pull(full_react(),deaths))))))+
      annotation_logticks(sides="l") +
      labs(title="Confirmed Deaths in the LAC Region",
           x="Date")+
      guides(color=F)+
      theme_bw() +
      theme(panel.grid.minor.x = element_line())
    
    
    
    
    if      ( (input$time_type=="onset") & (input$data_type == "cases"))  {p_count_onset}
    else if ( (input$time_type!="onset") & (input$data_type == "cases"))  {p_count_date}
    else if ( (input$time_type=="onset") & (input$data_type == "deaths")) {p_death_onset}
    else {p_death_date}
  },
  cacheKeyExpr = { list(input$time_type,input$data_type,df_c_react(),df_d_react(),full_react() ) })
  
}



#### 3. Run App ####

shinyApp(ui = ui, server = server)

