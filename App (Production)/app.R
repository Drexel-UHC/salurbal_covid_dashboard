rm(list=ls())
library(httr)
library(rintrojs)
library(cowplot)
library(shiny)
library(waiter)
library(sf)
library(highcharter)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(janitor)
library(scales)
library(gridExtra)
library(DT)
library(sparkline)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(dplyr)
options(scipen = 99)
load("sf_salurbal_0.8.rdata")
load("sf_l2_simp.rdata")
load("covid19_processed_data_static.rdata")


## Development Setup
# load("covid19_processed_data_dynamic.rdata")

## Stage Set up
# load("../../SALURBAL Covid19 Git Internal/Clean/covid19_processed_data_dynamic.rdata")

## Production Setup
req = content(
  GET(
    url = "https://api.github.com/repos/Drexel-UHC/salurbal_covid_dashboard/contents/Clean/covid19_processed_data_dynamic.rdata",
    authenticate("rl627@drexel.edu", "f72a8b5c5984910d715e1358e917ab74799a74ce")
  ),
  as = "parsed")
GET(req$download_url,
    write_disk(path = "tmp_covid.rdata", overwrite = T))
load("tmp_covid.rdata")


source("util.R",local = TRUE)
source("global.R",local = TRUE)

tidy.data.all = bind_rows(tidy.data.all.old,tidy.data.all.new) %>% 
  mutate(date = as.Date(date, origin = "1970-01-01")) %>% 
  arrange(level, country, loc, type_rate,date) 

tidy.daily = tidy.data.all  %>% 
  # filter(cum_value >0) %>% 
  # select(-cum_value)  %>%
  rename(value = daily_value) %>% 
  left_join(xwalk_data_rate) %>% 
  left_join(xwalk_data_rate_cleaned) %>% 
  left_join(xwalk_data_titles, 
            by = c("level", "country", 
                   "type", "rate")) %>% 
  left_join(xwalk_salid )

tidy.cumulative = tidy.data.all  %>% 
  rename(n = cum_value)%>% 
  left_join(xwalk_data_rate) %>% 
  left_join(xwalk_data_cum_rate_cleaned) %>% 
  left_join(xwalk_salid )

rm(tidy.data.all.old,tidy.data.all.new,tidy.data.all)



####  *************************** ####
server <- function(input, output) {
 
  Sys.sleep(1)
  waiter_hide()
  observeEvent("", {
    showModal(
      modalDialog( 
        easyClose = TRUE,
        div(id = "intro_modal_p1",
            img(src="LAC_icon.PNG", height = "100px"),
            h3("SALURBAL tracks COVID-19"),
            hr(),
            h5(intro_text, align = "center")
            
            
        ),
        footer = fluidRow(actionButton(inputId = "intro", 
                                       label = "Go to App!", 
                                       icon = icon("info-circle fa-blue")),
                          align = "center"
        )
      )
    )
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  
  ####  Tab 1: Compare Locations ####
  #### (Render UI) ####
  output$control_countries_1 = renderUI({
    req(length(input$level_1)>0)
    if (input$level_1 == "country"){
      pickerInput("countries_1",label = "Select Countries",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE),
                  choices = list(
                    SALURBAL = countries_salurbal,
                    "Other Central American countries" = other_central_countries,
                    "Other South American countries" = other_south_countries,
                    "Other Caribbean countries" = other_carribean_countries,
                    "Top 10 Countries by Cases" = countries_references),
                  selected = c(countries_salurbal))
    }
    else  {
      pickerInput("countries_1",label = "Select Country", 
                  choices = choices_df %>% filter(level ==input$level_1) %>% pull(country) %>% unique(),
                  selected = "Brazil")
    }
  })
  
  output$control_state_city_1 = renderUI({
    req(length(input$level_1)>0)
    req(length(input$countries_1)>0)
    if (input$level_1 == "state"){
      df_tmp =  choices_df %>% 
        filter(country == input$countries_1,
               level == input$level_1,
               type == "cases",
               rate == "count")
      states_top_tmp  = df_tmp %>%  filter(top == "top") %>% pull(state) 
      states_rest_tmp  = df_tmp %>% filter(top == "rest") %>% pull(state)
      label_tmp = paste("Select State")
      pickerInput("state_city_1",label = label_tmp,
                  multiple = TRUE, 
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE),
                  choices = list(
                    "Top 10 by outcome" = states_top_tmp,
                    "Others" =  states_rest_tmp),
                  selected = states_top_tmp)
    }
    else{
      df_tmp =  choices_df %>% 
        filter(country == input$countries_1,
               level == input$level_1 ,
               type == "cases",
               rate == "count")
      states_top_tmp  = df_tmp %>% filter(top == "top") %>% pull(state)
      states_rest_tmp = df_tmp %>% filter(top == "rest")  %>% pull(state)
      states_non_tmp = df_tmp %>% filter(top == "non")  %>% pull(state)
      label_tmp = paste("Select",ifelse(input$level_1=="L1","City", "Sub-city") )
      pickerInput("state_city_1",label = label_tmp,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE),
                  choices = list(
                    "Top 10 Largest by Outcome" = states_top_tmp,
                    "Others" =  states_rest_tmp,
                    "Non Salurbal" = states_non_tmp),
                  selected = states_top_tmp)
    }
  })
  

  
  output$control_rate_1 = renderUI({
    choices_tmp = c("Counts"="count",
                    "Rate"="rate")
    if (input$level_1 == "state"){choices_tmp = c("Counts"="count")}
    
    if (input$outcome_1 == "positivity"){
      radioGroupButtons(
        inputId = "rate_1",
        label = "Type",
        choices = choices_tmp,
        selected = "rate",
        size = "sm",
        justified = TRUE,
        disabled = T
      )
    }
    else {
      radioGroupButtons(
        inputId = "rate_1",
        label = "Type",
        choices = choices_tmp,
        size = "sm",
        justified = TRUE,
        # checkIcon = list(
        #   # yes = icon("ok",
        #   #            lib = "glyphicon")
        #   yes = icon("check-square"),
        #   no = icon("square-o")
        #   
        #   )
      )
    }
  })
  
  #### (Reactives) ####
  df_daily_react = reactive({
    if (input$level_1 == "country"){
      # input = list()
      # input$countries_1 = c("Chile","Argentina")
      # input$outcome_1 = "cases"
      # input$rate_1 = "rate"
      # input$outcome_1 = "positivity"
      # input$rate_1 = "count"
      df_daily = tidy.daily %>% 
        filter(level == "country") %>% 
        filter(loc%in%input$countries_1) %>% 
        filter(type==input$outcome_1) %>% 
        filter(rate == input$rate_1)
  
      df_daily
    }
    else {
      # input = list() 
      # input$countries_1 = "Mexico"
      # input$level_1 = "L1"
      # input$outcome_1 = 'positivity'
      # input$rate_1 = 'count'
      # input$rate_1 = 'rate'
      # input$state_city_1 = choices_df %>%
      #   filter(type == "cases") %>%
      #   filter(rate == "count") %>%
      #   filter(country == input$countries_1) %>%
      #   filter(level == input$level_1) %>%
      #   filter(top == "top") %>%
      #   pull(state)

      # tidy.daily %>% 
      #   filter(type == "positivity",level == "L1", country == "Argentina") %>% 
      #   filter(rate == "count") %>% 
      #   pull(value) %>% max()
      df_daily = tidy.daily %>% 
        filter(country == input$countries_1) %>% 
        filter(level == input$level_1) %>% 
        filter(type==input$outcome_1) %>% 
        filter(loc%in%input$state_city_1) %>% 
        filter(rate == input$rate_1)
    
      df_daily
    }
  })
  df_daily_react_debounce = debounce(df_daily_react,200)
  
  #### (Outputs) ####
  output$tab1_plot = renderHighchart({
    df_daily = df_daily_react_debounce()
    validate(
      need(nrow(df_daily)>0, "Trends in COVID-19 Testing and Positivity are only available at the City/Sub-city levels for Argentina, Guatemala and Mexico.")
    )
    # if (nrow(df_daily)>0){
      #w$show()
      # input = list();input$countries_1 =countries_salurbal;input$outcome_1 ="cases";input$rate_1="count"
      df_raw =  df_daily
      title_tmp = unique(df_raw$title)
      ylabs_tmp = unique(df_raw$ylabs)
      # subtitle_tmp = paste("Percent of peak is the % of the daily value relative to the highest daily count observed for each city.")
      
      df_tmp = df_raw %>% 
        select(date, value, loc) 
      
      hchart(df_tmp,"line",
             hcaes(x=date, y= value, group = loc),
             #color = df_tmp1$colors,
             #lineWidth = df_tmp1$width,
             
             #opacity = df_tmp1$opacity,
             #enableMouseTracking = df_tmp1$show,
             marker = list(
               enabled = F
             )
             #showInLegend = df_tmp1$show
      ) %>%
        hc_title(text = title_tmp) %>%
        # hc_subtitle(text = subtitle_tmp) %>% 
        hc_legend(layout = "vertical",
                  align = "right",
                  verticalAlign = "top",
                  y = 50) %>%
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = ylabs_tmp)) %>%
        hc_tooltip(borderColor = "#000000",
                   shared = F,
                   #split = TRUE,
                   borderWidth = 5) %>% 
        hc_add_theme(hc_theme_smpl())%>% 
        hc_exporting(
          enabled = TRUE
        )
    # }
  })
  #### Tab 2: Location Specific ####
  ####(Reactive Values/Events) ####
  rv_tab <- reactiveVal("Daily Trends")
  observeEvent(input$countries_2,{
    rv_tab(input$tabs)
  })
  observeEvent(input$state_city_2,{
    rv_tab(input$tabs)
  })
  ####(Render UI) ####
  output$report_outputs_ui = renderUI({
    req(input$level_2)
    if (input$level_2!="L1"){
      tabsetPanel(
        tabPanel("Daily Trends",
                 column(width = 12,
                        highchartOutput("plot_rolling", height = "450")
                        # plotOutput("plot_rolling", height = "450")
                 ))
      )
    }
    else{
      tabsetPanel(id = "tabs",
                  selected = rv_tab(),
                  tabPanel("Daily Trends",
                           column(width = 12,
                                  highchartOutput("plot_rolling", height = "450")
                                  # plotOutput("plot_rolling", height = "450")
                           )),
                  tabPanel("Sub-City Map",
                           column(width = 8,
                                  h3(paste0(input$state_city_2,", ",input$countries_2),align="center"),
                                  hr(),
                                  column(12, align='center',
                                         tagList(
                                           pickerInput("location_map_outcome",
                                                       choices = c("Incidence (Cases per 1M)"="cases",
                                                                   "Testing (Tests per 1M)"="tests",
                                                                   "Testing Positivity (%)"="positivity",
                                                                   "Mortality (Deaths per 10M)"="deaths"
                                                                   ),
                                                       selected = "cases"),
                                           
                                           leafletOutput("report_subcity_map")
                                         ))
                                  
                           ))
      )
    }
  })
  
  output$control_countries_2 = renderUI({
    if (input$level_2 == "country"){
      pickerInput("countries_2",label = "Select Countries",
                  multiple = FALSE,
                  options = list(`live-search` = TRUE),
                  choices = list(
                    SALURBAL = countries_salurbal,
                    LAC = other_lac_countries),
                  # choicesOpt = list(
                  #   icon = helper2$icon),
                  selected = "Argentina")
    }
    else if (input$level_2%in%c("state","L1","L2")){
      pickerInput("countries_2",label = "Select Country",
                  multiple = FALSE,
                  options = list(`live-search` = TRUE),
                  choices = choices_df %>% filter(level ==input$level_2) %>% pull(country) %>% unique(),
                  selected = "Brazil")
    }
  })
  
  output$control_state_city_2 = renderUI({
    req(input$countries_2)
    if (input$level_2 == "country"){}
    else if (input$level_2 == "state"){
      top1_tmp = choices_df%>%filter(type=="cases")  %>% filter(level == "state",rate =="count",country == input$countries_2) %>% filter(top1 == "best") %>% pull(state)
      states_top_tmp  = choices_df%>%filter(type=="cases")  %>% filter(level == "state",rate =="count",country == input$countries_2) %>% filter(top == "top") %>% pull(state)
      states_rest_tmp  = choices_df%>%filter(type=="cases")  %>% filter(level == "state",rate =="count",country == input$countries_2) %>% filter(top == "rest") %>% pull(state)
      pickerInput("state_city_2",label = "Select State",
                  multiple = FALSE,
                  options = list(`live-search` = TRUE),
                  choices = list(
                    "Top 10 Largest" = states_top_tmp,
                    "Others" =  states_rest_tmp),
                  selected = top1_tmp)
    }
    else if (input$level_2%in%c("L1","L2")){
      # input = list()
      # input$level_2 = "L1"
      # input$countries_2="Chile"
      top1_tmp = choices_df %>%filter(type=="cases") %>%  filter(level == input$level_2,rate =="count",country == input$countries_2,top == "top") %>% filter(top1 == "best") %>% pull(state)
      states_top_tmp  = choices_df %>%filter(type=="cases") %>% filter(level == input$level_2,rate =="count",country == input$countries_2,top == "top") %>% pull(state)
      states_rest_tmp  = choices_df %>%filter(type=="cases")%>% filter(level == input$level_2,rate =="count",country == input$countries_2) %>% filter(top == "rest") %>% pull(state)
      states_non_tmp = choices_df%>%filter(type=="cases") %>% filter(level == input$level_2,rate =="count",country == input$countries_2) %>% filter(top == "non") %>% pull(state)
      pickerInput("state_city_2",label = paste("Select",ifelse(input$level_2=="L1","City","Sub-city")),
                  multiple = FALSE,
                  options = list(`live-search` = TRUE),
                  choices = list(
                    "Top 10 Largest" = states_top_tmp,
                    "Others" =  states_rest_tmp,
                    "Non Salurbal" = states_non_tmp),
                  selected = top1_tmp)
    }
  })
  #### (Reactives) ####
  sub_city_level_cumulative_react = reactive({
    print("Start sub_city_level_cumulative_react")
    req(input$level_2)
    req(input$countries_2)
    req(input$state_city_2)
    req(input$location_map_outcome)
    # input = list()
    # input$level_2 = "L1"
    # input$countries_2="Argentina"
    # input$state_city_2 = "Buenos Aires"

    # input$countries_2="Argentina"
    # input$state_city_2 = "Buenos Aires"
    # input$location_map_outcome="tests"
    # input$location_map_outcome="positivity"
    if(input$level_2 == "L1") {
      salid1_tmp = tidy.daily %>% 
        filter(level == input$level_2) %>% 
        filter(country == input$countries_2) %>% 
        filter(loc == input$state_city_2) %>% 
        pull(salid) %>% unique()
      if(length(salid1_tmp)>0) {
        df_tmp = tidy.cumulative %>% 
          filter(level == "L2") %>% 
          filter(rate == "rate") %>% 
          filter(type == input$location_map_outcome) %>% 
          filter(country == input$countries_2) %>% 
          filter(str_detect(salid,salid1_tmp)) %>% 
          group_by(country, loc, salid, rate_cleaned) %>% 
          group_modify(~.x %>%  
                         filter(!is.na(n)) %>% 
                         filter(date == max(date))) %>% 
          ungroup() %>% 
          select(country, loc, salid2 = salid,n, rate_cleaned)
        print("end sub_city_level_cumulative_react")
        df_tmp
      }
    }
  })
  
  location_daily_react = reactive({
    print("Start location_daily_react")
    # w$show()
    req(input$level_2)
    req(input$countries_2)
    req(input$state_city_2)
    # input = list();
    # input$level_2 = "L1";
    # input$countries_2 = "Guatemala";
    # input$state_city_2 = "Ciudad de Guatemala"
    # input$level_2 = "country"; 
    # input$countries_2 = "Argentina"
    df_tmp = tidy.daily %>% 
      filter(type!="positivity")%>% 
      filter(type!="tests") %>% 
      filter(level == input$level_2) %>% 
      filter(loc == input$countries_2) %>% 
      filter(rate == "count") %>% 
      rename(rollsum= value)
    
    if (input$level_2 != "country"){
      req(!is.na(input$state_city_2))
      df_tmp = tidy.daily  %>% 
        filter(type!="positivity") %>% 
        filter(level == input$level_2) %>% 
        filter(country == input$countries_2) %>% 
        filter(loc == input$state_city_2)  %>% 
        filter(rate == "count") %>% 
        rename(rollsum= value)
      # if (input$countries_2 == "Chile"){
      #   df_tmp=df_tmp%>% 
      #     mutate(rollsum=ifelse( (type == "deaths"&date<"2020-06-08"),NA,rollsum   ))
      # }
    }
    print("End location_daily_react")
    df_tmp
    
  })
  
  sub_city_level_cumulative_react_debounce = sub_city_level_cumulative_react %>% debounce(100)
  location_daily_react_debounce = location_daily_react %>% debounce(100)
  # df_tmp = tidy.daily.subnational %>%
  #   filter(level == "L1",
  #          country=="Chile",
  #          loc == "Santiago"
  #          ) %>%
  #   mutate(rollsum=ifelse( (type == "deaths"&date<"2020-06-08"),NA,rollsum   ))
  # plot_lockdown_effect_salurbal(dfa)
  # 
  
  #### (Outputs) ####
  output$report_subcity_map = renderLeaflet({
    req(sub_city_level_cumulative_react_debounce())
    if (input$level_2 == "L1"){
      df_tmp =  sub_city_level_cumulative_react_debounce()
      ###$$$$$$
      validate(
        need(nrow(df_tmp)>0, "Sub-city Cumulative Testing Rates are only available at the City levels for Argentina, Guatemala and Mexico.")
      )
      legend_tmp =  ifelse(input$location_map_outcome=="cases",
                           "Cases per 1M",
                           "Deaths per 10M")
      sf_tmp = sf_l2_simp %>%
        left_join(df_tmp) %>% 
        filter(!is.na(loc))
      pal = colorNumeric("OrRd",domain = sf_tmp$n, reverse = F)
      sf_tmp %>% 
        leaflet(options = leafletOptions(preferCanvas = TRUE,
                                         zoomControl = FALSE)) %>% 
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        addPolygons(weight = 1, 
                    fillOpacity = 1,
                    fillColor = ~pal(n),
                    label = ~paste(loc,":",format(n,big.mark=",")),
                    color = I("black")) #%>%
        # addLegend(data = sf_tmp %>% filter(!is.na(n)),
        #           position = "bottomright",
        #           pal = pal,
        #           values = ~n,
        #           title = legend_tmp,
        #           opacity = 1) 
    }
  })
  
  output$tab2_diag = renderDataTable(location_daily_react())
  
  output$plot_rolling = renderHighchart({
    req(nrow(location_daily_react_debounce())>0)
    df_tmp = location_daily_react_debounce()
    n_outcomes = length(unique(df_tmp$type))
    title_tmp  = case_when(
      input$level_2=="country"~paste0("Normalized Trends of Cases and Deaths in ",input$countries_2),
      input$level_2!="country"&n_outcomes==2~ paste0("Normalized Trends of Cases and Deaths in ",input$state_city_2,", ",input$countries_2),
      input$level_2!="country"&n_outcomes==3~ paste0("Normalized Trends of Cases,Testing Positivity and Deaths in ",input$state_city_2,", ",input$countries_2)
      )
    df_tmp2 = df_tmp %>%
      select(type, date, rollsum) %>%
      #drop_na() %>%
      left_join(df_tmp %>% #drop_na() %>%
                  group_by(type) %>%
                  summarize(max_val = max(rollsum)) %>%
                  ungroup()) %>%
      mutate(norm = round(rollsum/max_val*100,0)) %>%
      mutate( type = recode(type,
                            "cases"="Confirmed Cases",
                            "deaths"="Deaths",
                            "tests"="% Tests Positive"))
    
    highchart() %>%
      hc_add_series(data = df_tmp2,type ="line",
                    marker = list(
                      enabled = F
                    ),
                    hcaes(x=date,y= norm,group = type)#,
                    # tooltip = list(pointFormat = "Shop Time")
      ) %>%
      hc_yAxis(title = list(text = "% Peak")) %>%
      hc_title(text = title_tmp) %>%
      hc_subtitle(text =  "Percent of peak is the % of the daily value relative to the highest daily value for each outcome.") %>%
      hc_xAxis(type = "datetime",
               labels = list(format = '{value:%m/%d}'))%>%
      hc_legend(layout = "vertical",
                align = "right",
                verticalAlign = "top",
                y = 50) %>%
      hc_tooltip(borderColor = "#000000",
                 shared = TRUE,
                 #split = TRUE,
                 borderWidth = 5,
                 pointFormat = " <span style='color:{point.color}'>●</span>
                 {series.name}: {point.y} % <br>")%>%
      hc_add_theme(hc_theme_smpl())%>%
      hc_exporting(
        enabled = TRUE
      )
    
  })
  
  # output$plot_rolling  = renderCachedPlot({
  #   req(nrow(location_daily_react())>0)
  #   if (input$level_2 =="country"){
  #     plot_daily_country(location_daily_react()) 
  #   }
  #   else {
  #     plot_lockdown_effect_salurbal(location_daily_react()) 
  #     #plot_lockdown_effect_salurbal(df_tmp) 
  #   }
  # },
  # cacheKeyExpr = { list(input$countries_2,location_daily_react()) })
  # 
  # 
  #### Tab 3: Map ####
  #### (Reactive) ####
  
  map_daily_data_react = reactive({
    df_tmp = df_map_data %>% 
      filter(level ==input$map_level) %>% 
      filter(date == input$plot_date) %>% 
      filter(rate == input$type_map) 
    
    if ((input$map_level == "L1")&(input$type_map%in%c("confirmed_rate","deaths_rate"))){
      df_tmp %>%
        filter(country%in%input$map_countries)
    }
    else {df_tmp}
    
  })
  
  current_global_cases_react = reactive({map_global_totals %>% filter(date == input$plot_date) %>% pull(confirmed)})
  current_global_deaths_react = reactive({map_global_totals %>% filter(date == input$plot_date) %>% pull(deaths)})
  
  
  #### (Output) ####
  output$map_text_date = renderText(map_daily_data_react() %>% slice(1) %>% pull(date_label))
  output$current_global_cases = renderText( paste("Total Global Cases:\n",format(current_global_cases_react(),big.mark=",") ))
  output$current_global_deaths = renderText( paste("Total Global Deaths:\n",format(current_global_deaths_react(),big.mark=",") ))
  
  output$map_country_filter = renderUI({
    if (input$map_level == "L1"&(input$type_map%in%c("confirmed_rate","deaths_rate"))){
      # input = list(); input$type_map = "confirmed_rate"
      countries_tmp = df_map_data %>% 
        filter(level == "L1") %>% 
        filter(rate == input$type_map) %>% 
        pull(country) %>% 
        unique()
      
      pickerInput(
        inputId = "map_countries",
        label = "Fitler Countries:", 
        choices = countries_tmp,
        options = list(
          `actions-box` = TRUE), 
        selected = countries_tmp,
        multiple = T
      )
    }
  })
  # output$mapper = renderLeaflet({
  #   basemap 
  # })
  
  output$mapper = renderLeaflet({
    basemap = leaflet(options = leafletOptions(preferCanvas = TRUE,
                                               zoomControl = FALSE)) %>% 
      # addTiles() %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -60.1, lat = -16,zoom = 3.25) %>% 
      addPolygons(data = sf_world,
                  weight = 1, 
                  color = I("black"),
                  fillOpacity = 0,
                  label = ~loc)
    basemap
  })
  
  output$mapper_add_marker = renderPlot({
    # df_tmp = df_map_data %>%
    #   filter(level =="L1") %>%
    #   filter(date == max(subset_dates_tmp)) %>%
    #   filter(rate == "deaths_rate") %>%
    #   filter(country%in%c("Brazil"))
    
    df_tmp =  map_daily_data_react()
    legend_tmp =  case_when(input$type_map == "confirmed_rate"~"New Cases per 1M",
                            input$type_map == "deaths_rate"~"New Deaths per 10M")
    if (input$type_map%in%c("confirmed","deaths") ){
      
      leafletProxy("mapper") %>%
        clearMarkers() %>% 
        leaflet::clearGroup("rateStuffCountry")%>%
        leaflet::clearGroup("Hexagons")%>%
        clearControls()%>%
        addCircleMarkers(data = df_tmp, 
                         lat = ~lat, lng = ~lng, 
                         radius=~radius,
                         label  = ~paste(loc,":",format(n,big.mark=",")),
                         fillOpacity = 0.4,
                         color = "red", weight = 0)
    }
    else if (input$map_level == "country"&(str_detect(input$type_map,"rate")) ){
      legend_tmp =  case_when(input$type_map == "confirmed_rate"~"Cases per 1M",
                              input$type_map == "deaths_rate"~"Deaths per 10M")
      sf_tmp = sf_world %>% 
        left_join(df_tmp)  %>% 
        mutate(fill = ifelse(is.na(fill),"#d3d3d3",fill))
      pal = colorNumeric("OrRd",
                         domain = sf_tmp$n, 
                         reverse = F)
      leafletProxy("mapper") %>%
        clearMarkers()%>% 
        leaflet::clearGroup("Hexagons")%>%
        leaflet::clearGroup("Polygons")%>%
        clearControls() %>% 
        addPolygons(data = sf_tmp,
                    weight = 1, 
                    fillOpacity = 1,
                    label = ~paste(loc,":",format(n,big.mark=",")),
                    color = I("black"),
                    fillColor  = ~fill,
                    group = "rateStuffCountry") %>%
        addLegend(data = sf_tmp %>% filter(!is.na(n)),
                  position = "topright",
                  pal = pal,
                  values = ~n,
                  title = legend_tmp,
                  opacity = 1)  
    }
    else if (input$map_level == "L1"&(str_detect(input$type_map,"rate")) ){
      legend_tmp =  case_when(input$type_map == "confirmed_rate"~"Cases per 1M",
                              input$type_map == "deaths_rate"~"Deaths per 10M")
      
      hex_sf_tmp = sf_salurbal_0.8 %>% 
        left_join(df_tmp, by = "salid1") %>% 
        mutate(opacity = ifelse(is.na(n),0,1)) %>% 
        select(name,opacity,n,salid1  ) 
      # sf_salurbal_0.8 %>% filter(name=="Palmas")
      # df_tmp %>% filter(salid1=="102255")
      # hex_sf_tmp%>% filter(salid1=="102255")
      l1_sf_tmp = salurbal_l1_sf %>% 
        left_join(df_tmp, by = "salid1") %>% 
        mutate(opacity = ifelse(is.na(n),0,1)) %>% 
        select(name,opacity,n ,salid1 )
      # sf_salurbal_0.8 %>% filter(name=="Palmas")
      # df_tmp %>% filter(salid1=="102255")
      # l1_sf_tmp%>% filter(salid1=="102255")      
      
      pal = colorNumeric("OrRd",
                         domain = hex_sf_tmp$n, 
                         reverse = F)
      leafletProxy("mapper") %>%
        # basemap%>%
        clearMarkers()%>% 
        leaflet::clearGroup("rateStuffCountry")%>%
        clearControls() %>%
        addMapPane("Hexagons", zIndex = 420) %>% # below
        addMapPane("Polygons", zIndex = 430) %>% # above
        addPolygons(data = hex_sf_tmp,
                    weight = 1, 
                    fillOpacity = ~opacity,
                    label = ~paste(name,":",format(n,big.mark=",")),
                    color = I("black"),
                    fillColor  = ~pal(n),
                    options = pathOptions(pane = "Hexagons"),
                    group = "Hexagons", 
                    layerId = paste0("a",1:371)) %>%
        addPolygons(data = l1_sf_tmp,
                    weight = 1, 
                    fillOpacity = ~opacity,
                    label = ~paste(name,":",format(n,big.mark=",")),
                    color = I("black"),
                    fillColor  =  ~pal(n),
                    options = pathOptions(pane = "Polygons"),
                    group = "Polygons", 
                    layerId = paste0("b",1:371)) %>%
        addLegend(data = hex_sf_tmp %>% filter(!is.na(n)),
                  position = "topright",
                  pal = pal,
                  values = ~n,
                  title = legend_tmp,
                  opacity = 1) %>% 
        addLayersControl(
          overlayGroups = c("Polygons","Hexagons"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup("Polygons")
    }
    
  })
  
  
  #### Tab 4: Data ####
  #### (Reactive) ####
  
  data_output_raw_react = reactive({
    tidy.cumulative%>% 
      filter(type%in%input$data_type4) %>% 
      filter(level%in%input$data_level4)  %>% 
      filter(rate%in%input$rate_4)
    
  })
  
  data_output_clean_react = reactive({
    data_output_raw_react() %>% 
      select(Outcome = type,
             Level = level,
             Type = rate_cleaned,
             Country = country,
             Location = loc,
             Date = date,
             Value = n)
    
  })
  
  
  #### (Output) ####
  
  output$data_available = renderDataTable({
    datatable(
      data_output_raw_react()%>%
        filter(level!="country") %>% 
        select(country, level, outcome = type) %>%
        count(level,outcome,country ) %>%
        select(-n) %>% 
        group_by(level, outcome) %>% 
        summarise(countries = paste(country, collapse = ", ")) %>% 
        ungroup()%>% 
        select(Level = level, Outcome =outcome, Countries = countries),
      rownames= FALSE
    )
    
  })
  
  output$data_dt = renderDataTable(datatable(data_output_clean_react(),
                                             rownames= FALSE))
  
  output$data_daily_dt = renderDataTable({
    datatable(tidy.daily%>% 
                filter(type%in%input$data_type4) %>% 
                filter(level%in%input$data_level4)  %>% 
                filter(rate%in%input$rate_4) %>% 
                select(Outcome = type,
                       Level = level,
                       Type = rate_cleaned,
                       Country = country,
                       Location = loc,
                       Date = date,
                       Value = value),
              rownames= FALSE) 
  })
  output$download_data = downloadHandler(
    filename = function() {
      paste(input$data_level4,"level covid19",input$outcome3,".csv")
    },
    content = function(file) {
      write.csv(data_output_clean_react(), file)
    }
  )
}



#### ***************************** ####

shinyApp(ui = ui, server = server)

