rm(list=ls())
library(rintrojs)
library(cowplot)
library(shiny)
library(waiter)
library(data.table)
library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(janitor)
library(scales)
library(gridExtra)
library(DT)
library(sparkline)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(dplyr)
options(scipen = 99)
load("covid19_processed_data.rdata")


shinyOptions(cache = diskCache("./covid19app-cache"), max_size = 200e6, max_age =25200)



# github_data_url = url("https://github.com/rl627/salurbal_covid19_data/raw/master/Clean/covid19_processed_data.rdata")
# load(github_data_url)



source("util.R",local = TRUE)


intro_text = "The 'SALURBAL tracks COVID-19' app allows users to tailor visualizations 
to highlight COVID-19 cases and deaths in Latin American countries, including those that 
are part of the SALURBAL project. In some cases, city and sub-city level data is available. 
The data is automatically updated each day to add cases and deaths reported by country and 
city governments the previous day." 

# make loading screen html
loading_screen = tagList(
  tags$img(
    src="LAC_logo.png",
    height=200,
    id = "myImage" # set id
  ),br(),
  spin_loader(),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
 
)

logo_footer = tagList(
  hr(class = "salurbal_hr"),
  absolutePanel(left = 20, fixed=F, draggable = FALSE, height = "auto",
                tags$a(href='https://drexel.edu/lac/salurbal/overview/', tags$img(src='SALURBAL_logo1.jpg',height='80'))),
  absolutePanel(left = 140, fixed=F, draggable = FALSE, height = "auto",
                tags$a(href='https://drexel.edu/uhc/', tags$img(src='UHC_logo.png',height='80'))),
  br(),br(),br(),br(),br()
  
)

####  *************************** ####
ui = function(){
  ui <- fluidPage(#theme = shinytheme("flatly"),
                  use_waiter(include_js = F),
                  introjsUI(),
                  tags$link(
                    rel = "stylesheet", 
                    href="https://fonts.googleapis.com/css?family=Source+Sans+Pro"
                  ),
                  tags$head(includeCSS("styles.css")),
                  

                  
                  
                  #shinythemes::themeSelector(),
                  fluidRow(
                    column(4, tags$a(href='https://drexel.edu/lac/', img(class ="header-logo",src='LAC_logo.png', height = "125px"))),
                    column(8, div(class="header-brand","COVID-19 in SALURBAL Countries"))
                  ),
                  navbarPage(title = "COVID-19 in SALURBAL Countries",
                             
                             ####  Tab 1:Cumulative ####       
                             tabPanel("Total cases and deaths",
                                      sidebarLayout(
                                        sidebarPanel(width  = 3,
                                                     fluidRow(column(width = 12,strong("Data Options"),align = 'center')),
                                                     pickerInput("level_1","Level",
                                                                 choices = c("Country"="country",
                                                                             "State"="state",
                                                                            "City"=  "L1",
                                                                            "Sub-city"="L2")),
                                                     uiOutput('control_countries_1'),
                                                     conditionalPanel("input.level_1 != 'country'",
                                                                      uiOutput('control_state_city_1')),
                                                     fluidRow(column(width = 12,strong("Visualization Options"),align = 'center')),
                                                     uiOutput("control_outcome_1"),
                                                     uiOutput("control_rate_1"),
                                                     radioGroupButtons("time_type",
                                                                       "Start date",
                                                                       choices = c("Since onset" = "onset",
                                                                                   "Calendar Time"="true"),
                                                                       justified = TRUE,
                                                                       size = "sm",
                                                                       checkIcon = list(
                                                                         yes = icon("ok",
                                                                                    lib = "glyphicon"))
                                                     )
                                        ),
                                        mainPanel(width  = 9,
                                                  column(width = 5, 
                                                         DT::dataTableOutput('dt1'),
                                                         em("Tip: Click rows in the Data Table to highlight countries or cities!")),
                                                  column(width = 7,
                                                         plotOutput("plot_count", height = "550" )
                                                  )
                                        )
                                      ),
                                      fluidRow(logo_footer)
                                      
                             ),
                             ####  Tab 2: Daily ####    
                             tabPanel("Daily cases and deaths",
                                      sidebarLayout(
                                        sidebarPanel(width  = 3,
                                                     pickerInput("level_2","Level",
                                                                 choices = c("Country"="country",
                                                                             "State"="state",
                                                                             "City"="L1",
                                                                             "Sub-city"="L2")),
                                                     uiOutput("control_countries_2"),
                                                     uiOutput("control_state_city_2")#,
                                                     #em("Note: We recognize there is heterogeneity in various national responses to COVID-19 and not all 'National lockdowns' are identical. Currently we using a very hueristic definition of national lock down and are working to adopt a more globally consistent definition.")
                                                     
                                        ),
                                        
                                        mainPanel(width  = 9,
                                                  column(width = 10,
                                                         plotOutput("plot_rolling", height = "450")
                                                  )
                                        )
                                      ),
                                      fluidRow(logo_footer)
                             ),
                             ####  Tab 3: Map ####            
                             tabPanel("Map",
                                      fluidRow(
                                        div(class="outer",
                                            leafletOutput("mapper", width="100%", height="600px"),
                                            plotOutput('mapper_add_marker', height="600px"),
                                            
                                            
                                            absolutePanel(id = "controls",
                                                          class = "panel panel-default",
                                                          top = 20, left = 20, width = 250,
                                                          draggable = TRUE, height = "auto",
                                                          h3("COVID-19 Spread Over Time"),
                                                          h4(textOutput("map_text_date")),
                                                          h4(textOutput("current_global_cases")),  
                                                          h4(textOutput("current_global_deaths")), 
                                                          
                                                          pickerInput(
                                                            inputId = "type_map",
                                                            label = "Outcome to Visualize", 
                                                            choices = c("Confirmed Cases" ="confirmed", 
                                                                        "Confirmed Cases per 1M" = "confirmed_rate" , 
                                                                        "Deaths"="deaths",
                                                                        "Deaths per 10M"="deaths_rate")
                                                          ),
                                                          pickerInput(
                                                            "map_level",
                                                            label = "Map Level",
                                                            choices = c("Country Level"="country",
                                                                        "City" = "L1")
                                                            
                                                          ),
                                                          sliderInput("plot_date",
                                                                      label = h5("Select mapping date"),
                                                                      min = min(map_daily_data$date),
                                                                      max = max(map_daily_data$date),
                                                                      step  = 1,
                                                                      value = max(map_daily_data$date),
                                                                      timeFormat = "%d %b", 
                                                                      animate=animationOptions(interval =300, loop = FALSE))
                                            )
                                        )
                                      ),
                                      fluidRow(logo_footer)
                                      
                             ),
                             ####  Tab 4: Data  ####
                             tabPanel("Data",
                                      sidebarLayout(
                                        sidebarPanel(width  = 3,
                                                     # textOutput("text_scr"),
                                                     checkboxGroupInput(
                                                       inputId = "data_type4",
                                                       label = "Outcome", 
                                                       choices = c("Cases"="cases",
                                                                   "Deaths"="deaths"),
                                                       selected = c("cases","deaths")
                                                     ),
                                                     # textOutput("text_scr"),
                                                     checkboxGroupInput(
                                                       inputId = "rate_4",
                                                       label = "Data Type", 
                                                       choices = c("Counts"="count", 
                                                                   "Rate"="rate"),
                                                       selected = c("count","rate")
                                                     ),
                                                     checkboxGroupInput(
                                                       inputId = "data_level4",
                                                       label = "Level", 
                                                       choices = c("Country"="country",
                                                                   "State"="state",
                                                                   "City"="city",
                                                                   "Sub-city"="sub-city"),
                                                       selected = c("country","state","city","sub-city")
                                                     ),
                                                     # pickerInput("data_level4","Level",
                                                     #             choices = c("Country"="country",
                                                     #                         "State"="state",
                                                     #                         "L1",
                                                     #                         "L2")),
                                                     downloadButton("download_data", "Download CSV")
                                        ),
                                        
                                        mainPanel(width  = 9,
                                                  tabsetPanel(type = "tabs",
                                                              tabPanel("Cumulative Data", DT::dataTableOutput('data_dt')),
                                                              
                                                              tabPanel("Subnational Availability", tableOutput('data_available')))
                                                  
                                                  
                                        ) 
                                      ),
                                      fluidRow(logo_footer)
                             ),
                             
                             
                             ####  Tab 5: About ####    
                             tabPanel("About App",
                                    
                                      
                                      h4("About SALURBAL"),
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
                                      "Brazil State Level Data:",tags$a(href="https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv",
                                                                        "https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv"),br(),
                                      "Brazil Municipal Level Data:",tags$a(href="https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
                                                                            "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"),br(),
                                      "Mexico State Level Cases:",tags$a(href="https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_casos_totales.csv",
                                                                         "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_casos_totales.csv"),br(),
                                      "Mexico State Level Deaths:",tags$a(href="https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_muertes.csv",
                                                                          "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_muertes.csv"),br(),
                                      "Mexico Municipal Level Cases:",tags$a(href="https://www.gob.mx/salud/documentos/datos-abiertos-152127",
                                                                             "https://www.gob.mx/salud/documentos/datos-abiertos-152127"),br(),
                                      "Chile Municipal Level Cases:",tags$a(href="https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto1/Covid-19.csv",
                                                                            "https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto1/Covid-19.csv"),br(),
                                      "Colombia Municipal Level Data:",tags$a(href="https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data",
                                                                              "https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data"),br(),
                                      "Peru Municipal Level Data:",tags$a(href="https://www.datosabiertos.gob.pe/sites/default/files/DATOSABIERTOS_SISCOVID.zip",
                                                                          "https://www.datosabiertos.gob.pe/sites/default/files/DATOSABIERTOS_SISCOVID.zip"),br(),
                                      h4("Version updates:"),
                                      tags$ul(
                                        tags$li("5/6/2020: Subnational data for Chile and Colombia. Added City level visualizations to map."),
                                        tags$li("5/20/2020: User Interface update."),
                                        tags$li("5/29/2020: Added Peru Subnational Data. Fixed label and axis issues for plot in 'Total cases and deaths' tab.")
                                      ),
                                      
                                      h4("Authors"),
                                      "Ran Li, Data Analyst at SALURBAL",br(),
                                      "Usama Bilal, Co-Investigator at SALURBAL",br() ,br() ,br() ,
                                      fluidRow(logo_footer)
                             )
                  ),
                  waiter_show_on_load(
                    html = loading_screen,
                    color = "#f0fcfc"
                  )
  )
}
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
  
  w <- Waiter$new(id = c("plot_count", "plot_rolling"),
                  html = spin_refresh(),
                  color = "$ffffff00")
  ####  Tab 1: Cumulative (Render UI) ####
  
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
  
  output$control_outcome_1 = renderUI({
    choices_tmp = c("Cases"="cases",
                    "Deaths"="deaths")
    
    if(input$level_1 == "country"){
      radioGroupButtons("outcome_1",
                        "Outcome",
                        choices = choices_tmp,
                        justified = T,
                        size = "sm",
                        checkIcon = list(
                          yes = icon("ok",
                                     lib = "glyphicon")))
    }
    else {
      available_tmp = choices_df %>%
        filter(level == input$level_1) %>%
        filter(country%in%input$countries_1) %>%
        filter(state%in%input$state_city_1) %>%
        pull(type) %>%
        unique()
      if (length(available_tmp)==2){
        radioGroupButtons("outcome_1",
                          #"Plot Outcome",
                          choices = choices_tmp,
                          justified = T,
                          size = "sm",
                          checkIcon = list(
                            yes = icon("ok",
                                       lib = "glyphicon")))
      }
      else{
        radioGroupButtons("outcome_1",
                          #"Plot Outcome",
                          choices = c("Cases"="cases"),
                          justified = T,
                          size = "sm",
                          checkIcon = list(
                            yes = icon("ok",
                                       lib = "glyphicon")))
        
      }
    }
  })
  
  output$control_rate_1 = renderUI({
    choices_tmp = c("Counts"="count",
                    "Rate"="rate")
    if (input$level_1 == "state"){choices_tmp = c("Counts"="count")}
    radioGroupButtons(
      inputId = "rate_1",
      "Type",
      choices = choices_tmp,
      size = "sm",
      justified = TRUE,
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon"))
    )
  })
  #### Tab 1: Cumulative (Reactives) ####
  dt2_global = reactive({
    w$show()
    req(!is.na(input$level_1)  )
    req(!is.na(input$countries_1))
    #req(!is.na(input$state_city_1))
    req(!is.na(input$outcome_1))
    req(!is.na(input$rate_1))
    req(!is.na(input$time_type))
    
    loc_name_tmp = case_when(
      input$level_1 == "country"~"Country",
      input$level_1 == "state"~"State",
      input$level_1 == "L1"~"Cities",
      input$level_1 == "L2"~"Sub-cities")
    type_name_tmp = case_when(
      input$outcome_1=="cases"&input$rate_1 =="rate"~"Cases per 1M",
      input$outcome_1=="deaths"&input$rate_1 =="rate"~"Deaths per 10M",
      input$outcome_1=="cases"~"Cases",
      input$outcome_1=="deaths"~"Deaths")    
    df_tmp = `dt1_global` %>% 
      filter(level == input$level_1) %>% 
      filter(type == input$outcome_1) %>% 
      filter(rate == input$rate_1)
    if(input$level_1 == "country"){df_tmp = df_tmp%>% filter(loc%in%input$countries_1)}
    else {df_tmp = df_tmp %>% filter(country == input$countries_1) %>% filter(loc%in%input$state_city_1)}
    df_tmp =df_tmp  %>% 
      mutate(n = format(n,big.mark=",")) %>% 
      select(loc,Trend = spark, n) %>% 
      arrange(desc(n)) %>% 
      rename(!!loc_name_tmp := loc) %>% 
      rename(!!type_name_tmp := n)
    
  })
  
  DT_selected_countries = reactive({
    if(length(input$dt1_rows_selected)==0){NA_character_}
    else {
      if(input$level_1 =="country"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(Country)}
      else if (input$level_1 =="state"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(State)}
      else if (input$level_1 =="L1"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(Cities)}
      else if (input$level_1 =="L2"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(`Sub-cities`)}
    }
  })
  
  df_c_react  = reactive({ 
    w$show()
    req(!is.na(input$level_1)  )
    req(!is.na(input$countries_1))
    #req(!is.na(input$state_city_1))
    req(!is.na(input$outcome_1))
    req(!is.na(input$rate_1))
    req(!is.na(input$time_type))
    w$show()
    df_level_tmp = df.c %>% filter(level == input$level_1)  %>% filter(rate == input$rate_1) 
    req(nrow(df_level_tmp)>0)
    if(input$level_1 == "country"){df_level_tmp = df_level_tmp %>% filter(loc%in%input$countries_1) }
    else {df_level_tmp = df_level_tmp %>% filter(country == input$countries_1) %>% filter(loc%in%input$state_city_1)}
    if( is.na(DT_selected_countries()) ){df_level_tmp %>% arrange(loc)%>% mutate(size = 0.5)}
    else{df_level_tmp %>% 
        mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
               colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
               size = ifelse(loc != DT_selected_countries(),0.25,1.5)) %>% 
        arrange(loc)}
  })
  
  df_d_react  = reactive({ 
    w$show()
    req(!is.na(input$level_1)  )
    req(!is.na(input$countries_1))
    #req(!is.na(input$state_city_1))
    req(!is.na(input$outcome_1))
    req(!is.na(input$rate_1))
    req(!is.na(input$time_type))
    
    df_level_tmp = df.d %>% filter(level == input$level_1) %>% filter(rate == input$rate_1) 
    if(input$level_1 == "country"){df_level_tmp = df_level_tmp %>% filter(loc%in%input$countries_1)}
    else {df_level_tmp = df_level_tmp %>% filter(country == input$countries_1) %>% filter(loc%in%input$state_city_1)}
    
    if( is.na(DT_selected_countries()) ){df_level_tmp %>% arrange(loc)%>% mutate(size = 0.5)}
    else{df_level_tmp %>% 
        mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
               colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
               size = ifelse(loc != DT_selected_countries(),0.25,1.5)) %>% 
        arrange(loc)}
  })
  
  
  
  full_react = reactive({ 
    w$show()
    req(!is.na(input$level_1)  )
    req(!is.na(input$countries_1))
    #req(!is.na(input$state_city_1))
    req(!is.na(input$outcome_1))
    req(!is.na(input$rate_1))
    req(!is.na(input$time_type))
    full_tmp = full_global %>%
      filter(type == input$outcome_1) %>% 
      filter(level == input$level_1) %>%
      filter(rate == input$rate_1)
    if(input$level_1 == "country"){full_tmp = full_tmp %>% filter(loc%in%input$countries_1)}
    else {full_tmp = full_tmp %>% filter(country == input$countries_1) %>% filter(loc%in%input$state_city_1)}
    
    if( is.na(DT_selected_countries()) ){full_tmp %>% arrange(loc) %>% mutate(size = 0.5)}
    else{full_tmp %>%
        mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
               colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
               size = ifelse(loc != DT_selected_countries(),0.25,1.5)) %>%
        arrange(loc)}
  })
  
  #### Tab 1: Cumulative (Outputs) ####
  output$dt1 = DT::renderDataTable({
    
    dt2_global() %>% 
      datatable(escape = F,
                rownames = F,
                class = 'cell-border stripe',
                selection = "single",
                options = list(fnDrawCallback = htmlwidgets::JS('function(){
                                                  HTMLWidgets.staticRender();
                                                  }'),
                               sDom  = '<"top">rt<"bottom">lp',
                               pageLength = 25))  %>% 
      spk_add_deps()
  })
  
  output$tab1_diag = renderDataTable({
    if ((input$time_type=="onset") & (input$outcome_1 == "cases")){df_c_react()}
    else if((input$time_type=="onset") & (input$outcome_1 == "deaths")) {df_d_react()}
    else {full_react()}
  })
  
  
  output$plot_count = renderCachedPlot({
    if  ( (input$time_type=="onset") & (input$outcome_1 == "cases")& (input$rate_1 == "count"))  {
      req(nrow(df_c_react())>0)
      grid.arrange(plot_cumulative_count_onset(df_c_react() ),doubling_legend, nrow = 2, heights = c(10, 1))
    }
    else if  ( (input$time_type=="onset") & (input$outcome_1 == "cases")& (input$rate_1 == "rate"))  {
      req(nrow(df_c_react())>0)
      grid.arrange(plot_cumulative_count_rate_onset(df_c_react() ),doubling_legend, nrow = 2, heights = c(10, 1))
    }
    
    
    else if ( (input$time_type=="onset") & (input$outcome_1 == "deaths")& (input$rate_1 == "count")) {
      req(nrow(df_d_react())>0)
      grid.arrange(plot_cumulative_death_onset(df_d_react()),
                   doubling_legend, nrow = 2, heights = c(10, 1))
    }
    
    else if ( (input$time_type=="onset") & (input$outcome_1 == "deaths")& (input$rate_1 == "rate")) {
      req(nrow(df_d_react())>0)
      grid.arrange(plot_cumulative_death_rate_onset(df_d_react()),
                   doubling_legend, nrow = 2, heights = c(10, 1))
    }
    
    else if ( (input$time_type!="onset") & (input$outcome_1 == "cases")& (input$rate_1 == "count"))  {
      req(nrow(full_react())>0)
      plot_cumulative_date(full_react(),"Confirmed Cases")
    }
    else if ( (input$time_type!="onset") & (input$outcome_1 == "cases")& (input$rate_1 == "rate"))  {
      req(nrow(full_react())>0)
      plot_cumulative_date(full_react(),"Confirmed Cases per 1 M")
    }
    else if ( (input$time_type!="onset") & (input$outcome_1 != "cases")& (input$rate_1 == "count"))  {
      req(nrow(full_react())>0)
      plot_cumulative_date(full_react(),"Deaths" )
    }
    else if ( (input$time_type!="onset") & (input$outcome_1 != "cases")& (input$rate_1 == "rate"))  {
      req(nrow(full_react())>0)
      plot_cumulative_date(full_react(),"Deaths per 10 M" )
    }
  },
  cacheKeyExpr = { list(input$time_type,input$level_1,input$outcome_1,df_d_react(),df_c_react() ,full_react()) })
  
  #### Tab 2: Daily (Render UI) ####
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
    else {
      pickerInput("countries_2",label = "Select Country",
                  multiple = FALSE,
                  options = list(`live-search` = TRUE),
                  choices = choices_df %>% filter(level ==input$level_2) %>% pull(country) %>% unique(),
                  selected = "Brazil")
    }
  })
 
  output$control_state_city_2 = renderUI({
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
    else {
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
  #### Tab 2: Daily (Reactives) ####
  
  tidy.daily.subnational.react = reactive({
    w$show()
    req(!is.na(input$level_2)  )
    req(!is.na(input$countries_2))
    if (input$level_2 == "country"){
      tidy.daily.country %>% 
        filter(smooth_days == 7) %>% 
        filter(loc == input$countries_2)
    }
    else { 
      # input = list()
      # input$level_2 = "L1"
      # input$countries_2 = "Peru"
      # input$state_city_2 = "Lima"
      req(!is.na(input$state_city_2))
      df_tmp = tidy.daily.subnational %>% 
        filter(level == input$level_2) %>% 
        filter(country == input$countries_2) %>% 
        filter(loc == input$state_city_2) #%>% 
        #filter(smooth_days == 7)
      if (input$countries_2 == "Chile"){
        df_tmp%>% 
          mutate(rollsum=ifelse( (type == "deaths"&date<"2020-06-08"),NA,rollsum   ))
      }
      else {
        df_tmp
      }
    }
  })
  
  # df_tmp = tidy.daily.subnational %>%
  #   filter(level == "L1",
  #          country=="Chile",
  #          loc == "Santiago"
  #          ) %>%
  #   mutate(rollsum=ifelse( (type == "deaths"&date<"2020-06-08"),NA,rollsum   ))
  # plot_lockdown_effect_salurbal(dfa)
  # 
  
  #### Tab 2: Daily (Outputs) ####
  output$tab2_diag = renderDataTable(tidy.daily.subnational.react())
  
  output$plot_rolling  = renderCachedPlot({
    req(nrow(tidy.daily.subnational.react())>0)
    if (input$level_2 =="country"){
      plot_lockdown_effect(tidy.daily.subnational.react()) 
      }
    else {
      plot_lockdown_effect_salurbal(tidy.daily.subnational.react()) 
     #plot_lockdown_effect_salurbal(df_tmp) 
    }
  },
  cacheKeyExpr = { list(input$countries_2,tidy.daily.subnational.react()) })
  
  
  #### Tab 3: Map ####
  map_daily_data_react = reactive({
    if (input$map_level == "country"){map_daily_data %>% filter(date == input$plot_date) %>% filter(rate == input$type_map)}
    else { map_daily_data_salurbal_l1 %>% filter(date == input$plot_date) %>% filter(rate == input$type_map)}
  })
  
  current_global_cases_react = reactive({map_global_totals %>% filter(date == input$plot_date) %>% pull(confirmed)})
  current_global_deaths_react = reactive({map_global_totals %>% filter(date == input$plot_date) %>% pull(deaths)})
  
  output$map_text_date = renderText(map_daily_data_react() %>% slice(1) %>% pull(date_label))
  output$current_global_cases = renderText( paste("Total Global Cases:\n",format(current_global_cases_react(),big.mark=",") ))
  output$current_global_deaths = renderText( paste("Total Global Deaths:\n",format(current_global_deaths_react(),big.mark=",") ))
  output$mapper = renderLeaflet({
    basemap 
  })
  
  map_radius_factor_react = reactive({
    case_when(
      input$map_level=="country"&input$type_map=="confirmed" ~ 3.5,
      input$map_level=="country"&input$type_map=="confirmed_rate" ~ 2.5,
      input$map_level=="country"&input$type_map=="deaths" ~ 3,
      input$map_level=="country"&input$type_map=="deaths_rate" ~ 2.5,
      input$map_level!="country"&input$type_map=="confirmed" ~ 3.5,
      input$map_level!="country"&input$type_map=="confirmed_rate" ~ 3.5,
      input$map_level!="country"&input$type_map=="deaths" ~ 3,
      input$map_level!="country"&input$type_map=="deaths_rate" ~ 2.5
      
    )
  })
  
  output$mapper_add_marker = renderPlot({
    leafletProxy("mapper") %>%
      clearMarkers() %>%
      addCircleMarkers(data = map_daily_data_react(), 
                       lat = ~lat, lng = ~lng, 
                       radius=~radius,
                       # radius=~(n)^(1/map_radius_factor_react()),
                       #radius=~(log_n),#^(1/map_radius_factor_react())
                       label  = ~paste(loc,":",format(n,big.mark=",")),
                       fillOpacity = 0.4,
                       color = "red", weight = 0)
    
  })
  
  
  #### Tab 4: Data ####
  
  
  data_output_raw_react = reactive({
    tidy.full%>% 
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
  
  output$data_available = renderTable({
    data_output_raw_react()%>%
      filter(level!="country") %>% 
      select(country, level, outcome = type) %>%
      count(level,outcome,country ) %>%
      select(-n) %>% 
      group_by(level, outcome) %>% 
      summarise(countries = paste(country, collapse = ", ")) %>% 
      ungroup()
  })
  output$data_dt = renderDataTable(
    data_output_clean_react()
  )
  output$download_data = downloadHandler(
    filename = function() {
      paste("SALURBAL_covid19_data.csv")
    },
    content = function(file) {
      fwrite(data_output_clean_react(), file)
    }
  )
}



#### ***************************** ####

shinyApp(ui = ui(), server = server)

