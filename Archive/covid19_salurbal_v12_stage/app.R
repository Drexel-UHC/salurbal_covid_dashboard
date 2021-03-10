rm(list=ls())
library(cowplot)
library(shiny)
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
library(RCurl)
library(tidyverse)
#shinyOptions(cache = diskCache("./covid19app-cache"), max_size = 200e6, max_age =72000)
options(scipen = 99)

#load(url("https://github.com/rl627/SALURBAL-Covid19-Internal-data/blob/master/covid19_processed_data.rdata?raw=true"))
load("covid19_processed_data_dev.rdata")
source("util.R",local = TRUE)
string_packages_uhc()



####  *************************** ####
ui = function(){
  ui <- bootstrapPage(theme = shinytheme("flatly"),
                      # tags$style(".glyphicon-ok-sign {color:#2b8ee5}
                      #        .glyphicon-question-sign {color:#f4e107}
                      #        .glyphicon-exclamation-sign {color:#e5413b}
                      #        .glyphicon-flag, .glyphicon-trash {color:#28b728}"),
                      tags$style(".glyphicon-alert {color:#f2b25e}
                  .glyphicon-cog {color:#00f4e107}"),       
                      
                      # glyphicon-alert
                      
                      #shinythemes::themeSelector(),
                      navbarPage(title = "COVID19 in SALURBAL Countries",
                                 
                                 ####  Tab 1:Cumulative ####       
                                 tabPanel("Cumulative",
                                          
                                          sidebarLayout(
                                            sidebarPanel(width  = 3,
                                                         # textOutput("text_scr"),
                                                         pickerInput("data_type","Outcome",
                                                                     choices = c("Cases"="cases",
                                                                                 "Deaths"="deaths")),
                                                       
                                                         pickerInput("data_level","Level",
                                                                     choices = c("Country"="country",
                                                                                 "State"="state",
                                                                                 "L1",
                                                                                 "L2")),
                                                         conditionalPanel("input.data_level == 'country'",
                                                                          pickerInput("countries_interest",label = "Select Countries",
                                                                                      multiple = TRUE,
                                                                                      choices = list(
                                                                                        SALURBAL = countries_salurbal,
                                                                                        "Other Central American countries" = other_central_countries,
                                                                                        "Other South American countries" = other_south_countries,
                                                                                        "Other Caribbean countries" = other_carribean_countries,
                                                                                        "Top 10 Countries by Cases" = countries_references),
                                                                                      selected = c(countries_salurbal))),
                                                         conditionalPanel("input.data_level == 'state'",
                                                                          pickerInput("country_interest_state",label = "Select Country", 
                                                                                      choices = state_level_countries,
                                                                                      selected = "Brazil"),
                                                                          
                                                                          uiOutput('select_state_1')
                                                         ),
                                                         conditionalPanel("input.data_level == 'L1'",
                                                                          uiOutput('select_country_l1_1'),
                                                                          uiOutput('select_l1_1')
                                                                          
                                                         ),
                                                         conditionalPanel("input.data_level == 'L2'",
                                                                          uiOutput('select_country_l2_1'),
                                                                          uiOutput('select_l2_1')
                                                        ),
                                                         pickerInput("time_type","Plotting start date:",
                                                                     choices = c("Since onset" = "onset",
                                                                                 "Calendar Time"="true")),
                                                         
                                                         em("Tip: Click rows in the Data Table to highlight countries or cities!")
                                            ),
                                            
                                            mainPanel(width  = 9,
                                                      column(width = 4, DT::dataTableOutput('dt1')),
                                                      column(width = 5, plotOutput("plot_count", height = "500" )
                                                      )
                                                      
                                            )
                                          )
                                 ),
                                 ####  Tab 2: Daily ####    
                                 tabPanel("Daily",
                                          sidebarLayout(
                                            sidebarPanel(width  = 3,
                                                         pickerInput("data_level3","Level",
                                                                     choices = c("Country"="country",
                                                                                 "State"="state",
                                                                                 "L1",
                                                                                 "L2")),
                                                         conditionalPanel("input.data_level3 == 'country'",
                                                                          pickerInput("countries_interest3",label = "Select Countries",
                                                                                      multiple = FALSE,
                                                                                      choices = list(
                                                                                        SALURBAL = countries_salurbal,
                                                                                        LAC = other_lac_countries),
                                                                                      choicesOpt = list(
                                                                                        icon = helper2$icon),
                                                                                      selected = "Argentina")),
                                                         conditionalPanel("input.data_level3 == 'state'",
                                                                          pickerInput("country_interest_state_3",label = "Select Country",
                                                                                      multiple = FALSE,
                                                                                      choices = state_level_countries,
                                                                                      selected = "Brazil"),
                                                                          uiOutput('select_state_3')
                                                                          ),
                                                         conditionalPanel("input.data_level3 == 'L1'",
                                                                          pickerInput("country_interest_l1_3",label = "Select Country",
                                                                                      multiple = FALSE,
                                                                                      choices = unique(lock.dates.state$loc),
                                                                                      selected = "Brazil"),
                                                                          pickerInput("l1_interest_3",label = "Select L1",
                                                                                      multiple = FALSE,
                                                                                      choices = list(
                                                                                        "Top 25 Largest" = l1_br_top,
                                                                                        "Others" =  l1_br_rest),
                                                                                      selected = "Sao Paulo")),
                                                         conditionalPanel("input.data_level3 == 'L2'",
                                                                          pickerInput("country_interest_l2_3",label = "Select Country",
                                                                                      multiple = FALSE,
                                                                                      choices = unique(lock.dates.state$loc),
                                                                                      selected = "Brazil"),
                                                                          pickerInput("l2_interest_3",label = "Select L2",
                                                                                      multiple = FALSE,
                                                                                      choices = list(
                                                                                        "Top 25 Largest" = l2_br_top,
                                                                                        "Others" =  l2_br_rest),
                                                                                      selected = "Sao Paulo")),
                                                         pickerInput("smooth3","Smooth counts:",
                                                                     choices = c("Yes"="yes",
                                                                                 "No"="no"),
                                                                     selected = "yes"),
                                                         em("Note: We recognize there is heterogeneity in various national responses to Covid19 and not all 'National lockdowns' are identical. Currently we using a very hueristic definition of national lock down and are working to adopt a more globally consistent definition.")
                                                         
                                            ),
                                            
                                            mainPanel(width  = 9,
                                                      column(width = 8,
                                                             plotOutput("plot_rolling", height = "450")
                                                      )
                                            )
                                          )
                                 ),
                                 ####  Tab 3: Map ####            
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
                                 ####  Tab 4: Data  ####
                                 tabPanel("Data",
                                          sidebarLayout(
                                            sidebarPanel(width  = 3,
                                                         # textOutput("text_scr"),
                                                         pickerInput("data_type4","Outcome",
                                                                     choices = c("Cases"="confirmed",
                                                                                 "Deaths"="deaths",
                                                                                 "Deaths and Cases" = "both"),
                                                                     selected = "confirmed"),
                                                         
                                                         pickerInput("data_level4","Level",
                                                                     choices = c("Country"="country",
                                                                                 "State"="state",
                                                                                 "L1",
                                                                                 "L2")),
                                                         downloadButton("download_data", "Download CSV")
                                            ),
                                            
                                            mainPanel(width  = 9,
                                                      tabsetPanel(type = "tabs",
                                                                  tabPanel("Data", DT::dataTableOutput('data_dt')),
                                                                  
                                                                  tabPanel("Availability", tableOutput('data_available')))
                                                                 

                                            ) #
                                          )
                                 ),
                                 
                                 
                                 ####  Tab 5: About ####    
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
                                          "SALURBAL Level Data:",tags$a(href="https://github.com/rl627/SALURBAL-Covid19-Internal-data/blob/master/salurbal_covid19_cumulative_data.csv",
                                                                                 "https://github.com/rl627/SALURBAL-Covid19-Internal-data/blob/master/salurbal_covid19_cumulative_data.csv"),br(),
                                
                                          h4("Authors"),
                                          "Ran Li, Data Analyst at SALURBAL",br(),
                                          "Usama Bilal, Co-Investigator at SALURBAL",br() ,br() ,br() ,
                                          fluidRow(
                                            
                                            absolutePanel(left = 20, width = 80, fixed=F, draggable = FALSE, height = "auto",
                                                          tags$a(href='https://drexel.edu/lac/salurbal/overview/', tags$img(src='SALURBAL_logo.png',height='70',width='160'))),
                                            absolutePanel(left = 220, width = 80, fixed=F, draggable = FALSE, height = "auto",
                                                          tags$a(href='https://drexel.edu/uhc/', tags$img(src='UHC_logo.png',height='70',width='160')))
                                            
                                          )
                                          
                                          
                                          
                                 )  
                                 
                      )
  )
}
####  *************************** ####
server <- function(input, output) {
  
  ####  Reactives (UI) ####
  output$select_state_1 = renderUI({
    states_top_tmp  = choices_df %>% filter(level == "state",country == input$country_interest_state,top == "top") %>% pull(state)
    states_rest_tmp  = choices_df %>% filter(level == "state",country == input$country_interest_state) %>% filter(top == "rest") %>% pull(state)
    pickerInput("state_interest_1",label = "Select state",
                multiple = TRUE,
                choices = list(
                  "Top 10 Largest" = states_top_tmp,
                  "Others" =  states_rest_tmp),
                selected = states_top_tmp)
  })
  
  output$select_country_l1_1 = renderUI({
    l1_countries_tmp  = c()
    if (input$data_type=="cases"){l1_countries_tmp = c("Brazil","Mexico") }
    else {l1_countries_tmp = c("Brazil") }
    pickerInput("country_interest_l1",label = "Select Country", 
                choices = l1_countries_tmp,
                selected = "Brazil")
  })
  
  output$select_l1_1 = renderUI({
    states_top_tmp  = choices_df %>% filter(level == "L1",country == input$country_interest_l1,top == "top") %>% pull(state)
    states_rest_tmp  = choices_df %>% filter(level == "L1",country == input$country_interest_l1) %>% filter(top == "rest") %>% pull(state)
    states_non_tmp = choices_df %>% filter(level == "L1",country == input$country_interest_l1) %>% filter(top == "non") %>% pull(state)
    pickerInput("l1_interest",label = "Select L1",
                multiple = TRUE,
                choices = list(
                  "Top 10 Largest" = states_top_tmp,
                  "Others" =  states_rest_tmp,
                  "Non Salurbal" = states_non_tmp),
                selected = states_top_tmp)
  })
  
  output$select_country_l2_1 = renderUI({
    l2_countries_tmp  = c()
    if (input$data_type=="cases"){l2_countries_tmp = c("Brazil","Mexico") }
    else {l2_countries_tmp = c("Brazil") }
    pickerInput("country_interest_l2",label = "Select Country", 
                choices = l2_countries_tmp,
                selected = "Brazil")
  })
  
  output$select_l2_1 = renderUI({
    states_top_tmp  = choices_df %>% filter(level == "L2",country == input$country_interest_l2,top == "top") %>% pull(state)
    states_rest_tmp  = choices_df %>% filter(level == "L2",country == input$country_interest_l2) %>% filter(top == "rest") %>% pull(state)
    states_non_tmp = choices_df %>% filter(level == "L2",country == input$country_interest_l2) %>% filter(top == "non") %>% pull(state)
    pickerInput("l2_interest",label = "Select L2",
                multiple = TRUE,
                choices = list(
                  "Top 10 Largest" = states_top_tmp,
                  "Others" =  states_rest_tmp,
                  "Non Salurbal" = states_non_tmp),
                selected = states_top_tmp)
  })
  
  output$select_state_3 = renderUI({
    states_top_tmp  = choices_df %>% filter(level == "state",country == input$country_interest_state_3) %>% filter(top == "top") %>% pull(state)
    states_rest_tmp  = choices_df %>% filter(level == "state",country == input$country_interest_state_3) %>% filter(top == "rest") %>% pull(state)
    pickerInput("state_interest_3",label = "Select state",
                multiple = FALSE,
                choices = list(
                  "Top 10 Largest" = states_top_tmp,
                  "Others" =  states_rest_tmp),
                selected = states_top_tmp[1])
  })
  ####  Reactives (Server) ####
  ## Data Table
  # output$text_scr = renderText(paste("country_interest_state",input$country_interest_state,
  #                                    "state_interest_1",input$state_interest_1
  #                                    ))
  
  dt2_global = reactive({
    loc_name_tmp = case_when(
      input$data_level == "country"~"Country",
      input$data_level == "state"~"State",
      input$data_level == "L1"~"L1",
      input$data_level == "L2"~"L2")
    type_name_tmp = ifelse(input$data_type == "cases","Confirmed", "Deaths" )
    df_tmp = dt1_global %>% 
      filter(level == input$data_level) %>% 
      filter(type == input$data_type) %>% 
      mutate(n = format(n,big.mark=",")) 
    if(input$data_level == "country"){df_tmp = df_tmp%>% filter(loc%in%input$countries_interest)}
    else if(input$data_level == "state"){df_tmp = df_tmp %>% filter(country == input$country_interest_state) %>% filter(loc%in%input$state_interest_1)}
    else if(input$data_level == "L1"){df_tmp = df_tmp%>% filter(loc%in%input$l1_interest)}
    else {df_tmp = df_tmp%>% filter(loc%in%input$l2_interest)}
    df_tmp %>% 
      select(loc,Trend = spark, n) %>% 
      arrange(desc(n)) %>% 
      rename(!!loc_name_tmp := loc) %>% 
      rename(!!type_name_tmp := n)
    
  })
  
  DT_selected_countries = reactive({
    if(length(input$dt1_rows_selected)==0){NA_character_}
    else {
      if(input$data_level =="country"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(Country)}
      else if (input$data_level =="state"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(State)}
      else if (input$data_level =="L1"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(L1)}
      else if (input$data_level =="L2"){dt2_global() %>% slice(input$dt1_rows_selected) %>% pull(L2)}
    }
  })
  
  df_c_react  = reactive({ 
    df_level_tmp = df.c %>% filter(level == input$data_level) 
  if(input$data_level == "country"){df_level_tmp = df_level_tmp %>% filter(loc%in%input$countries_interest)}
  else if(input$data_level == "state"){df_level_tmp = df_level_tmp %>% filter(country == input$country_interest_state) %>% filter(loc%in%input$state_interest_1)}
  else if(input$data_level == "L1"){df_level_tmp = df_level_tmp   %>% filter(country==input$country_interest_l1) %>% filter(loc%in%input$l1_interest)}
  else {df_level_tmp = df_level_tmp %>% filter(country == input$country_interest_l2) %>% filter(loc%in%input$l2_interest)}
  
  if( is.na(DT_selected_countries()) ){df_level_tmp %>% arrange(loc)}
  else{df_level_tmp %>% 
      mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
             colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
             size = ifelse(loc != DT_selected_countries(),0.75,size)) %>% 
      arrange(loc)}
  })

  df_d_react  = reactive({ 
    df_level_tmp = df.d %>% filter(level == input$data_level) 
    if(input$data_level == "country"){df_level_tmp = df_level_tmp %>% filter(loc%in%input$countries_interest)}
    else if(input$data_level == "state"){df_level_tmp = df_level_tmp %>% filter(country == input$country_interest_state) %>% filter(loc%in%input$state_interest_1)}
    else if(input$data_level == "L1"){df_level_tmp = df_level_tmp %>% filter(country == input$country_interest_l1) %>% filter(loc%in%input$l1_interest)}
    else {df_level_tmp = df_level_tmp %>% filter(country == input$country_interest_l2) %>% filter(loc%in%input$l2_interest)}
    
    if( is.na(DT_selected_countries()) ){df_level_tmp %>% arrange(loc)}
    else{df_level_tmp %>% 
        mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
               colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
               size = ifelse(loc != DT_selected_countries(),0.75,size)) %>% 
        arrange(loc)}
  })
  
  
  full_react = reactive({ 
    full_tmp = full_global
    if(input$data_level == "country"){full_tmp = full_tmp %>% filter(level == "country")%>% filter(loc%in%input$countries_interest)}
    else if(input$data_level == "state"){full_tmp = full_tmp %>% filter(level == "state") %>% filter(country == input$country_interest_state)  %>% filter(loc%in%input$state_interest_1)}
    else if(input$data_level == "L1"){full_tmp = full_tmp %>% filter(level == "L1") %>% filter(country == input$country_interest_l1) %>% filter(loc%in%input$l1_interest)
    }
    else {full_tmp = full_tmp %>% filter(level == "L2") %>% filter(country == input$country_interest_l2) %>% filter(loc%in%input$l2_interest)
    }
    if( is.na(DT_selected_countries()) ){full_tmp %>% arrange(loc)}
    
    else{full_tmp %>% 
        mutate(colors = ifelse(loc != DT_selected_countries(),"grey",colors),
               colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
               size = ifelse(loc != DT_selected_countries(),0.75,size)) %>% 
        arrange(loc)}
  })
  tidy.daily.subnational.react = reactive({
    tidy.daily.subnational
    
    if(input$data_level3 == "state"){
      tidy.daily.subnational %>% 
        filter(level == "state") %>% 
        filter(country == input$country_interest_state_3) %>% filter(loc == input$state_interest_3)
      }
    else if(input$data_level3 == "L1"){
      tidy.daily.subnational %>% 
        filter(level == "L1") %>% 
        filter(country == input$country_interest_l1_3)  %>% 
        filter(loc == input$l1_interest_3)
    }
    else if (input$data_level3 == "L2"){
      tidy.daily.subnational %>% 
        filter(level == "L2") %>% 
        filter(country == input$country_interest_l2_3)  %>% 
        filter(loc == input$l2_interest_3)
    }
    
  })
  #### Tab 1: Cumulative ####
  output$dt_test = renderDataTable(if (input$time_type=="onset"){df_c_react()} else {full_react() })
 
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
  

  output$plot_count = renderCachedPlot({
    if  ( (input$time_type=="onset") & (input$data_type == "cases"))  {
      grid.arrange(plot_cumulative_count_onset(df_c_react() ),doubling_legend, nrow = 2, heights = c(10, 1))
      }
    else if ( (input$time_type!="onset") & (input$data_type == "cases"))  {
      plot_cumulative_counts_date(full_react())
      }
    else if ( (input$time_type=="onset") & (input$data_type == "deaths")) {
      grid.arrange(plot_cumulative_death_onset(df_d_react()),
                   doubling_legend, nrow = 2, heights = c(10, 1))
      }
    else { plot_cumulative_death_date(full_react())  }
  },
  cacheKeyExpr = { list(input$time_type,input$data_level,input$data_type,df_d_react(),df_c_react() ,full_react()) })
  
  
  
  #### Tab 2: Daily ####
  output$plot_rolling  = renderCachedPlot({
    if (input$data_level3 =="country"){
      plot_lockdown_effect(tidy.daily.country,
                           input$countries_interest3,
                           input$smooth3)}
    else if (input$data_level3 =="state"){
      plot_lockdown_effect_salurbal(tidy.daily.subnational.react(),
                                 input$smooth3)}
    else if (input$data_level3 =="L1"){
      plot_lockdown_effect_salurbal(tidy.daily.subnational.react(),
                                 input$smooth3)}
    else if (input$data_level3 =="L2"){
      plot_lockdown_effect_salurbal(tidy.daily.subnational.react(),
                                    input$smooth3)}
    },
    cacheKeyExpr = { list(input$countries_interest3, tidy.daily.subnational.react(),input$smooth3) })


  #### Tab 3: Map ####
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
  addCircleMarkers(data = full_date_react(), lat = ~lat, lng = ~lng, radius=~(confirmed)^(1/3.5),
                   label  = ~paste(loc,":",format(confirmed,big.mark=",")),
                   fillOpacity = 0.2,
                   color = "red", weight = 1)
  })

    
  #### Tab 4: Data ####
 
  
  data_output_raw_react = reactive({
    if (input$data_type4 == "both") {tidy.full  %>% filter(level == input$data_level4)  }
    else { tidy.full  %>% filter(level == input$data_level4) %>% filter(count_type == input$data_type4)}
    
  })
  data_output_clean_react = reactive({
    data_tmp = data_output_raw_react()
    if (input$data_level4=="country") {data_tmp %>% select(-salid1, -salid2, -country) %>% 
        select(Outcome = count_type, Level = level,Country = loc, date, count)}
    else if (input$data_level4=="state") {data_tmp %>% 
        select(-salid1, -salid2, ) %>% 
        select(Outcome = count_type, Level = level,Country = country, State = loc, date, count)}
    else if (input$data_level4=="L1") {data_tmp %>% select(-salid2) %>% 
        select(Outcome = count_type,Level = level, Country = country,"L1"= loc,date,count )}
    else if (input$data_level4=="L2") {data_tmp %>% select(-salid1)%>% 
        select(Outcome = count_type,Level = level, Country = country, "L2"= loc,date,count )}
    })
  
  output$data_available = renderTable({
    data_output_raw_react()%>%
      select(country, level, outcome = count_type) %>%
      count(level,outcome,country ) %>%
      select(-n)
  })
  output$data_dt = renderDataTable(data_output_clean_react())
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

shinyApp(ui = ui(), server = server)

