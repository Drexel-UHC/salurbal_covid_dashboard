
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

#### UI ####

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
             
             ####  Tab 1: Compare across Countries/Cities ####       
             tabPanel("Compare across Countries/Cities",
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
                                     radioGroupButtons("outcome_1",
                                                       "Outcome",
                                                       choices = c("Cases"="cases",
                                                                   "Tests" = "tests",
                                                                   "Positivity"="positivity",
                                                                   "Deaths"="deaths"),
                                                       justified = T,
                                                       size = "sm"#,
                                                       # checkIcon = list(
                                                       #   yes = icon("ok",
                                                       #              lib = "glyphicon"))
                                                       ),
                                     # uiOutput("control_outcome_1"),
                                     uiOutput("control_rate_1")
                                     
                                    
                        ),
                        mainPanel(width  = 9,
                                  highchartOutput("tab1_plot", height = "400" )#,DTOutput("tab1_diag")
                        )
                      ),
                      fluidRow(logo_footer)
                      
             ),
             ####  Tab 2: Location Specific Report ####    
             tabPanel("Location Specific Report",
                      sidebarLayout(
                        sidebarPanel(width  = 3,
                                     pickerInput("level_2","Level",
                                                 choices = c("Country"="country",
                                                             "State"="state",
                                                             "City"="L1",
                                                             "Sub-city"="L2"),
                                                 selected = "L1"),
                                     uiOutput("control_countries_2"),
                                     uiOutput("control_state_city_2")#,
                                     #em("Note: We recognize there is heterogeneity in various national responses to COVID-19 and not all 'National lockdowns' are identical. Currently we using a very hueristic definition of national lock down and are working to adopt a more globally consistent definition.")
                                     
                        ),
                        
                        mainPanel(width  = 9,
                                  uiOutput("report_outputs_ui")
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
                                                        "Deaths per 10M"="deaths_rate"),
                                            selected = "confirmed_rate"
                                          ),
                                          pickerInput(
                                            "map_level",
                                            label = "Map Level",
                                            choices = c("Country Level"="country",
                                                        "City" = "L1")
                                            
                                          ),
                                          uiOutput("map_country_filter"),
                                          sliderInput("plot_date",
                                                      label = h5("Select mapping date"),
                                                      min = min(subset_dates_tmp),
                                                      max = max(subset_dates_tmp),
                                                      step  = 30,
                                                      value = max(subset_dates_tmp),
                                                      timeFormat = "%d %b", 
                                                      animate=animationOptions(interval =500, loop = FALSE) ) #
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
                                                   "City"="L1",
                                                   "Sub-city"="L2"),
                                       selected = c("country","state","L1","L2")
                                     ),
                              
                                     downloadButton("download_data", "Download CSV")
                        ),
                        
                        mainPanel(width  = 9,
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Daily Data", DT::dataTableOutput('data_daily_dt')),
                                              tabPanel("Cumulative Data", DT::dataTableOutput('data_dt')),
                                              tabPanel("Subnational Availability", dataTableOutput('data_available')))
                                  
                                  
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
                     hr(),
                      h4("Data Access"),
                     p("This data is updated daily at 6:00AM EST. A public facing repository containing daily updated data (without SALURBAL ID) can be found ",
                       tags$a(href="https://github.com/rl627/salurbal_covid19_data","here", target="_blank"),". If you are a SALURBAL researcher and would like this data linked by SALURBAL ID, then please submit a manuscript proposal and a data request application."),
                     hr(),
                      h4("Data Sources"),
                      "Country Level Cases:",tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                                                    "Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)"),br(),
                      "Country Level Deaths:",tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                                                     "Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)"),br(),
                     "Argentina Municipal Level Data:",tags$a(href="http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina",
                                                              "http://datos.salud.gob.ar/dataset/covid-19-casos-registrados-en-la-republica-argentina"),br(),
                     "Argentina (City of Buenos Aires) L2 Level Data:",tags$a(href="https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv",
                                                              "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv"),br(),
                     "Brazil State Level Data:",tags$a(href="https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv",
                                                        "https://github.com/wcota/covid19br/blob/master/cases-brazil-cities.csv"),br(),
                      "Brazil Municipal Level Data:",tags$a(href="https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
                                                            "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"),br(),
                      "Chile Municipal Level Cases:",tags$a(href="https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto1/Covid-19.csv",
                                                            "https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto1/Covid-19.csv"),br(),
                      "Colombia Municipal Level Data:",tags$a(href="https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data",
                                                              "https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data"),br(),
                      "Guatemala Municipal Level Data:",tags$a(href="https://tablerocovid.mspas.gob.gt/",
                                                               "https://tablerocovid.mspas.gob.gt/"),br(),
                      "Mexico State Level Cases:",tags$a(href="https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_casos_totales.csv",
                                                         "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_casos_totales.csv"),br(),
                      "Mexico State Level Deaths:",tags$a(href="https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_muertes.csv",
                                                          "https://raw.githubusercontent.com/mexicovid19/Mexico-datos/master/datos/series_de_tiempo/covid19_mex_muertes.csv"),br(),
                      "Mexico Municipal Level Cases:",tags$a(href="https://www.gob.mx/salud/documentos/datos-abiertos-152127",
                                                             "https://www.gob.mx/salud/documentos/datos-abiertos-152127"),br(),
                      "Peru Municipal Level Data:",tags$a(href="https://www.datosabiertos.gob.pe/sites/default/files/DATOSABIERTOS_SISCOVID.zip",
                                                          "https://www.datosabiertos.gob.pe/sites/default/files/DATOSABIERTOS_SISCOVID.zip"),br(),
                     hr(),
                     h4("Version updates:"),
                      tags$ul(
                        tags$li("5/6/2020: Subnational data for Chile and Colombia. Added City level visualizations to map."),
                        tags$li("5/20/2020: User Interface update."),
                        tags$li("5/29/2020: Added Peru Subnational Data. Fixed label and axis issues for plot in 'Total cases and deaths' tab."),
                        tags$li("9/17/2020: New data: Argentina, Guatemala subnational data. New features: maps at L2/Sub-city level maps and interactive visualizations focusing on trends over time. "),
                        tags$li("9/28/2020: Data Update: Address undercounting of COVID-19 cases and deaths for the City of Buenos Aires, Argentina by using using data provided by city itself.")
                      ),
                     hr(),
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

