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
shinyOptions(cache = diskCache("./covid19app-cache"), max_size = 200e6, max_age =72000)
options(scipen = 99)

load(url("https://github.com/rl627/SALURBAL-Covid19-Internal-data/blob/master/covid19_processed_data.rdata?raw=true"))
source("util.R",local = TRUE)
string_packages_uhc()


#### 1. UI ####
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

           # Tab 1         
           tabPanel("Cumulative",
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
                                                                  "Latin American and the Caribbean" = other_lac_countries,
                                                                  "Top 10 Countries by Cases" = countries_references),
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
                                                           "Calendar Time"="true")),
                                   em("Tip: Click rows in the Data Table to highlight countries or cities!")
                      ),
                      
                      mainPanel(width  = 9,
                                column(width = 4, DT::dataTableOutput('dt1')),
                                column(width = 5, plotOutput("plot_count", height = "500" ))
                      )
                    )
           ),
           # Tab 2
           tabPanel("Daily",
                    sidebarLayout(
                      sidebarPanel(width  = 3,
                                   pickerInput("data_level3","Level",
                                               choices = c("Country"="country",
                                                           "State"="state")),
                                   conditionalPanel("input.data_level3 == 'country'",
                                                    pickerInput("countries_interest3",label = "Select Countries",
                                                                multiple = FALSE,
                                                                choices = list(
                                                                  SALURBAL = countries_salurbal,
                                                                  LAC = other_lac_countries),
                                                                choicesOpt = list(
                                                                  icon = helper2$icon),
                                                                selected = "Argentina")),
                                   conditionalPanel("input.data_level3 != 'country'",
                                                    pickerInput("country_interest3",label = "Select Country",
                                                                multiple = FALSE,
                                                                choices = unique(lock.dates.state$loc),
                                                                selected = "Brazil")),
                                   conditionalPanel("input.data_level3 != 'country'",
                                                    pickerInput("states_interest3",label = "Select State",
                                                                multiple = FALSE,
                                                                choices = unique(lock.dates.state$state),
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
           # Tab 3        
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
           # Tab 4
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
                             tags$a(href='https://drexel.edu/uhc/', tags$img(src='UHC_logo.png',height='70',width='160')))

               )
               
               
                    
           )  
                   
        )
)

#### 2. Server ####

server <- function(input, output) {
  
  ## Tab 2: Daily 
  output$plot_rolling  = renderCachedPlot({
    if (input$data_level3 =="country"){
      plot_lockdown_effect(tidy.full.rolling.country,
                           input$countries_interest3,
                           input$smooth3)}
    else {
      plot_lockdown_effect_state(tidy.full.rolling.state,
                                 input$country_interest3,
                                 input$states_interest3,
                                 input$smooth3)
    }},
    cacheKeyExpr = { list(input$data_level3,
                          input$countries_interest3,input$smooth3,
                          input$country_interest3,input$states_interest3) })

  ## Tab 3:  Map
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

    
  ##Tab 1: For DT
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
                 colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
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
                 colors = ifelse((loc == DT_selected_countries())&(loc%in%countries_references),"red",colors),
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
      #annotate("text", label="Doubles\nevery day", x=5, y=10*(2^(1/1))^(12), color="black")+
      geom_line(data=template_count, aes(x=days.since.100, y=y), lty=2, color="black")+
      geom_line(data=template_count, aes(x=days.since.100, y=y2), lty=2, color="blue")+
      #annotate("text", label="Doubles\nevery 2 days", x=15, y=10*(2^(1/2))^(as.numeric(max(df_c_react()$days.since.100))), color="blue")+
      geom_line(data=template_count, aes(x=days.since.100, y=y3), lty=2, color="red")+
      #annotate("text", label="Doubles\nevery 3 days", x=max(df_c_react()$days.since.100)+2.5, y=minc*(2^(1/3))^(as.numeric(max(df_c_react()$days.since.100))), color="red")+
      geom_line(data=template_count, aes(x=days.since.100, y=y4), lty=2, color="green")+
      geom_line(data=template_count, aes(x=days.since.100, y=y5), lty=2, color="grey")+
      #annotate("text", label="Doubles\nevery 4 days", x=max(df_c_react()$days.since.100)+2.5, y=minc*(2^(1/4))^(as.numeric(max(df_c_react()$days.since.100))), color="green")+
      geom_line(data =df_c_react(), aes(group=loc), size = pull(df_c_react(),size), color = pull(df_c_react(), colors))+
      geom_text_repel(data=df_c_react() %>% filter(lastday==1), 
                      aes(label=loc), color=df_c_react() %>% filter(lastday==1) %>% pull(colors),
                      segment.alpha = 0)+

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
        geom_line(data=template_death, aes(x=days.since.10, y=y2), lty=2, color="blue")+
        geom_line(data=template_death, aes(x=days.since.10, y=y3), lty=2, color="red")+
        geom_line(data=template_death, aes(x=days.since.10, y=y4), lty=2, color="green")+
        geom_line(data=template_death, aes(x=days.since.10, y=y5), lty=2, color="grey")+        
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
    
    
    #doubling_legend
    
    if      ( (input$time_type=="onset") & (input$data_type == "cases"))  {
      grid.arrange(p_count_onset,
                   doubling_legend, nrow = 2, heights = c(10, 1))}
    else if ( (input$time_type!="onset") & (input$data_type == "cases"))  {p_count_date}
    else if ( (input$time_type=="onset") & (input$data_type == "deaths")) {
      grid.arrange(p_death_onset,
                   doubling_legend, nrow = 2, heights = c(10, 1))}
    else {p_death_date}
  },
  cacheKeyExpr = { list(input$time_type,input$data_type,df_c_react(),df_d_react(),full_react() ) })
    

  
  
}



#### 3. Run App ####

shinyApp(ui = ui, server = server)

