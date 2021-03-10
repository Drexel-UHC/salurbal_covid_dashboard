library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(janitor)
library(scales)
library(gridExtra)
library(DT)
library(sparkline)
library(lubridate)
library(ggrepel)
library(readxl)
library(googlesheets4)
library(tidyverse)

#### 0. Set up ####
## Countries of Interest
countries_salurbal<-c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica",
                      "Mexico", "Peru", "Panama", "Nicaragua","Guatemala", "El Salvador")
other_lac_countries<-c("Honduras", "Uruguay", "Bolivia", "Venezuela",
                       "Ecuador", "Cuba", "Paraguay", "Dominican Republic")
countries_interest =  c(countries_salurbal,other_lac_countries)

## Get Data from Github
raw_count  = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    as_tibble();print("git1")
raw_deaths =  fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
    as_tibble();print("git2")

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

## Process data for plots of confirmed cases and deaths 
minc = 100
mind = 10 
full = full_join(counts,deaths, by = c("loc","date")) %>% 
    filter(loc%in%countries_interest) 
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
    })
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
    })

## Latest Date
date_str = paste0("Data last update: ",max(full$date)," 11:59PM" )

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
              spark_counts = spk_chr(confirm.daily, type = 'bar',width = 80,height = 30,
                                     tooltipFormatter=spk_tool(confirm.labels)),
              spark_deaths = spk_chr(deaths.daily, type = 'bar',width = 80,height = 30,
                                     tooltipFormatter=spk_tool(deaths.labels))
              ) %>% 
    rename(Country = loc)






#### 1. UI ####
ui <- fluidPage(theme = shinytheme("readable"),
    titlePanel("COVID19 in Latin America"),
    h6(date_str),
    radioGroupButtons(
        inputId = "data_type",
        label = "",
        choices = c("Cases"="cases",
                    "Deaths"="deaths")),
    fluidRow(
        column(5,
               code("Dan:Click to select rows.. the delay is here!"),
               DT::dataTableOutput('dt1')),
        column(7,
               fluidRow(   plotOutput("plot_count") ),
               fluidRow(   
                   radioGroupButtons(inputId ="time_type",label = "Time scale:", 
                                c("Since onset" = "onset",
                                  "Real Time"="true")))
        )
    )
    
)

#### 2. Server ####

server <- function(input, output) {
    
    dt2_global = reactive({
        if (input$data_type == "cases"){
            dt1_global %>% 
                select(Country,Trend = spark_counts ,Confirmed) %>% 
                arrange(desc(Confirmed)) %>% 
                mutate(Confirmed = format(Confirmed,big.mark=","))}
        else {dt1_global %>% 
                select(Country,Trend = spark_deaths,Deaths) %>% 
                arrange(desc(Deaths))%>% 
                mutate(Deaths = format(Deaths,big.mark=","))}
    })
    
    
    output$dt1 = DT::renderDataTable({
        dt2_global() %>% 
            datatable(escape = F,
                      rownames = F,
                      class = 'cell-border stripe',
                      options = list(fnDrawCallback = htmlwidgets::JS('function(){
                                                  HTMLWidgets.staticRender();
                                                  }'),
                                     sDom  = '<"top">rt<"bottom">lp',
                                     pageLength = 10))  %>% 
            spk_add_deps()
    })
    
    selected_countries = reactive({
        if(length(input$dt1_rows_selected)==0)
        {dt1_global %>% pull(Country)}
        else {dt1_global %>% slice(input$dt1_rows_selected) %>% pull(Country)}
        
    })
    
    df_c_sel = reactive(df.c %>% filter(loc%in%selected_countries() ))
    df_c_null = reactive(df.c %>% filter(!loc%in%selected_countries() ))
    df_d_sel = reactive(df.d %>% filter(loc%in%selected_countries() ))
    df_d_null = reactive(df.d %>% filter(!loc%in%selected_countries() ))
    full_sel =  reactive(full %>% filter(loc%in%selected_countries() ))
    full_null =  reactive(full %>% filter(!loc%in%selected_countries() ))
    
    output$plot_count = renderPlot({
        p_count_onset = df.c %>% 
            ggplot(aes(x=days.since.100, y=confirmed)) +
            geom_text_repel(data=df_c_sel() %>% filter(lastday==1), 
                            aes(label=loc, color=loc))+
            geom_text_repel(data=df_c_null() %>% filter(lastday==1), 
                            aes(label=loc), col= 'grey')+
            geom_line(data =df_c_sel(), aes(group=loc, color=loc), size = 1)+
            geom_line(data =df_c_null(), aes(group=loc), col= 'grey')+
            geom_line(data=template_count, aes(x=days.since.100, y=y), lty=2, color="black")+
            annotate("text", label="Doubles\nevery day", x=5, y=10*(2^(1/1))^(12), color="black")+
            geom_line(data=template_count, aes(x=days.since.100, y=y2), lty=2, color="blue")+
            annotate("text", label="Doubles\nevery 2 days", x=15, y=10*(2^(1/2))^(as.numeric(max(df.c$days.since.100))), color="blue")+
            geom_line(data=template_count, aes(x=days.since.100, y=y3), lty=2, color="red")+
            annotate("text", label="Doubles\nevery 3 days", x=max(df.c$days.since.100)+2.5, y=minc*(2^(1/3))^(as.numeric(max(df.c$days.since.100))), color="red")+
            geom_line(data=template_count, aes(x=days.since.100, y=y4), lty=2, color="green")+
            annotate("text", label="Doubles\nevery 4 days", x=max(df.c$days.since.100)+2.5, y=minc*(2^(1/4))^(as.numeric(max(df.c$days.since.100))), color="green")+
            scale_linetype_manual(values=c(2, 1))+
            scale_x_continuous(breaks=seq(0, 50, by=5), limits=c(0, max(df.c$days.since.100)+5))+
            scale_y_log10(limits=c(100, 10^5))+
            annotation_logticks(sides="l") + 
            labs(title="Confirmed Cases in the LAC Region (countries with >=100 cases)",
                 x="Days from epidemic onset (>=100 cases)", y="Confirmed Cases")+
            theme_bw() +
            theme(panel.grid.minor.x = element_line(),
                  legend.position = 'none')
        
        p_count_date =  full %>% 
            ggplot(aes(x=date, y=confirmed)) +
            geom_line(data = full_null(),aes(group=loc), col = 'grey')+
            geom_line(data = full_sel(), aes(group=loc, color=loc))+
            geom_text_repel(data=full_null() %>% arrange(loc, desc(date)) %>% 
                                filter(!duplicated(loc), confirmed>1),
                            aes(label=loc, x=date), color="grey", hjust=0,
                            segment.color = "gray",
                            nudge_x = max(full$date)+1,
                            direction     = "y",
                            hjust = 0.5)+
            geom_text_repel(data=full_sel() %>% arrange(loc, desc(date)) %>% 
                                filter(!duplicated(loc), confirmed>1),
                            aes(label=loc, x=date, color=loc), hjust=0,
                            segment.color = "gray",
                            nudge_x = max(full$date)+1,
                            direction     = "y",
                            hjust = 0.5)+
            #geom_point(pch=21, fill="gray")+
            scale_linetype_manual(values=c(2, 1))+
            scale_x_date(limits=c(as_date("2020-03-01"), max(full$date)+5))+
            scale_y_log10(limits=c(1, 10^5))+
            annotation_logticks(sides="l") + 
            labs(title="Confirmed Cases in the LAC Region",
                 x="Date")+
            guides(color=F)+
            theme_bw() +
            theme(panel.grid.minor.x = element_line())
        
        p_death_onset = df.d %>% 
            ggplot(aes(x=days.since.10, y=deaths)) +
            geom_text_repel(data=df_d_sel() %>% filter(lastday==1), 
                            aes(label=loc, color=loc))+
            geom_text_repel(data=df_d_null() %>% filter(lastday==1), 
                            aes(label=loc), col= 'grey')+
            geom_line(data =df_d_sel(), aes(group=loc, color=loc), size = 1)+
            geom_line(data =df_d_null(), aes(group=loc), col= 'grey')+
            geom_line(data=template_death, aes(x=days.since.10, y=y), lty=2, color="black")+
            annotate("text", label="Doubles\nevery day", x=5, y=10*(2^(1/1))^(5), color="black")+
            geom_line(data=template_death, aes(x=days.since.10, y=y2), lty=2, color="blue")+
            annotate("text", label="Doubles\nevery 2 days", x=12, y=10^3, color="blue")+
            geom_line(data=template_death, aes(x=days.since.10, y=y3), lty=2, color="red")+
            annotate("text", label="Doubles\nevery 3 days", x=max(df.d$days.since.10)+2.5, y=mind*(2^(1/3))^(as.numeric(max(df.d$days.since.10))), color="red")+
            geom_line(data=template_death, aes(x=days.since.10, y=y4), lty=2, color="green")+
            annotate("text", label="Doubles\nevery 4 days", x=max(df.d$days.since.10)+2.5, y=mind*(2^(1/4))^(as.numeric(max(df.d$days.since.10))), color="green")+
            scale_linetype_manual(values=c(2, 1))+
            scale_x_continuous(breaks=seq(0, 50, by=5), limits=c(0, max(df.d$days.since.10)+5))+
            scale_y_log10(limits=c(10, 10^3))+
            annotation_logticks(sides="l") + 
            labs(title="Confirmed Deaths in the LAC Region (countries with >=10 deaths)",
                 x="Days from epidemic onset (>=10 deaths)", y="Deaths")+
            theme_bw() +
            theme(panel.grid.minor.x = element_line(),
                  legend.position = 'none')
        
        p_death_date =  full %>% 
            ggplot(aes(x=date, y=deaths)) +
            geom_line(data = full_null(),aes(group=loc), col = 'grey')+
            geom_line(data = full_sel(), aes(group=loc, color=loc))+
            geom_text_repel(data=full_null() %>% arrange(loc, desc(date)) %>% 
                                filter(!duplicated(loc)),
                            aes(label=loc, x=date), color="grey", hjust=0,
                            segment.color = "gray",
                            nudge_x = max(full$date)+1,
                            direction     = "y",
                            hjust = 0.5)+
            geom_text_repel(data=full_sel() %>% arrange(loc, desc(date)) %>% 
                                filter(!duplicated(loc), deaths>3),
                            aes(label=loc, x=date, color=loc), hjust=0,
                            segment.color = "gray",
                            nudge_x = max(full$date)+1,
                            direction     = "y",
                            hjust = 0.5)+
            scale_linetype_manual(values=c(2, 1))+
            scale_x_date(limits=c(as_date("2020-03-08"), max(full$date)+5))+
            scale_y_log10(limits=c(1, 10^3))+
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
    })
    

    
}



#### 3. Run App ####

shinyApp(ui = ui, server = server)

