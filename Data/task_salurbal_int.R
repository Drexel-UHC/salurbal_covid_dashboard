
salurbal_covid19_update = function(){
  
  ## 0. Setup -----
  {
    library(stringr)
    rm(list = ls())
    cpu_RL = (str_detect(getwd(),"ranli"))
    cpu_UHC = !cpu_RL
    if (cpu_UHC) { 
      setwd("C:/Users/rl627/Desktop/Git/SALURBAL COVID19 Dashbaord (Private)/Data") 
    } else { setwd("C:/Users/ranli/Desktop/Git local/SALURBAL COVID-19 Dashboard/Data") }
  }
  
  
  ## 1. Clean data -----
  {
    task0 = try ({ source("code_salurbal_data_downloader.R")}) %>% as.character()
    task1 = try ({ source("code_salurbal_data_updater.R")}) %>% as.character()
  }
  
  
  ## 2. Save Files -----
  {
    if (str_detect(getwd(),"Dev")){
      print("Step 2: Save to Dev")
      ### Save Static
      save(
        countries_salurbal,other_central_countries,other_south_countries,
        other_carribean_countries,other_lac_countries,lac_countries,minc,mind,
        countries_references,countries_interest,col_rolling,
        country_coords,
        sf_world,salurbal_l1_sf,sf_salurbal_0.8,  
        xwalk_data_cum_rate_cleaned,xwalk_data_titles,xwalk_data_rate_cleaned,xwalk_data_rate,xwalk_salid,
        tidy.data.all.old,
        file = "../App (Development)/covid19_processed_data_static.rdata")
      ### Save Dynamic
      save(
        df_map_data,map_global_totals,subset_dates_tmp,
        choices_df,
        tidy.data.all.new,
        file = "../App (Development)/covid19_processed_data_dynamic.rdata")
    } else {
      print("Step 2: Save to Clean")
      ### Save Static
      save(
        countries_salurbal,other_central_countries,other_south_countries,
        other_carribean_countries,other_lac_countries,lac_countries,minc,mind,
        countries_references,countries_interest,col_rolling,
        country_coords,
        sf_world,salurbal_l1_sf,sf_salurbal_0.8,  
        xwalk_data_cum_rate_cleaned,xwalk_data_titles,xwalk_data_rate_cleaned,xwalk_data_rate,xwalk_salid,
        tidy.data.all.old,
        file = "../App (Production)/covid19_processed_data_static.rdata")
      ### Save Data
      save(
        countries_salurbal,other_central_countries,other_south_countries,
        other_carribean_countries,other_lac_countries,lac_countries,minc,mind,
        countries_references,countries_interest,col_rolling,
        country_coords,
        sf_world,salurbal_l1_sf,sf_salurbal_0.8,  
        xwalk_data_cum_rate_cleaned,xwalk_data_titles,xwalk_data_rate_cleaned,xwalk_data_rate,xwalk_salid,
        tidy.data.all.old,
        file = "../Clean/covid19_processed_data_static.rdata")
      save(
        df_map_data,map_global_totals,subset_dates_tmp,
        choices_df,
        tidy.data.all.new,
        file = "../Clean/covid19_processed_data_dynamic.rdata")
      
      ### Push if no error and after 10PM
      if ( (!any(str_detect(task1, "Error")))&
           (format(Sys.time(),"%H")>=6)&
           (file.size("../Clean/covid19_processed_data_dynamic.rdata")<970000)) {
        print("Step 2: Push to GitHub")
        git2r::config(user.name = "rl627",user.email = "rl627@drexel.edu")
        git2r::config()
        gitstatus()
        gitadd()
        gitcommit()
        gitpush()
      }
    }
  }
  
  ## 3. Send Email ----
  if(cpu_RL){ 
    library(tableHTML)
    library (RDCOMClient)
    df_update_status = read.csv("../Clean/status_log.csv") %>% select(Country, Status,Date)
    error_rows = df_update_status %>%
      mutate(n = row_number()) %>%
      filter(str_detect(Status, "rror")) %>%
      pull(n)
    html_table_tmp = df_update_status  %>%
      tableHTML(rownames = F,
                widths = c(150,150,150)) %>%
      add_css_row(css = list('color', 'red'), rows =error_rows+1)
    ## Write Email
    subject_tmp = ifelse(any(str_detect(df_update_status$Status,"Error")),
                         paste0("SALURBAL COVID Data Update ",
                                Sys.Date() %>% format("%b %d, %Y"),
                                "; ERROR in pipeline"),
                         paste0("SALURBAL COVID Data Update ",
                                Sys.Date() %>% format("%b %d, %Y"))
    )
    body_tmp  = ifelse(any(str_detect(df_update_status$Status,"Error")),
                       str_c(
                         "Hi SALURBAL COVID-19 Team,<br/><br/>",
                         "This is a biweekly automated email to keep track of our data updates. There was an error in the daily update process which scheduled at ",
                         Sys.time() %>% format("%I:%M %p %b %d, %Y."),
                         " Please see the table below for details.<br/><br/>",
                         html_table_tmp,
                         "<br/><br/>Thanks,<br/>Ran"
                       ),
                       str_c(
                         "Hi SALURBAL COVID-19 Team,<br/><br/>",
                         "This is a biweekly automated email to keep track of our data updates.
                       Please see the table below for details about the data update for ",
                         Sys.time() %>% format("%I:%M %p %b %d, %Y."),
                         "<br/><br/>",
                         html_table_tmp,
                         "<br/><br/>Thanks,<br/>Ran"
                       )
    )
    ## Send email (Daily to self and Weekly to group)
    library (RDCOMClient)
    Outlook <- COMCreate("Outlook.Application")
    Email = Outlook$CreateItem(0)
    Email[["to"]] = ifelse((format(Sys.Date(),"%a")%in%c("Wed"))&
                             (format(Sys.time(),"%H")>=17)&
                             (!str_detect(getwd(),"Development") ),
                           "rl627@drexel.edu;ub45@drexel.edu;jlk465@drexel.edu;",
                           "rl627@drexel.edu") 
    Email[["cc"]] = ""
    Email[["bcc"]] = ""
    Email[["subject"]] = subject_tmp
    Email[["htmlbody"]] = body_tmp
    Email$Send()
    rm(Outlook, Email)
  }
  
}

salurbal_covid19_update()

