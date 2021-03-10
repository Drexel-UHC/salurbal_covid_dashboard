setwd("C:/Users/ranli/Desktop/Git local/SALURBAL Covid19 Git Internal/Data")
source("code_salurbal_data_updater_util.R")

salurbal_covid19_update = function(){
  ## Clean Data
  task1 = try (source("code_salurbal_data_updater.R")) %>% as.character()
  
  # Push if no Error then Push
  load("../Clean/status_log.rdata")
  if ( (!any(str_detect(c(task1,df_update_status$Status), "Error")))&
       (format(Sys.time(),"%H")>=22)
       
       ) {
    git2r::config(user.name = "rl627",user.email = "rl627@drexel.edu")
    git2r::config()
    gitstatus()
    gitadd()
    gitcommit()
    gitpush()
    
  }
  
  
  ## Get Status Log 
  library(tableHTML)
  library (RDCOMClient)
  error_rows = df_update_status %>%
    mutate(n = row_number()) %>%
    filter(str_detect(Status, "Error")) %>%
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
                           (format(Sys.time(),"%H")>=22)&
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

salurbal_covid19_update()

