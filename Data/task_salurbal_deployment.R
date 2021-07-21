# Setup ----
dir_data = "C:/Users/ranli/Desktop/Git local/SALURBAL COVID-19 Dashboard/Data/"
dir_app = "C:/Users/ranli/Desktop/Git local/SALURBAL COVID-19 Dashboard/App (Production)/"
setwd(dir_data)
# source("code_salurbal_data_updater_util.R", local = T)
appName_tmp = "salurbal_covid19"
sysTime_tmp = Sys.time() %>% format("%I:%M %p %b %d, %Y.")
gitpull = function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git pull"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
gitpull()



# Deploy SALURBAL App ----
library(rsconnect)
setwd(dir_app)
deployment_status = try({
  deployApp(account = "drexel-uhc",
            appName = appName_tmp,
            forceUpdate  = T)
  # Sys.sleep(time = 60)
  # browseURL("https://drexel-uhc.shinyapps.io/salurbal_covid19/")
  print("Deployment Okay")
})

# Email notification ----
{
  library (RDCOMClient)
  library(tidyverse)
  subject_tmp = ifelse(str_detect(deployment_status,"Error"),
                       paste0("SALURBAL COVID App Deployment Update ",sysTime_tmp,"; Error in pipeline"),
                       paste0("SALURBAL COVID App Deployment Update ",sysTime_tmp))
  body_tmp  = ifelse(any(str_detect(deployment_status,"Error")),
                     str_c("Hi SALURBAL COVID-19 Data Platform Team,<br/><br/>",
                           "There was an error in the daily deployment of '",
                           appName_tmp,"' which scheduled at ",sysTime_tmp),
                     str_c("Hi SALURBAL COVID-19 Data Platform Team,<br/><br/>",
                           "Notification of successful daily deployment of '",
                           appName_tmp,"' which scheduled at ",sysTime_tmp))
  
  Outlook <- COMCreate("Outlook.Application")
  Email = Outlook$CreateItem(0)
  Email[["to"]] =  "rl627@drexel.edu"
  Email[["cc"]] = ""
  Email[["bcc"]] = ""
  Email[["subject"]] = subject_tmp
  Email[["htmlbody"]] = body_tmp
  Email$Send()
  rm(Outlook, Email)
}
