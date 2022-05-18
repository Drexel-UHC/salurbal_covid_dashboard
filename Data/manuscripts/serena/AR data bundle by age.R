#' Argentina data for Servena Perner, Andres and Marco
#' First run code_salurbal_data_updater.R; then subset data for Argentina
#' 
{ # Subset for Argentina----
  tidy.daily.data.output_AR = tidy.daily.data.output %>% filter(country == "AR")
  tidy.daily.testing.output_AR  = tidy.daily.testing.output %>% filter(country == "AR")
  tidy.cumulative.output_AR  = tidy.cumulative.output %>% filter(country == "AR")
  tidy.cumulative.testing.output_AR  = tidy.cumulative.testing.output %>% filter(country == "AR")
   
  
  ## 2.4 Save data----
  fwrite(tidy.daily.data.output_AR, file = "SALURBAL_AR_covid 3-25-22/SALUBRAL_AR_COVID19_trends_cases_death.csv")
  fwrite(tidy.daily.testing.output_AR, file = "SALURBAL_AR_covid 3-25-22/SALURBAL_AR_COVID19_trends_tests_positivity.csv")
  fwrite(tidy.cumulative.output_AR,"SALURBAL_AR_covid 3-25-22/SALURBAL_AR_COVID19_cumulative_cases_death.csv")
  fwrite(tidy.cumulative.testing.output_AR,"SALURBAL_AR_covid 3-25-22/SALURBAL_AR_COVID19_cumulative_tests_positivity.csv")
}
