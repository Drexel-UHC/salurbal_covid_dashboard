library(tidyverse)
library(data.table)
library(janitor)

# Import raw data
l1_ar_file = "raw-data/PRJAR_L1AD_20200719.csv"
l2_ar_file = "raw-data/PRJAR_L2_20200719.csv"

## Function to format age groups
formatAgeGroups = function(file){
  ## Import  
  df_raw = fread(file) %>% clean_names()
  
  ## Get level
  level_tmp = df_raw %>% select(contains('salid')) %>% names()
  if (length(level_tmp)>1){
    level_tmp = "salid2"
    df_raw = df_raw %>% 
      select(-salid1) %>% 
      mutate(prjpop = prjl2pop)
  } else {
    level_tmp = "salid1"
    df_raw = df_raw %>% 
      mutate(prjpop = prjl1adpop)
  }
  
  ## Clean
  df_clean_tmp = df_raw %>% 
    ## Import and filter to max year
    filter(year== max(year)) %>% 
    ## Group prjage5c into 10 year age_group
    rename(age_min = prjage5c) %>% 
    mutate(age_max = case_when(
      age_min==0~1, 
      age_min==1~4,
      TRUE~age_min+4),
      age_group_raw = paste0(age_min,"-",age_max),
      age_group = case_when(
        age_max<=9~'0-9',
        age_max<=19~'10-19',
        age_max<=29~'20-29',
        age_max<=39~'30-39',
        age_max<=49~'40-49',
        age_max<=59~'50-59',
        age_max<=69~'60-69',
        age_max<=79~'70-79',
        age_max<=84~'80-84',
        TRUE~NA_character_
      ) ) %>% 
    ## Aggreate pop estiamtes acoess gender by age_group
    rename(salid = contains('salid')) %>% 
    group_by(salid, age_group) %>% 
    summarize(pop = sum(prjpop)) %>% 
    ungroup() %>% 
    rename(!!level_tmp := salid )
    
  return(df_clean_tmp)
}


## Calcualte pop by age for AR
df_pop_l1_ar = formatAgeGroups(l1_ar_file)
df_pop_l2_ar = formatAgeGroups(l2_ar_file)


## Save
save(df_pop_l1_ar, df_pop_l2_ar, file  = "processed-data/pop_ar_by_age.rdata")

