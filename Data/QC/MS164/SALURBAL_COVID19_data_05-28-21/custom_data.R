# Check  raw.daily.subnational for L1/L2 consistency------
{
  # read in cumulative data
  dfTrendsRaw = raw.daily.subnational %>% as_tibble()
  
  # Filter L1 level data
  dfTrendsL1 = dfTrendsRaw %>% 
    filter(country == "Brazil", level == "L1") %>% 
    select(salid1 = salid, date,type, value)
  
  # Aggregate L2 level to L1 level
  dfTrendsL1Aggregated = dfTrendsRaw %>% 
    filter(country == "Brazil", level == "L2") %>% 
    select(salid2 = salid, date,type, value) %>% 
    left_join(xwalk_sal_br %>% select(salid1,salid2)) %>% 
    group_by(salid1,date,type)%>%
    summarise(value_agg = sum(value)) %>% 
    ungroup()
  
  # Check difference between L1 value and L2-to-L1 Aggreagation value
  dfMergeTrends =  dfTrendsL1 %>%  left_join(dfTrendsL1Aggregated) %>% filter(salid1!="BR888")%>% 
    mutate(diff = value_agg - value)
  dfMergeTrends %>% count(diff) 
}
# Check  tidy.daily.data.output_1 for L1/L2 consistency ------
{
  # read in cumulative data
  dfTrendsRaw = tidy.daily.data.output_1_uncensored %>% as_tibble()

  # Filter L1 level data
  dfTrendsL1 = dfTrendsRaw %>% 
    filter(country == "BR", level == "L1") %>% 
    select(salid1 = salid, date,type,value= count_raw)
  
  # Aggregate L2 level to L1 level
  dfTrendsL1Aggregated = dfTrendsRaw %>% 
    filter(country == "BR", level == "L2") %>% 
    select(salid2 = salid, date,type, count_raw) %>% 
    left_join(xwalk_sal_br %>% select(salid1,salid2)) %>% 
    group_by(salid1,date,type)%>%
    summarise(value_agg = sum(count_raw)) %>% 
    ungroup()
  
  # Check difference between L1 value and L2-to-L1 Aggreagation value
  dfMergeTrends =  dfTrendsL1 %>%  left_join(dfTrendsL1Aggregated) %>% filter(salid1!="BR888")%>% 
    mutate(diff = value_agg - value)
  dfMergeTrends %>% count(diff) 
  
  ### This is where the problem is. Some daily data is messed up and we censor when daily values are negative. 
  dfMergeTrends %>% 
    filter(value<0)
  
}
#  Data (Censored)-----
{
  tidy.daily.data.output_1 = tidy.daily.data %>%
    bind_rows(raw.daily.subnational) %>%
    filter(rate !="rate") %>%
    filter(!type%in%c( "positivity","tests")) %>%
    rename(count_smoothed = rate) %>%
    mutate(count_smoothed = count_smoothed %>%
             recode("count"="count_7dayMA",
                    "raw count"="count_raw")) %>%
    pivot_wider(names_from = count_smoothed, values_from = value) %>%
    left_join(bind_rows(pop_l1,pop_l2) %>% select(salid = loc, pop)) %>%
    select(level, country, salid,loc, type, date,count_raw,count_7dayMA)%>%
    arrange(level, country, salid, loc, type,date) %>%
    mutate_at(vars(count_raw, count_7dayMA), ~ifelse(.x<0,NA,.x))  %>%
    left_join(raw.daily.subnational.rates %>%
                select(level, salid, type, date,rate_7dayMA, rate_label ),
              by =c("level", "salid","type", "date")) %>% 
    left_join(xwalk_countries) %>% 
    select(country= iso2, level, salid, type, date, 
           count_raw, count_7dayMA,
           rate_7dayMA, rate_label) %>% 
    distinct()
  
  tidy.daily.data.output = tidy.daily.data.output_1 %>% 
    select(-rate_label) %>% 
    pivot_wider(names_from = type, values_from = c(count_raw, count_7dayMA, rate_7dayMA)) %>% 
    select(country, level, salid, date, 
           cases_raw = count_raw_cases,
           cases_7dayMA = count_7dayMA_cases,
           cases_per_1M = rate_7dayMA_cases, 
           deaths_raw = count_raw_deaths,
           deaths_7dayMA = count_7dayMA_deaths,
           deaths_per_10M = rate_7dayMA_deaths) %>% 
    mutate_at(vars(cases_raw,cases_7dayMA,cases_per_1M,deaths_raw,deaths_7dayMA,deaths_per_10M),
              ~round(.x,0))
  
}
# Data (Uncensored)-----
{
  tidy.daily.data.output_1_uncensored = tidy.daily.data %>%
    bind_rows(raw.daily.subnational) %>%
    filter(rate !="rate") %>%
    filter(!type%in%c( "positivity","tests")) %>%
    rename(count_smoothed = rate) %>%
    mutate(count_smoothed = count_smoothed %>%
             recode("count"="count_7dayMA",
                    "raw count"="count_raw")) %>%
    pivot_wider(names_from = count_smoothed, values_from = value) %>%
    left_join(bind_rows(pop_l1,pop_l2) %>% select(salid = loc, pop)) %>%
    select(level, country, salid,loc, type, date,count_raw,count_7dayMA)%>%
    arrange(level, country, salid, loc, type,date) %>%
    # mutate_at(vars(count_raw, count_7dayMA), ~ifelse(.x<0,NA,.x))  %>%
    left_join(raw.daily.subnational.rates %>%
                select(level, salid, type, date,rate_7dayMA, rate_label ),
              by =c("level", "salid","type", "date")) %>% 
    left_join(xwalk_countries) %>% 
    select(country= iso2, level, salid, type, date, 
           count_raw, count_7dayMA,
           rate_7dayMA, rate_label) %>% 
    distinct()
  
  tidy.daily.data.output_uncensored = tidy.daily.data.output_1 %>% 
    select(-rate_label) %>% 
    pivot_wider(names_from = type, values_from = c(count_raw, count_7dayMA, rate_7dayMA)) %>% 
    select(country, level, salid, date, 
           cases_raw = count_raw_cases,
           cases_7dayMA = count_7dayMA_cases,
           cases_per_1M = rate_7dayMA_cases, 
           deaths_raw = count_raw_deaths,
           deaths_7dayMA = count_7dayMA_deaths,
           deaths_per_10M = rate_7dayMA_deaths) %>% 
    mutate_at(vars(cases_raw,cases_7dayMA,cases_per_1M,deaths_raw,deaths_7dayMA,deaths_per_10M),
              ~round(.x,0))
  
}
# Save Data ----- 