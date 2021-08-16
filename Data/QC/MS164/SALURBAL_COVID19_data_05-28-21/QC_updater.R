### Lets check cumulatively if there are discrpancies between L1 and L2

xwalk_sal_br

dfCumL1 = full_br_l1 %>%
  group_by(salid1) %>% 
  summarise(cum_cases = sum(confirmed),
         cum_deaths = sum(deaths)) %>% 
  ungroup() %>% 
  pivot_longer(cols = -salid1, names_to = "type")

dfCumL2 = full_br_l2 %>%
  group_by(salid2) %>% 
  summarise(cum_cases = sum(confirmed),
            cum_deaths = sum(deaths)) %>% 
  ungroup() %>% 
  left_join(xwalk_sal_br %>% select(salid1,salid2))

dfCumL1Aggregated = dfCumL2 %>% 
  group_by(salid1)%>%
  summarise(cum_cases = sum(cum_cases),
            cum_deaths = sum(cum_deaths)) %>% 
  ungroup() %>% 
  pivot_longer(cols = -salid1, names_to = "type", values_to="value_agg")


dfCumL1 %>% 
  left_join(dfCumL1Aggregated) %>% 
  mutate(diff = value_agg - value) %>% 
  count(diff)
