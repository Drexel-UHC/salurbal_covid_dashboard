


View(tidy.daily.data2)


tidy.daily.data2 %>% 
  filter(count_raw<0) %>% 
  count(level, country, salid) %>% 
  arrange(desc(n))

## Negative daily values... drops.. 
mun_tmp = xwalk_sal_br %>% 
  filter(salid1 == 102194) %>% pull(mun)
raw_br_mun %>% 
  filter(mun%in%mun_tmp) %>% 
  ggplot(aes(date, confirmed, col =mun))+
  geom_line()
