#### string_packages_uhc(): return a string of packages loaded  ######
string_packages_uhc = function(){(.packages()) %>% paste0("'",.,"'")%>% paste0(collapse = ",")}


#### 1. plot_lockdown_effect():  ######
# plot_lockdown_effect(tidy.full.rolling.country,"Brazil","Yes")
# df_tmp =tidy.full.rolling.country;loc_tmp="Argentina"; smooth_tmp ="yes"
plot_lockdown_effect = function(df_tmp,loc_tmp, smooth_tmp ){
  x = tibble()
  if (smooth_tmp == "no"){
    x = df_tmp %>% 
      filter(loc == loc_tmp)%>% 
      select(-rollsum) %>% 
      rename(rollsum = daily_counts)
  }
  
  else {x= df_tmp %>% filter(loc == loc_tmp)}
  
  cols = col_rolling
  start_tmp = x$start %>% unique()
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p = x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_line(aes(color=type)) +
    scale_color_manual(values=cols)+
    scale_y_continuous(limits=c(0, NA),
                       sec.axis = sec_axis(trans = ~./factorpeak,
                                           name = "New deaths per day (5 day avg)"))+
    scale_x_date(breaks="2 days")+
    guides(color=F)+
    labs(y="New cases per day (5 day avg)",
         x="",
         title=loc_tmp)+
    theme_bw() +
    theme(plot.title = element_text(size=22),
          axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
  grid.arrange(p,
               lockdown_legend, nrow = 2, heights = c(10, 1))
}



plot_lockdown_effect_state = function(df_tmp, loc_tmp, state_tmp, smooth_tmp  ){
  x = tibble()
  if (smooth_tmp == "no"){
    x = df_tmp %>% 
      filter(loc == loc_tmp,
             state == state_tmp)%>% 
      select(-rollsum) %>% 
      rename(rollsum = daily_counts)
  }
  else {x= df_tmp %>% 
    filter(loc == loc_tmp,
           state == state_tmp)}
  
  cols = col_rolling

  start_tmp = x$start %>% unique()
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p=x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    geom_vline(xintercept = start_tmp+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    geom_line(aes(color=type)) +
    scale_color_manual(values=cols)+
    scale_y_continuous(limits=c(0, NA),
                       sec.axis = sec_axis(trans = ~./factorpeak,
                                           name = "New deaths per day (5 day avg)"))+
    scale_x_date(breaks="2 days")+
    guides(color=F)+
    labs(y="New cases per day (5 day avg)",
         x="",
         title=state_tmp)+
    theme_bw() +
    theme(plot.title = element_text(size=22),
          axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.right = element_line(color = cols[[2]]),
          axis.text.y.right=element_text(color=cols[[2]]),
          axis.ticks.y.right=element_line(color=cols[[2]]),
          axis.title.y.right =element_text(color=cols[[2]]),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
  grid.arrange(p,
               lockdown_legend, nrow = 2, heights = c(10, 1))
}
# plot_lockdown_effect_state(tidy.full.rolling.state,"Brazil","Sao Paulo","yes")
# df_tmp =tidy.full.rolling.state;loc_tmp="Brazil";state_tmp = "Sao Paulo"; smooth_tmp ="yes"

#### 2. Base Map  ######

basemap = leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE )) %>%
  setView(lng = -20.11915, lat = 8.517508,zoom = 3)  

