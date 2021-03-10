string_packages_uhc = function(){(.packages()) %>% paste0("'",.,"'")%>% paste0(collapse = ",")}

format_gg = function(ggplot_tmp){
  ggplot_tmp+theme(strip.text.x = element_text(size = 15),
                   plot.title = element_text(size = 17),
                   plot.subtitle = element_text(size = 15), 
                   axis.title = element_text(size = 15),
                   axis.text =  element_text(size = 15),
                   legend.title = element_text(size = 15),
                   legend.text =  element_text(size = 15)
  )
}
geom_text_repel_size = 6

plot_cumulative_count_onset = function(df_tmp){
  title_tmp = df_tmp$title %>% unique()
p = df_tmp %>% 
  ggplot(aes(x=days.since.100, y=confirmed)) +
  geom_line(data=template_count, aes(x=days.since.100, y=y), lty=2, color="black")+
  geom_line(data=template_count, aes(x=days.since.100, y=y2), lty=2, color="blue")+
  geom_line(data=template_count, aes(x=days.since.100, y=y3), lty=2, color="red")+
  geom_line(data=template_count, aes(x=days.since.100, y=y4), lty=2, color="green")+
  geom_line(data=template_count, aes(x=days.since.100, y=y5), lty=2, color="grey") +
  geom_line(data =df_tmp, aes(group=loc, col = loc, size = loc))+
  geom_text_repel(data=df_tmp %>% filter(lastday==1),
                  aes(label=loc, col = loc),size = geom_text_repel_size,
                  segment.alpha = 0)+
  scale_color_manual(values= df_tmp %>% select(loc, colors) %>% distinct() %>% pull(colors))+
  scale_size_manual(values = df_tmp %>% select(loc, size) %>% distinct() %>% pull(size) )+
  scale_linetype_manual(values=c(2, 1))+
  scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_tmp,days.since.100))), by=5), limits=c(0, as.numeric(max(pull(df_tmp,days.since.100)))+5))+
  scale_y_log10(limits=c(100, 10^nchar(trunc(max(pull(df_tmp,confirmed))))))+
  annotation_logticks(sides="l") +
  labs(title=title_tmp,
       x="Days from epidemic onset (>=100 cases)", y="Confirmed Cases")+
  theme_bw() +
  theme(panel.grid.minor.x = element_line(),
        legend.position = 'none')
format_gg(p)
}


# df_tmp = df.c %>%
#   filter(level == "country")%>%
#   filter(loc%in%countries_salurbal)  %>%
#   filter(rate =="rate") %>% 
#   arrange(loc)
plot_cumulative_count_rate_onset = function(df_tmp){
  title_tmp = df_tmp$title %>% unique()
  p=df_tmp %>% 
    ggplot(aes(x=days.since.100, y=confirmed)) +
    geom_line(data=template_count_rate, aes(x=days.since.100, y=y), lty=2, color="black")+
    geom_line(data=template_count_rate, aes(x=days.since.100, y=y2), lty=2, color="blue")+
    geom_line(data=template_count_rate, aes(x=days.since.100, y=y3), lty=2, color="red")+
    geom_line(data=template_count_rate, aes(x=days.since.100, y=y4), lty=2, color="green")+
    geom_line(data=template_count_rate, aes(x=days.since.100, y=y5), lty=2, color="grey") +
    geom_line(data =df_tmp, aes(group=loc, col = loc, size = loc))+
    geom_text_repel(data=df_tmp %>% filter(lastday==1),
                    aes(label=loc, col = loc),size = geom_text_repel_size,
                    segment.alpha = 0)+
    scale_color_manual(values= df_tmp %>% select(loc, colors) %>% distinct() %>% pull(colors))+
    scale_size_manual(values = df_tmp %>% select(loc, size) %>% distinct() %>% pull(size) )+
    scale_linetype_manual(values=c(2, 1))+
    scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_tmp,days.since.100)))+5, by=5), limits=c(0, as.numeric(max(pull(df_tmp,days.since.100)))+5))+
    scale_y_log10(limits=c(1, 10^nchar(trunc(max(pull(df_tmp,confirmed))))))+
    annotation_logticks(sides="l") +
    labs(title=title_tmp,
         x="Days from epidemic onset (>=1 cases 1 M)", y="Cases Per 1 Million")+
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          legend.position = 'none')
  format_gg(p)
}
#
#
plot_cumulative_death_onset = function(df_tmp){
  title_tmp = df_tmp$title %>% unique()
  p=df_tmp %>%
    ggplot(aes(x=days.since.10, y=deaths)) +
    geom_line(data=template_death, aes(x=days.since.10, y=y), lty=2, color="black")+
    geom_line(data=template_death, aes(x=days.since.10, y=y2), lty=2, color="blue")+
    geom_line(data=template_death, aes(x=days.since.10, y=y3), lty=2, color="red")+
    geom_line(data=template_death, aes(x=days.since.10, y=y4), lty=2, color="green")+
    geom_line(data=template_death, aes(x=days.since.10, y=y5), lty=2, color="grey")+
    geom_line(data =df_tmp, aes(group=loc, col = loc, size = loc))+
    geom_text_repel(data=df_tmp %>% filter(lastday==1),
                    aes(label=loc, col = loc), size = geom_text_repel_size,
                    segment.alpha = 0)+
    scale_color_manual(values= df_tmp %>% select(loc, colors) %>% distinct() %>% pull(colors))+
    scale_size_manual(values = df_tmp %>% select(loc, size) %>% distinct() %>% pull(size) )+
    scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_tmp,days.since.10))), by=5), limits=c(0, as.numeric(max(pull(df_tmp,days.since.10)))+5))+
    scale_y_log10(limits=c(10, 10^nchar(trunc(max(pull(df_tmp,deaths))))))+
    annotation_logticks(sides="l") +
    labs(title=title_tmp,
         x="Days from epidemic onset (>=10 Deaths)", y="Deaths per 1 M")+
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          legend.position = 'none')
  format_gg(p)
}

# df_tmp = df.d %>%
#   filter(level == "country")%>%
#   filter(loc%in%countries_salurbal)  %>%
#   filter(rate =="rate") %>%
#   arrange(loc)
plot_cumulative_death_rate_onset = function(df_tmp){
  title_tmp = df_tmp$title %>% unique()
  p=df_tmp %>% 
    ggplot(aes(x=days.since.10, y=deaths)) +
    geom_line(data=template_death_rate, aes(x=days.since.10, y=y), lty=2, color="black")+
    geom_line(data=template_death_rate, aes(x=days.since.10, y=y2), lty=2, color="blue")+
    geom_line(data=template_death_rate, aes(x=days.since.10, y=y3), lty=2, color="red")+
    geom_line(data=template_death_rate, aes(x=days.since.10, y=y4), lty=2, color="green")+
    geom_line(data=template_death_rate, aes(x=days.since.10, y=y5), lty=2, color="grey") +
    geom_line(data =df_tmp, aes(group=loc, col = loc, size = loc))+
    geom_text_repel(data=df_tmp %>% filter(lastday==1),
                    aes(label=loc, col = loc),size = geom_text_repel_size,
                    segment.alpha = 0)+
    scale_color_manual(values= df_tmp %>% select(loc, colors) %>% distinct() %>% pull(colors))+
    scale_size_manual(values = df_tmp %>% select(loc, size) %>% distinct() %>% pull(size) )+
    scale_linetype_manual(values=c(2, 1))+
    scale_x_continuous(breaks=seq(0, as.numeric(max(pull(df_tmp,days.since.10)))+5, by=5), limits=c(0, as.numeric(max(pull(df_tmp,days.since.10)))+5))+
    scale_y_log10(limits=c(1, 10^nchar(trunc(max(pull(df_tmp,deaths))))))+
    annotation_logticks(sides="l") +
    labs(title=title_tmp,
         x="Days from epidemic onset (>=1 Deaths per 10 M)", y="Death Per 10 Million")+
    theme_bw() +
    theme(panel.grid.minor.x = element_line(),
          legend.position = 'none')
  format_gg(p)
}


plot_cumulative_date = function(df_tmp,str_tmp){
  title_tmp = paste(str_tmp,unique(df_tmp$title))
  p=df_tmp %>%
    ggplot(aes(x=date, y=n)) +
    geom_line(aes(group=loc, col = loc, size = loc))+
    geom_text_repel(data=df_tmp %>% arrange(loc, desc(date)) %>%
                      filter(!duplicated(loc), n>1),
                    aes(label=loc, x=date, col = loc), hjust=0,size = geom_text_repel_size,
                    segment.color = "gray",
                    nudge_x = max(df_tmp$date)+3,
                    direction     = "y",
                    hjust = 0.5)+
    scale_color_manual(values= df_tmp %>% select(loc, colors) %>% distinct() %>% pull(colors))+
    scale_size_manual(values = df_tmp %>% select(loc, size) %>% distinct() %>% pull(size) )+
    scale_x_date(limits=c(filter(df_tmp,n> 1) %>% pull(date) %>% min(), max(df_tmp$date)+10))+
    scale_y_log10(limits=c(1, 10^nchar(trunc(max(pull(df_tmp,n))))  )  )+
    annotation_logticks(sides="l") +
    labs(title=title_tmp,
         x="Date",
         y = str_tmp)+
    guides(color=F)+
    theme_bw() +
    theme(legend.position = 'none',
          panel.grid.minor.x = element_line())
  format_gg(p)
}



# plot_lockdown_effect(tidy.full.rolling.country,"Brazil","Yes")
# df_tmp =tidy.daily.country %>% filter(smooth_days ==7);loc_tmp="Argentina"; smooth_tmp ="yes"
plot_lockdown_effect = function(df_tmp,loc_tmp ){
  x= df_tmp %>% filter(loc == loc_tmp)
  cols = col_rolling
  start_tmp = x$start %>% unique()
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p = x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
    geom_vline(xintercept = start_tmp+0, lty=2,size =1.5, color="red")+
    # geom_vline(xintercept = start_tmp+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
    # geom_vline(xintercept = start_tmp+5.2+5.2+5.2+5.2+5.2+5.2+5.2+5.2, lty=2,size =1.5, color="grey")+
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
  grid.arrange(format_gg(p),
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
  grid.arrange(format_gg(p),
               lockdown_legend, nrow = 2, heights = c(10, 1))
}
# plot_lockdown_effect_state(tidy.full.rolling.state,"Brazil","Sao Paulo","yes")
# df_tmp =tidy.full.rolling.state;loc_tmp="Brazil";state_tmp = "Sao Paulo"; smooth_tmp ="yes"
plot_lockdown_effect_salurbal = function(df_tmp ){
  x= df_tmp 
  state_tmp = unique(df_tmp$loc)
  cols = col_rolling
  factorpeak=(x %>% filter(type=="confirmed") %>% pull(rollsum) %>% max)/
    (x %>% filter(type=="deaths") %>% pull(rollsum) %>% max)
  p = x  %>%
    mutate(rollsum=ifelse(type=="deaths", rollsum*factorpeak, rollsum)) %>% 
    ggplot(aes(x=date, y=rollsum, group=type)) +
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
  format_gg(p)
}
# df_tmp = tidy.daily.subnational %>% filter(country == "Mexico") %>% filter(level == "L1") %>% filter(loc == "Cancun") 
plot_lockdown_effect_salurbal_confirmed = function(df_tmp){
  x= df_tmp
  state_tmp = unique(df_tmp$loc)
  cols = col_rolling[1]
  p = x  %>%
    ggplot(aes(x=date, y=rollsum)) +
    geom_line(aes(color=type)) +
    scale_color_manual(values=cols)+
    scale_x_date(breaks="2 days")+
    guides(color=F)+
    labs(y="New cases per day (5 day avg)",
         x="",
         title=state_tmp)+
    theme_bw() +
    theme(plot.title = element_text(size=22),
          axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=.5),
          axis.line.y.left = element_line(color = cols[[1]]),
          axis.text.y.left=element_text(color=cols[[1]]),
          axis.ticks.y.left=element_line(color=cols[[1]]),
          axis.title.y.left =element_text(color=cols[[1]]))
  format_gg(p)
}
#### 2. Base Map  ######
basemap  = leaflet(options = leafletOptions(preferCanvas = TRUE,zoomControl = FALSE)) %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE )) %>%
  setView(lng = -60.11915, lat = -10.517508,zoom = 3.25)  


