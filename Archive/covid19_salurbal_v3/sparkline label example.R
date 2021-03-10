library(DT)
library(sparkline)
library(dplyr)
library(tidyr)
library(htmlwidgets)

ChickWeight %>% 
  mutate(cut_weight = as.numeric(cut(weight, c(0, 50, 100, 150, 200, 300) , labels = c(1:5)))) %>% 
  group_by(Chick, cut_weight) %>% 
  tally %>% 
  complete(cut_weight = c(1:5), fill = list(n = 0)) %>% 
  summarize(
    cut_weight = spk_chr(
      cut_weight,
      type = "bar",
      barWidth = 20,
      barSpacing = 5,
      highlightColor = 'orange',
      tooltipFormat = '{{offset:levels}} : {{value}}',
      tooltipValueLookups = list(
        levels = list( 
          '0' = '0-50',
          '1' = '51-100',
          '2' = '101-150',
          '3' = '151-200',
          '4' = '201-300'
        )
      )
    )
  ) %>%
  datatable(
    escape = FALSE,
    ,
    options = list(
      fnDrawCallback = htmlwidgets::JS(
        '
// not the best way but works fairly well
function(){
  HTMLWidgets.staticRender();
}
'
      )
    )
  ) %>%
  spk_add_deps()