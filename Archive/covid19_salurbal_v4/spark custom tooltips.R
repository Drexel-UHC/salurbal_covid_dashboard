spk_tool <- function(labels) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  return %s[field[0].offset];
}",
      jsonlite::toJSON(labels)
    )
  )
}

# create data
spark_data1<-tibble(
  id = c("a","b"),  
  spark = c(spk_chr(1:3,type="bar", tooltipFormatter=spk_tool(c("C","D","E"))),
            spk_chr(3:1,type="bar",tooltipFormatter=spk_tool(c("C","D","E"))))
)

ui <- tagList(
  fluidPage(
    DT::dataTableOutput("tbl")
  ),
  #### add dependencies for sparkline in advance
  #### since we know we are using
  htmlwidgets::getDependency("sparkline", "sparkline")
) 

server <- function(input, output) {
  
  output$tbl <- DT::renderDataTable({
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    dt <-  DT::datatable(
      as.data.frame(spark_data1),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        #### add the drawCallback to static render the sparklines
        ####   staticRender will not redraw what has already been rendered
        drawCallback =  cb
      )
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
