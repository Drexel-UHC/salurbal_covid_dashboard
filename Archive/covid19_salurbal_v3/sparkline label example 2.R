library(sparkline)

# example from documentation
#  http://omnipotent.net/jquery.sparkline/#tooltips
sparkline(
  c(1,3,5,3,8),
  type="bar", 
  tooltipFormat= '{{value:levels}} - {{value}}',
  tooltipValueLookups= htmlwidgets::JS(
    "
{
  levels: $.range_map({':2': 'Low', '3:6':'Medium', '7:':'High'})
}
"
  )
)

# custom tooltipFormatter
#  silly use of as.roman
sparkline(
  c(1,3,5,3,8),
  type="bar", 
  tooltipFormatter = htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  debugger;
  return %s[field[0].offset];
}",
      jsonlite::toJSON(paste0(as.roman(1:5)," value"))
    )
  )
)

#  silly use of as.roman
sparkline(
  c(1,3,5,3,8),
  type="bar", 
  tooltipFormatter = htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  debugger;
  return %s[field[0].offset];
}",
      jsonlite::toJSON(paste0(as.roman(1:5)," value"))
    )
  )
)

#  maybe more useful date
sparkline(
  c(1,3,5,3,8),
  type="bar", 
  tooltipFormatter = htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  debugger;
  return %s[field[0].offset] + '<br/>' + field[0].value;
}",
      jsonlite::toJSON(
        format(
          seq.Date(as.Date("2015-01-01"),by="month",length.out =5)
        )
      )
    )
  )
)

#  try something with array of arrays line
library(xts)

ap <- as.xts(AirPassengers)
sparkline(
  jsonlite::toJSON(
    # get array of arrays [[date1,y1],[date2,y2],...]
    t(mapply(
      function(x,y){c(as.numeric(as.Date(x)),y)},
      index(ap),
      as.vector(ap[,1])
    ))
  ),
  type = "line",
  tooltipFormatter = htmlwidgets::JS(
    "
function(sp, opts, fld){
  debugger;
  return [
    '<span style=\"font-size:20px;\">',
    (new Date(fld.x*24*60*60*1000)).toString().split(' ').slice(1,4).join(' '),
    '</span>',
    fld.y + ' miles'
  ].join('<br/>');
}
"
  ),
  width = 300,
  height = 100
)