fluidPage(

  titlePanel("DAPD valuation"),

  sidebarLayout(

    sidebarPanel(
      textInput("api_token", "Tushare API Token:"),
      textInput("ts_code", "Tushare Code:"),
      numericInput("div_rate", "Est. Dividen Rate:", 0.0, min = 0.0, step = 0.001),
      numericInput("dist_rate", "Discount Rate:", 0.1),
      numericInput("N", "Years of Stable Growth:", 3, min = 1, step = 1),
      dateInput("start_date", "Historic Growth From:", lubridate::as_date("2010-01-01")),
      actionButton("calc", "Calculate")
    ),

    mainPanel(
      tableOutput("result")
    )

  )

)
