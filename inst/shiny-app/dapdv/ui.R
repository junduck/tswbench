fluidPage(

  titlePanel("DAPD valuation"),

  sidebarLayout(

    sidebarPanel(
      textInput("api_token", "Tushare API Token:"),
      textInput("ts_code", "Tushare Code:"),
      numericInput("dist_rate", "Discount Rate:", 0.1),
      numericInput("N", "Years of Stable Growth:", 3, min = 1, step = 1),
      dateInput("start_date", "Historic Growth From:", lubridate::as_date("2010-01-01")),
      actionButton("calc", "Calculate"),
      width = 3
    ),

    mainPanel(
      tableOutput("result"),
      plotOutput("roe"),
      plotOutput("pb")
    )

  )

)
