fluidPage(

  titlePanel("DAPD valuation"),

  sidebarLayout(

    sidebarPanel(
      textInput("api_token", "Tushare API Token:"),
      textInput("ts_code", "Tushare Code:"),
      numericInput("div_yield", "Est. Dividen Yield:", 0.0, min = 0.0, step = 0.001),
      numericInput("risk_free", "Risk-free rate:", 0.026),
      numericInput("N", "Year to maturity:", 3, min = 1, step = 1),
      selectInput("mode", "Growth based on:", c("auto", "roe", "roa"), "auto"),
      dateInput("start_date", "Historic Growth Starts from:", lubridate::as_date("2010-01-01")),
      actionButton("calc", "Calculate")
    ),

    mainPanel(
      tableOutput("result")
    )

  )

)
