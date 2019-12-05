function(input, output) {

  value <- eventReactive(input$calc, {
    api <- tswbench::TushareApi(input$api_token)
    tswbench::dapdv(api = api,
                    ts_code = toupper(input$ts_code),
                    div_yield = input$div_yield,
                    risk_free = input$risk_free,
                    N = input$N,
                    mode = input$mode,
                    start_date = input$start_date)
  })

  output$result <- renderTable({
    df <- as.data.frame(value())
    colnames(df) <- c("Estimate Market Cap", "Estimate Price", "Target Market Cap", "Target Price")
    df
  })
}
