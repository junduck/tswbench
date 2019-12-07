function(input, output) {

  value <- eventReactive(input$calc, {
    api <- tswbench::TushareApi(input$api_token)
    tswbench::dapdv(api = api,
                    ts_code = toupper(input$ts_code),
                    div_rate = input$div_rate,
                    discount_rate = input$dist_rate,
                    N = input$N,
                    start_date = input$start_date)
  })

  output$result <- renderTable({
    value()
  })
}
