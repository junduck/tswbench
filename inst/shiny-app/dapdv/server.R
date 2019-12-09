function(input, output) {

  value <- eventReactive(input$calc, {
    api <- tswbench::TushareApi(input$api_token)
    tswbench::dapdv(api, ts_code = input$ts_code, discount_rate = input$dist_rate,
                    N = input$N, start_date = input$start_date, intraday_freq = input$freq)
  })

  output$result <- renderTable({
    value()
  })

  output$roe <- renderPlot({
    v <- value()
    tswbench::dapdv_plot(v, "ROE")
  })

  output$pb <- renderPlot({
    v <- value()
    tswbench::dapdv_plot(v, "PB")
  })
}
