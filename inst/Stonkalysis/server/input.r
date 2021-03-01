ticker_df <- reactiveValues(ticker_df = data.frame())

output$tickers <- renderUI({
	temp <- av_tickers$av_tickers
	selectInput(inputId = "ticker", label = "Select Ticker", choices = temp)
})

observeEvent(input$ticker, {
	ticker_df$ticker_df <- readCache(input$ticker, cache_path)
})
