ticker_df <- reactiveValues(ticker_df = data.frame())

output$tickers <- renderUI({
	temp <- av_tickers$av_tickers
	selectInput(inputId = "ticker", label = "Select Ticker", choices = temp)
})

observeEvent(input$ticker, {
	showModal(modalDialog(title = "Reading cache....", easyClose = FALSE, footer = NULL))
	ticker_df$ticker_df <- readCache(input$ticker, cache_path)
	update_table_ttm() #current ttm data
	update_table_annual() #annual data
	update_table_quarterly() #quarterly data
	update_table_chart() #historical
	update_table_chart_v() #volatility
	notes$notes <- readNotes(input$ticker, cache_path) #read notes
	updateAceEditor(session, "ace_notes", value=notes$notes, wordWrap = TRUE, mode = "plain_text", theme = "dawn")
	removeModal()
})
