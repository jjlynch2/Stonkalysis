notes <- reactiveValues(notes = "test")

output$notes <- renderUI ({
	aceEditor("ace_notes", value=notes$notes, wordWrap = TRUE, height = "600px", mode = "plain_text", theme = "dawn")
})

observeEvent(input$ace_notes, {
	saveNotes(input$ace_notes, input$ticker, cache_path)
})
