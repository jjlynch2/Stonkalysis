notes_ui <- tabItem(tabName = "Notes",
	h2("Ticker Notes"),
	column(12,
		fluidRow(
			br(),
			uiOutput("notes")
		)
	)
)
