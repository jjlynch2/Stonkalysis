notes_ui <- tabItem(tabName = "Notes",
	column(12,
		fluidRow(
			box(
				title = "Notes",
				solidHeader=TRUE,
				width=12,
				status="primary",
				collapsible=TRUE,
				uiOutput("notes")
			)
		)
	)
)
