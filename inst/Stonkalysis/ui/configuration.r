configuration_ui <- tabItem(tabName = "Configuration",
	h2("Configuration"),
	tabPanel("Add Ticker",
		fluidRow(
			column(3,
				sidebarPanel(
					uiOutput("available_tickers"),
					actionButton("add_tickers","Add Tickers", icon = icon("plus"))
				,width=12)
			),
			column(3,
				sidebarPanel(
					uiOutput("cached_tickers"),
					actionButton("update_tickers","Update Tickers", icon = icon("refresh"))
				,width=12)
			),
			column(3,
				sidebarPanel(
					uiOutput("delete_tickers"),
					actionButton("delete_ticker_button","Delete Tickers", icon = icon("trash"))
				,width=12)
			),
			column(3,
				sidebarPanel(
					uiOutput("api_key"),
					actionButton("add_key","Add API key", icon = icon("plus"))
				,width=12)
			)
		),
		br(),
		fluidRow(
			column(12,
				DT::dataTableOutput('cache_table')
			)
		)

	)
)
