configuration_ui <- tabItem(tabName = "Configuration",
	tabPanel("Add Ticker",
		fluidRow(
			column(3,
				box(
					title = "Available Tickers",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("available_tickers"),
					actionButton("update_available_tickers","Update Ticker List", icon = icon("refresh")),
					actionButton("add_tickers","Add Tickers", icon = icon("plus"))
				)
			),
			column(3,
				box(
					title = "Update Cached Tickers",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("cached_tickers"),
					actionButton("update_tickers","Update Tickers", icon = icon("refresh"))
				)
			),
			column(3,
				box(
					title = "Delete Cached Tickers",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("delete_tickers"),
					actionButton("delete_ticker_button","Delete Tickers", icon = icon("trash"))
				)
			),
			column(3,
				box(
					title = "API Key",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					uiOutput("api_key"),
					actionButton("add_key","Add API key", icon = icon("plus"))
				)
			)
		),
		br(),
		fluidRow(
			column(12,
				box(
					title = "Cached Tickers",
					solidHeader=TRUE,
					width=12,
					status="primary",
					collapsible = TRUE,
					DT::dataTableOutput('cache_table')
				)
			)
		)

	)
)
