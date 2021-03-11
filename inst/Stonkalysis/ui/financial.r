financial_ui <- tabItem(tabName = "Financial",
	h2("Financial Data"),
	tabsetPanel(id="tabSelected",
		tabPanel("TTM",
			fluidRow(
				column(12,
					br(),
					DT::dataTableOutput('financial_table_ttm')
				)
			)
		),
		tabPanel("Annual",
			fluidRow(
				column(12,
					br(),
					DT::dataTableOutput('financial_table_annual')
				)
			)
		),

		tabPanel("Quarterly",
			fluidRow(
				column(12,
					br(),
					DT::dataTableOutput('financial_table_quarterly')
				)
			)
		)
	)
)
