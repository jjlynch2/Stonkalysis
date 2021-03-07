fundamental_ui <- tabItem(tabName = "Fundamental",
	h2("Fundamental"),
	tabsetPanel(id="tabSelected",
		tabPanel("Profile",
			br(),
			fluidRow(
				column(6,
					sidebarPanel(
						uiOutput("profile1_title"),
						tableOutput("profile1"),
						br(),
						uiOutput("profile2")
					,width=12),
					fluidRow(
						column(6,
							sidebarPanel(
								tableOutput("am_1")
							,width=12)
						),
						column(6,
							sidebarPanel(
								tableOutput("am_2")
							,width=12)
						)
					),
					fluidRow(
						column(6,
							sidebarPanel(
								tableOutput("am_3")
							,width=12)
						),
						column(6,
							sidebarPanel(
								tableOutput("am_4")
							,width=12)
						)
					)
				),
				column(6,
					sidebarPanel(
						uiOutput("ownership_title"),
						uiOutput("instutitional_ui_plot"),
						tableOutput("ownership")
					,width=12),
					sidebarPanel(
						uiOutput("fund_title"),
						uiOutput("fund_ui_plot"),
						tableOutput("fund")
					,width=12),
					sidebarPanel(
						uiOutput("insider_title"),
						uiOutput("ownership_ui_plot"),
						tableOutput("insider")
					,width=12)
				)
			)
		),
		tabPanel("Historical Data",
			br(),
			fluidRow(
				column(12,
					sidebarPanel(
						fluidRow(
							column(2,
								uiOutput("plotly_control_ui")
							),
							column(1,
								uiOutput("plotly_color")
							),
							column(1,
								uiOutput("plotly_color2")
							),
							column(8,
								uiOutput("plotly_control_ui2")
							)
						),
						fluidRow(
							uiOutput("plotly_chart_ui")
						),
					,width=12)
				)
			),
			fluidRow(
				column(12,
					sidebarPanel(
						br(),
						DT::dataTableOutput('chart_table'),
					,width=12)
				)
			)
		),
		tabPanel("Historical Volatility",
			br(),
			fluidRow(
				column(12,
					sidebarPanel(
						fluidRow(
							column(2,
								uiOutput("plotly_control_ui_v")
							),
							column(10,
								uiOutput("plotly_control_ui2_v")
							)
						),
						fluidRow(
							uiOutput("plotly_chart_ui_v")
						),
						fluidRow(
							uiOutput("table_title"),
							uiOutput("plotly_chart_ui_v_p")
						),
					,width=12)
				)
			),
			fluidRow(
				column(12,
					sidebarPanel(
						br(),
						DT::dataTableOutput('chart_table_v'),
					,width=12)
				)
			)
		), #provide volatility graph and table for open, close, high, low, etc, see market Chameleon for example
		
		###each one will have raw data tables + graphs + regression predictions? Make sure to allow user select of years with a selectizeInput somewhere. 
		tabPanel("Annual",
			br(),
			tabsetPanel(
				tabPanel("Income Statement",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("netincome"),
								uiOutput("income_plots"),
								br(),
								uiOutput("income_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("operating"),
								uiOutput("operating_plots"),
								br(),
								uiOutput("operating_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("revenue"),
								uiOutput("revenue_plots"),
								br(),
								uiOutput("revenue_table")
							,width=12)
						)
					)
				),
				tabPanel("Balance Statement",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("assets_balance"),
								uiOutput("assets_balance_plot"),
								br(),
								uiOutput("assets_balance_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("liabilities_balance"),
								uiOutput("liabilities_balance_plot"),
								br(),
								uiOutput("liabilities_balance_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("debt_to_asset_balance"),
								uiOutput("debt_to_asset_balance_plot"),
								br(),
								uiOutput("debt_to_asset_balance_table")
							,width=12)
						),
					)
				),
				tabPanel("Cash Statement",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("operating_cashflow"),
								uiOutput("operating_cashflow_plot"),
								br(),
								uiOutput("operating_cashflow_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("investing_cashflow"),
								uiOutput("investing_cashflow_plot"),
								br(),
								uiOutput("investing_cashflow_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("financing_cashflow"),
								uiOutput("financing_cashflow_plot"),
								br(),
								uiOutput("financing_cashflow_table")
							,width=12)
						),
					)
				)
			)
		),
		tabPanel("Quarterly",
			br()
		),
		tabPanel("DCF",
			br()
		),
		tabPanel("DDM",
			br()
		),
		tabPanel("Custom Plot",
		
		)
	)

)
