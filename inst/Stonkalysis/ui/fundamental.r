fundamental_ui <- tabItem(tabName = "Fundamental",
	tabsetPanel(id="tabSelected",
		tabPanel("Profile",
			br(),
			fluidRow(
				column(6,
					box(
						title = "Overview",
						solidHeader=TRUE,
						tableOutput("profile1"),
						br(),
						uiOutput("profile2"),
						width=12,
						status="primary",
						collapsible = TRUE
					),
					fluidRow(
						column(6,
							box(
							    solidHeader=TRUE,
								tableOutput("am_1"),
								width=12,
								height="365",
								status="primary"
							)
						),
						column(6,
							box(
							    solidHeader=TRUE,
								tableOutput("am_2"),
								width=12,
								height="365",
								status="primary"
							)
						)
					),
					fluidRow(
						column(6,
							box(
							    solidHeader=TRUE,
								tableOutput("am_3"),
								width=12,
								height="365",
								status="primary"
							)
						),
						column(6,
							box(
							    solidHeader=TRUE,
								tableOutput("am_4"),
								width=12,
								height="365",
								status="primary"
							)
						)
					)
				),
				column(6,
					box(
						title = "Institutional Ownership (top 10)",
						solidHeader=TRUE,
						uiOutput("instutitional_ui_plot"),
						tableOutput("ownership"),
						width=12,
						status="primary",
						collapsible = TRUE
					),
					box(
						title = "Fund Ownership (top 10)",
						solidHeader=TRUE,
						uiOutput("fund_ui_plot"),
						tableOutput("fund"),
						width=12,
						status="primary",
						collapsible = TRUE
					),
					box(
						title = "Insider Trading (last 6 months)",
						solidHeader=TRUE,
						uiOutput("ownership_ui_plot"),
						tableOutput("insider"),
						width=12,
						status="primary",
						collapsible = TRUE
					)
				)
			)
		),
		tabPanel("Historical Data",
			br(),
			fluidRow(
				column(12,
					box(
						title = "Daily Price Chart",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
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
							column(12,
								uiOutput("plotly_chart_ui")
							)
						)
					)
				)
			),
			fluidRow(
				column(12,
					box(
						title = "Daily Price Data",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
						DT::dataTableOutput('chart_table')
					)
				)
			)
		),
		tabPanel("Historical Volatility",
			br(),
			fluidRow(
				column(12,
					box(
						title = "Daily Volatility Measures",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
						fluidRow(
							column(2,
								uiOutput("plotly_control_ui_v")
							),
							column(10,
								uiOutput("plotly_control_ui2_v")
							)
						),
						fluidRow(
							column(12,
								uiOutput("plotly_chart_ui_v")
							)
						),
						fluidRow(
							column(12,
								uiOutput("table_title"),
								uiOutput("plotly_chart_ui_v_p")
							)
						)
					)
				)
			),
			fluidRow(
				column(12,
					box(
						title = "Daily Volatility Data",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
						DT::dataTableOutput('chart_table_v'),
					)
				)
			)
		),
		tabPanel("Annual",
			br(),
			fluidRow(
				column(4,
					box(
						title = "Plot Controls",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
						fluidRow(
							column(8,
								uiOutput("annual_control")
							),
							column(4,
								uiOutput("annual_color_o")
							)
						)
					)
				)
			),
			tabsetPanel(
				tabPanel("General Metrics",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Earnings per Share",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("eps_plot"),
								br(),
								uiOutput("eps_table")
							)
						),
						column(4,
							box(
								title = "Return on Equity",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("roe_plot"),
								br(),
								uiOutput("roe_table")
							)
						
						),
						column(4,
							box(
								title = "Return on Assets",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("roa_plot"),
								br(),
								uiOutput("roa_table")
							)
						)
					
					#	column(4,
					#		sidebarPanel(
					#			br()       #Net Margin
					#		,width=12),
					#		sidebarPanel(
					#			br()      #BVPS
					#		,width=12)
					#	),
					#	column(4,
					#		sidebarPanel(
					#			br()      #CRPS
					#		,width=12),
					#		sidebarPanel(
					#			br()       #CFPS
					#		,width=12)
					#	)
					)
				),
				tabPanel("Income",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Net Income",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("income_plots"),
								br(),
								uiOutput("income_table")
							)
						),
						column(4,
							box(
								title = "Operating Income",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("operating_plots"),
								br(),
								uiOutput("operating_table")
							)
						),
						column(4,
							box(
								title = "Total Revenue",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("revenue_plots"),
								br(),
								uiOutput("revenue_table")
							)
						)
					)
				),
				tabPanel("Balance",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Total Assets",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("assets_balance_plot"),
								br(),
								uiOutput("assets_balance_table")
							)
						),
						column(4,
							box(
								title = "Total Liabilities",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("liabilities_balance_plot"),
								br(),
								uiOutput("liabilities_balance_table")
							)
						),
						column(4,
							box(
								title = "Debt to Asset Ratio",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("debt_to_asset_balance_plot"),
								br(),
								uiOutput("debt_to_asset_balance_table")
							)
						),
					)
				),
				tabPanel("Cash",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Cash Flow from Operations",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("operating_cashflow_plot"),
								br(),
								uiOutput("operating_cashflow_table")
							)
						),
						column(4,
							box(
								title = "Cash Flow from Investing",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("investing_cashflow_plot"),
								br(),
								uiOutput("investing_cashflow_table")
							)
						),
						column(4,
							box(
								title = "Cash Flow from Financing",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("financing_cashflow_plot"),
								br(),
								uiOutput("financing_cashflow_table")
							)
						),
					)
				)
			)
		),
		tabPanel("Quarterly",
			br(),
			fluidRow(
				column(4,
					box(
						title = "Plot Controls",
						solidHeader=TRUE,
						width=12,
						status="primary",
						collapsible = TRUE,
						fluidRow(
							column(8,
								uiOutput("quarterly_control")
							),
							column(4,
								uiOutput("quarterly_color_o")
							)
						)
					)
				)
			),
			tabsetPanel(
				tabPanel("General Metrics",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Earnings per Share",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("eps_plot_quarterly"),
								br(),
								uiOutput("eps_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Return on Equity",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("roe_plot_quarterly"),
								br(),
								uiOutput("roe_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Return on Assets",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("roa_plot_quarterly"),
								br(),
								uiOutput("roa_table_quarterly")
							)
						)
					
					#	column(4,
					#		sidebarPanel(
					#			br()       #Net Margin
					#		,width=12),
					#		sidebarPanel(
					#			br()      #BVPS
					#		,width=12)
					#	),
					#	column(4,
					#		sidebarPanel(
					#			br()      #CRPS
					#		,width=12),
					#		sidebarPanel(
					#			br()       #CFPS
					#		,width=12)
					#	)
					)
				),
				tabPanel("Income",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Net Income",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("income_plots_quarterly"),
								br(),
								uiOutput("income_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Operating Income",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("operating_plots_quarterly"),
								br(),
								uiOutput("operating_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Total Revenue",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("revenue_plots_quarterly"),
								br(),
								uiOutput("revenue_table_quarterly")
							)
						)
					)
				),
				tabPanel("Balance",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Total Assets",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("assets_balance_plot_quarterly"),
								br(),
								uiOutput("assets_balance_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Total Liabilities",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("liabilities_balance_plot_quarterly"),
								br(),
								uiOutput("liabilities_balance_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Debt to Asset Ratio",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("debt_to_asset_balance_plot_quarterly"),
								br(),
								uiOutput("debt_to_asset_balance_table_quarterly")
							)
						),
					)
				),
				tabPanel("Cash",
					br(),
					fluidRow(
						column(4,
							box(
								title = "Cash Flow from Operations",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("operating_cashflow_plot_quarterly"),
								br(),
								uiOutput("operating_cashflow_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Cash Flow from Investing",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("investing_cashflow_plot_quarterly"),
								br(),
								uiOutput("investing_cashflow_table_quarterly")
							)
						),
						column(4,
							box(
								title = "Cash Flow from Financing",
								solidHeader=TRUE,
								width=12,
								status="primary",
								collapsible = TRUE,
								uiOutput("financing_cashflow_plot_quarterly"),
								br(),
								uiOutput("financing_cashflow_table_quarterly")
							)
						),
					)
				)
			)
		),
		tabPanel("Discounted Cash Flow",
			br()
		),
		tabPanel("Dividend Discount Model",
			br()
		),
		tabPanel("Custom Plot",
		
		)
	)

)
