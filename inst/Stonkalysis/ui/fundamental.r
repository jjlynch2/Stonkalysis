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
						)
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
		),
		tabPanel("Annual",
			br(),
			fluidRow(
				column(4,
					sidebarPanel(
						fluidRow(
							column(8,
								uiOutput("annual_control")
							),
							column(4,
								uiOutput("annual_color_o")
							)
						)
					,width=12)
				)
			),
			tabsetPanel(
				tabPanel("General Metrics",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("eps_title"),
								uiOutput("eps_plot"),
								br(),
								uiOutput("eps_table")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("roe_title"),
								uiOutput("roe_plot"),
								br(),
								uiOutput("roe_table")
							,width=12)
						
						),
						column(4,
							sidebarPanel(
								uiOutput("roa_title"),
								uiOutput("roa_plot"),
								br(),
								uiOutput("roa_table")
							,width=12)
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
				tabPanel("Balance",
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
				tabPanel("Cash",
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
			br(),
			fluidRow(
				column(4,
					sidebarPanel(
						fluidRow(
							column(8,
								uiOutput("quarterly_control")
							),
							column(4,
								uiOutput("quarterly_color_o")
							)
						)
					,width=12)
				)
			),
			tabsetPanel(
				tabPanel("General Metrics",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("eps_title_quarterly"),
								uiOutput("eps_plot_quarterly"),
								br(),
								uiOutput("eps_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("roe_title_quarterly"),
								uiOutput("roe_plot_quarterly"),
								br(),
								uiOutput("roe_table_quarterly")
							,width=12)
						
						),
						column(4,
							sidebarPanel(
								uiOutput("roa_title_quarterly"),
								uiOutput("roa_plot_quarterly"),
								br(),
								uiOutput("roa_table_quarterly")
							,width=12)
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
							sidebarPanel(
								uiOutput("netincome_quarterly"),
								uiOutput("income_plots_quarterly"),
								br(),
								uiOutput("income_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("operating_quarterly"),
								uiOutput("operating_plots_quarterly"),
								br(),
								uiOutput("operating_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("revenue_quarterly"),
								uiOutput("revenue_plots_quarterly"),
								br(),
								uiOutput("revenue_table_quarterly")
							,width=12)
						)
					)
				),
				tabPanel("Balance",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("assets_balance_quarterly"),
								uiOutput("assets_balance_plot_quarterly"),
								br(),
								uiOutput("assets_balance_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("liabilities_balance_quarterly"),
								uiOutput("liabilities_balance_plot_quarterly"),
								br(),
								uiOutput("liabilities_balance_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("debt_to_asset_balance_quarterly"),
								uiOutput("debt_to_asset_balance_plot_quarterly"),
								br(),
								uiOutput("debt_to_asset_balance_table_quarterly")
							,width=12)
						),
					)
				),
				tabPanel("Cash",
					br(),
					fluidRow(
						column(4,
							sidebarPanel(
								uiOutput("operating_cashflow_quarterly"),
								uiOutput("operating_cashflow_plot_quarterly"),
								br(),
								uiOutput("operating_cashflow_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("investing_cashflow_quarterly"),
								uiOutput("investing_cashflow_plot_quarterly"),
								br(),
								uiOutput("investing_cashflow_table_quarterly")
							,width=12)
						),
						column(4,
							sidebarPanel(
								uiOutput("financing_cashflow_quarterly"),
								uiOutput("financing_cashflow_plot_quarterly"),
								br(),
								uiOutput("financing_cashflow_table_quarterly")
							,width=12)
						),
					)
				)
			)
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
