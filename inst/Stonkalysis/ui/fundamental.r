fundamental_ui <- tabItem(tabName = "Fundamental",
	h2("Fundamental"),
	tabsetPanel(id="tabSelected",
		tabPanel("Profile",
			br(),
			fluidRow(
				column(6,
					sidebarPanel(
						uiOutput("profile1")
					,width=12)
				),
				column(6,
					sidebarPanel(
						uiOutput("profile2")
					,width=12)
				)
			),
			fluidRow(
				column(6,
					sidebarPanel(
						uiOutput("ownership_title"),
						uiOutput("instutitional_ui_plot"),
						uiOutput("ownership")
					,width=12)
				),
				column(6,
					sidebarPanel(
						uiOutput("insider_title"),
						uiOutput("ownership_ui_plot"),
						uiOutput("insider")
					,width=12)
				)
			)
		),
		tabPanel("Current Metrics"), ###each one will have raw data tables + graphs + regression predictions?
		tabPanel("Annual Metrics"),
		tabPanel("Quarterly Metrics"),
		tabPanel("DCF")
	)

)
