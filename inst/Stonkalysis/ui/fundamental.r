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
						br(),
						plotOutput("institutional_plot"),
						br(),
						uiOutput("ownership")
					,width=12)
				),
				column(6,
					sidebarPanel(
						uiOutput("insider_title"),
						br(),
						plotOutput("ownership_plot"),
						br(),
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
