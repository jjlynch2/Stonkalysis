fundamental_ui <- tabItem(tabName = "Fundamental",
	h2("Fundamental"),
	tabsetPanel(id="tabSelected",
		tabPanel("Profile"),
		tabPanel("Current Metrics"), ###each one will have raw data tables + graphs + regression predictions?
		tabPanel("Annual Metrics"),
		tabPanel("Quarterly Metrics"),
		tabPanel("DCF")
	)

)
