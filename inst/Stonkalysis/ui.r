source(system.file("Stonkalysis", 'libraries.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'about.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'configuration.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'fundamental.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'financial.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'comparison.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'technical.r', package = "Stonkalysis"), local=TRUE)

ui <- dashboardPage(skin = "green",
	dashboardHeader(title = "Stonkalysis"),
	dashboardSidebar(
		uiOutput("tickers"),
		menuItem("About", tabName = "About", icon = icon("question")),
		menuItem("Configuration", tabName = "Configuration", icon = icon("cogs")),
		menuItem("Fundamental", tabName = "Fundamental", icon = icon("calculator")),
		menuItem("Financial", tabName = "Financial", icon = icon("money-bill-wave")),
		menuItem("Comparison", tabName = "Comparison", icon = icon("balance-scale-left")),
		menuItem("Technical", tabName = "Technical", icon = icon("chart-bar"))
	),
	dashboardBody(
		tabItems(
			about_ui,
			configuration_ui,
			financial_ui,
			fundamental_ui,
			comparison_ui,
			technical_ui
	 	)
	)
)


