source(system.file("Stonkalysis", 'libraries.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'about.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'configuration.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'fundamental.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'financial.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'comparison.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'technical.r', package = "Stonkalysis"), local=TRUE)
source(system.file("Stonkalysis/ui", 'notes.r', package = "Stonkalysis"), local=TRUE)

ui <- dashboardPage(skin = "blue",
	dashboardHeader(
		title = "Stonkalysis",
		titleWidth=150
	),
	dashboardSidebar(
		sidebarMenu(
			uiOutput("tickers"),
			menuItem("About", tabName = "About", icon = icon("question")),
			menuItem("Configuration", tabName = "Configuration", icon = icon("cogs")),
			menuItem("Fundamental", tabName = "Fundamental", icon = icon("calculator")),
			menuItem("Financial Data", tabName = "Financial", icon = icon("money-bill-wave")),
			menuItem("Comparison", tabName = "Comparison", icon = icon("balance-scale-left")),
			menuItem("Technical", tabName = "Technical", icon = icon("chart-bar")),
			menuItem("Notes", tabName = "Technical", icon = icon("pen"))
		),
		width = 150
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


