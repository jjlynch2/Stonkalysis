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
		titleWidth=150,
		dropdownMenuOutput("tickers")
	),
	dashboardSidebar(
		sidebarMenu(id = "sidebar",
			menuItem("About", tabName = "About", icon = icon("question")),
			menuItem("Configuration", tabName = "Configuration", icon = icon("cogs")),
			menuItem("Fundamental", tabName = "Fundamental", icon = icon("calculator")),
			menuItem("Financial Data", tabName = "Financial", icon = icon("money-bill-wave")),
			menuItem("Comparison", tabName = "Comparison", icon = icon("balance-scale-left")),
			menuItem("Technical", tabName = "Technical", icon = icon("chart-bar")),
			menuItem("Notes", tabName = "Notes", icon = icon("pen")),
			conditionalPanel(condition="input.sidebar == 'Fundamental' && input.tabSelected1 == 'Annual'",uiOutput("annual_control"),uiOutput("annual_color_o")),
			conditionalPanel(condition="input.sidebar == 'Fundamental' && input.tabSelected1 == 'Quarterly'",uiOutput("quarterly_control"),uiOutput("quarterly_color_o"))
		),
		width = 150
	),
	dashboardBody(
		tags$head(
			tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}"),
        	tags$head(tags$style(HTML(".selectize-input {max-height: 20px; max-width: 100px; margin-bottom: -26px}")))
		),
		tags$style(HTML("
			.box.box-solid.box-primary>.box-header {
				  color:#ffffff;
				  background:#2c3e50
			}

			.box.box-solid.box-primary{
				border-bottom-color:#2c3e50;
				border-left-color:#2c3e50;
				border-right-color:#2c3e50;
				border-top-color:#2c3e50;
				background:#f5f5f5;
			}

			.box.box-primary>.box-header {
				color:#000000;
				background:#f5f5f5
			}

			.box.box-primary{
				border-bottom-color:#2c3e50;
				border-left-color:#2c3e50;
				border-right-color:#2c3e50;
				border-top-color:#2c3e50;
				background:#f5f5f5;
			}
			.nav-tabs-custom .nav-tabs li.active {
				border-top-color: #2c3e50;
			}	
			.nav-tabs-custom .tab-content {
				background:#ecf0f5;
			}	

		")),
		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
		),
		tabItems(
			about_ui,
			configuration_ui,
			financial_ui,
			fundamental_ui,
			comparison_ui,
			technical_ui,
			notes_ui
	 	)
	)
)


