library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

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
			tabItem(tabName = "About",
				h2("About")
			),
			tabItem(tabName = "Configuration",
				h2("Configuration"),
				tabPanel("Add Ticker",
					fluidRow(
						column(3,
							sidebarPanel(
								uiOutput("available_tickers"),
								actionButton("add_tickers","Add Tickers", icon = icon("plus"))
							,width=12)
						),
						column(3,
							sidebarPanel(
								uiOutput("cached_tickers"),
								actionButton("update_tickers","Update Tickers", icon = icon("refresh"))
							,width=12)
						),
						column(3,
							sidebarPanel(
								uiOutput("delete_tickers"),
								actionButton("delete_ticker_button","Delete Tickers", icon = icon("minus"))
							,width=12)
						)
					),
					br(),
					fluidRow(
						column(12,
							DT::dataTableOutput('cache_table')
						)
					)

				)
			),
			tabItem(tabName = "Financial",
				h2("Financial"),
				tabsetPanel(id="tabSelected",
					tabPanel("TTM"),
					tabPanel("Annual"),
					tabPanel("Quarterly")
				)
			),
			tabItem(tabName = "Fundamental",
				h2("Fundamental"),
				tabsetPanel(id="tabSelected",
					tabPanel("Profile"),
					tabPanel("Current Metrics"), ###each one will have raw data tables + graphs + regression predictions?
					tabPanel("Annual Metrics"),
					tabPanel("Quarterly Metrics"),
					tabPanel("DCF")
				)
			
			),
			tabItem(tabName = "Comparison",
				h2("Comparison"),
				tabsetPanel(id="tabSelected",
					tabPanel("Metrics"),
					tabPanel("Correlation") #include correlation between open, close, low, and high
				)
			),
			tabItem(tabName = "Technical",
				h2("Technical"),
				tabsetPanel(id="tabSelected",
					tabPanel("MARS"), #multiple adaptive regression splines? 
					tabPanel("LM"), #linear model
					tabPanel("MACDaddy") #provides thresholds for buying and selling based on stretched points?
				)
			)
	 	)
	)
)


