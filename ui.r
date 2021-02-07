library(shinydashboard)

dashboardPage(
dashboardHeader(),
dashboardSidebar(
	menuItem("Configuration", tabName = "dashboard", icon = icon("dashboard")),
	menuItem("Fundamental", tabName = "widgets", icon = icon("th")),
	menuItem("Financial", tabName = "widgets", icon = icon("th")),
	menuItem("Compare", tabName = "widgets", icon = icon("th")),
	menuItem("Technical", tabName = "widgets", icon = icon("th"))
  ),
  dashboardBody()
)
