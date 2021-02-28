financial_ui <- tabItem(tabName = "Financial",
	h2("Financial"),
	tabsetPanel(id="tabSelected",
		tabPanel("TTM"),
		tabPanel("Annual"),
		tabPanel("Quarterly")
	)
)
