comparison_ui <- tabItem(tabName = "Comparison",
	h2("Comparison"),
	tabsetPanel(id="tabSelected",
		tabPanel("Metrics"),
		tabPanel("Correlation") #include correlation between open, close, low, and high
	)
)
