technical_ui <- tabItem(tabName = "Technical",
	h2("Technical"),
	tabsetPanel(id="tabSelected",
		tabPanel("MARS"), #multiple adaptive regression splines? 
		tabPanel("LM"), #linear model
		tabPanel("MACD") #provides thresholds for buying and selling based on stretched points?
	)
)
