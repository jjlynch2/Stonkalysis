server <- function(input, output) {
	#API stuff
	APIURL <- "https://sandbox.iexapis.com/stable"
	apikey <- readLines(system.file("Stonkalysis/server", 'apikey', package = "Stonkalysis"))
	
	#libraries
	source(system.file("Stonkalysis", 'libraries.r', package = "Stonkalysis"), local=TRUE)
	
	#general input from ticker
	source(system.file("Stonkalysis/server", 'input.r', package = "Stonkalysis"), local=TRUE)
	
	#financial raw data code
	source(system.file("Stonkalysis/server", 'financial.r', package = "Stonkalysis"), local=TRUE)
	
	#configuration / cache code
	source(system.file("Stonkalysis/server", 'cache_system.r', package = "Stonkalysis"), local=TRUE)
	
	#notes code
	source(system.file("Stonkalysis/server", 'notes.r', package = "Stonkalysis"), local=TRUE)
	
	#fundamental source code
	source(system.file("Stonkalysis/server", 'fundamental_overview.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'fundamental_historical.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'fundamental_volatility.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'fundamental_annual.r', package = "Stonkalysis"), local=TRUE)
}
