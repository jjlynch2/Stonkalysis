server <- function(input, output) {
	APIURL <- "https://sandbox.iexapis.com/stable"
	apikey <<- readLines(system.file("Stonkalysis/server", 'apikey', package = "Stonkalysis"))
	source(system.file("Stonkalysis", 'libraries.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'input.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'fundamental.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'financial.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'cache_system.r', package = "Stonkalysis"), local=TRUE)
}
