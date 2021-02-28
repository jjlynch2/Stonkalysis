server <- function(input, output) { 
	source(system.file("Stonkalysis", 'libraries.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'api.r', package = "Stonkalysis"), local=TRUE)
	source(system.file("Stonkalysis/server", 'cache_system.r', package = "Stonkalysis"), local=TRUE)
}
