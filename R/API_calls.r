#pulls advanced financial data
getFinance <- function(ticker, APIURL, apikey) {
	URL_annual <- paste(APIURL, "/time-series/fundamentals/", ticker, "/annual?last=10","&token=", apikey, sep="")
	URL_quarterly <- paste(APIURL, "/time-series/fundamentals/", ticker, "/quarterly?range=20q","&token=", apikey, sep="")
	annual <- fromJSON(file = URL_annual)
	quarterly <- fromJSON(file = URL_quarterly)
	return(list(annual, quarterly))
}

#advanced statistics with ratios pre-calculated
getAdvancedStats <- function(ticker, APIURL, apikey) {
	URL_advancedStats <- paste(APIURL, "/stock/", ticker, "/advanced-stats","?token=", apikey, sep="")
	advanced <- fromJSON(file = URL_advancedStats)
	return(advanced)
}

#pulls the company profile
getProfile <- function(ticker, APIURL, apikey) {
	URL_profile <- paste(APIURL, "/stock/", ticker, "/company","?token=", apikey, sep="")
	profile <- fromJSON(file = URL_profile)
	return(profile)
}

#pulls maximum amount of daily historical prices
getChartData <- function(ticker, APIURL, apikey) {
	URL_chartData <- paste(APIURL, "/stock/", ticker, "/chart/","max?token=", apikey, sep="")
	chartData <- fromJSON(file = URL_chartData)
	return(chartData)
}

#pulls insider summary
getInsiderSummary <- function(ticker, APIURL, apikey) {
	URL_insider <- paste(APIURL, "/stock/", ticker, "/insider-summary", "?token=", apikey, sep="")
	insider <- fromJSON(file = URL_insider)
	return(insider)
}

#pulls institutinal wnership
getInstitutionalOwnership <- function(ticker, APIURL, apikey) {
	URL_institutional <- paste(APIURL, "/stock/", ticker, "/institutional-ownership", "?token=", apikey, sep="")
	institutional <- fromJSON(file = URL_institutional)
	return(institutional)
}

#pulls available tickers for IEX API
getTickers <- function(APIURL, apikey) {
	URL_tickers <- paste(APIURL, "/ref-data/symbols?token=", apikey, sep="")
	available_tickers <- fromJSON(file = URL_tickers)
	temp_list <- do.call(rbind, available_tickers)
	available_tickers <- unlist(temp_list[,1])
	return(available_tickers)
}
