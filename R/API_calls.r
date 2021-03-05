#pulls advanced financial data
getFinance <- function(ticker, APIURL, apikey) {
	URL_annual <- paste(APIURL, "/time-series/fundamentals/", ticker, "/annual?last=30","&token=", apikey, sep="")
	URL_quarterly <- paste(APIURL, "/time-series/fundamentals/", ticker, "/quarterly?range=120q","&token=", apikey, sep="")
	URL_ttm <- paste(APIURL, "/time-series/fundamentals/", ticker, "/ttm","?token=", apikey, sep="")
	annual <- fromJSON(file = URL_annual)
	quarterly <- fromJSON(file = URL_quarterly)
	ttm <- fromJSON(file = URL_ttm)
	return(list(annual, quarterly, ttm))
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

#pulls institutinal ownership
getInstitutionalOwnership <- function(ticker, APIURL, apikey) {
	URL_institutional <- paste(APIURL, "/stock/", ticker, "/institutional-ownership", "?token=", apikey, sep="")
	institutional <- fromJSON(file = URL_institutional)
	return(institutional)
}

#pulls fund ownership
getFundOwnership <- function(ticker, APIURL, apikey) {
	URL_fund <- paste(APIURL, "/stock/", ticker, "/fund-ownership", "?token=", apikey, sep="")
	fund <- fromJSON(file = URL_fund)
	return(fund)
}

#pulls available tickers for IEX API
getTickers <- function(APIURL, apikey) {
	URL_tickers <- paste(APIURL, "/ref-data/symbols?token=", apikey, sep="")
	available_tickers <- fromJSON(file = URL_tickers)
	temp_list <- do.call(rbind, available_tickers)
	available_tickers <- unlist(temp_list[,1])
	return(available_tickers)
}
