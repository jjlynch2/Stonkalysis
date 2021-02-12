#libraries
library(rjson)

#variables
ticker <- "AAPL"
API_LIMIT <- 60

######################################################Polygon.io API
apikey_poly<- c("")
API_URL_poly <- "https://api.polygon.io/v2/reference/financials/"
cache_path <- "/home/fynchy/research/VALAPP/data/"

#write a cache for this. Two caches? one for each API?
getFinance <- function(ticker) {
	URL_annual <- paste(API_URL_poly, ticker, "?limit=100", "&type=Y", "&apiKey=", apikey_poly, sep="")
	URL_quarterly <- paste(API_URL_poly, ticker, "?limit=100", "&type=Q", "&apiKey=", apikey_poly, sep="")
	annual <- fromJSON(file = URL_annual)
	quarterly <- fromJSON(file = URL_quarterly)
	return(list(annual[[2]], quarterly[[2]]))
}
######################################################Polygon.io API

######################################################Alpha Vantage API
apikey_alpha <- c("")
API_URL_alpha <- "https://www.alphavantage.co/query?function="
company_profile_call <- paste(API_URL_alpha, "OVERVIEW", sep="")
quote_call <- paste(API_URL_alpha, "GLOBAL_QUOTE", sep="")
daily_call <- paste(API_URL_alpha, "TIME_SERIES_DAILY", sep="")
weekly_call <- paste(API_URL_alpha, "TIME_SERIES_WEEKLY", sep="")
monthly_call <- paste(API_URL_alpha, "TIME_SERIES_MONTHLY", sep="")
API_LIMIT_alpha <- 60

#API calls
getProfile <- function(ticker) {
	URL <- paste(company_profile_call, "&symbol=", ticker, "&apikey=", apikey_alpha, sep="")
	results <- fromJSON(file = URL)
	results <- t(unlist(results))
	return(results)
}

getDaily <- function(ticker) {
	URL <- paste(daily_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey_alpha, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}

getWeekly <- function(ticker) {
	URL <- paste(weekly_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey_alpha, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}

getMonthly <- function(ticker) {
	URL <- paste(monthly_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey_alpha, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}
######################################################Alpha Vantage API
