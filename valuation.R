#libraries
library(rjson)

#API settings
apikey <- c("")
API_URL <- "https://www.alphavantage.co/query?function="
income_call <- paste(API_URL, "INCOME_STATEMENT", sep="")
balance_call <- paste(API_URL, "BALANCE_SHEET", sep="")
cash_call <- paste(API_URL, "CASH_FLOW", sep="")
company_profile_call <- paste(API_URL, "OVERVIEW", sep="")
quote_call <- paste(API_URL, "GLOBAL_QUOTE", sep="")
daily_call <- paste(API_URL, "TIME_SERIES_DAILY", sep="")
weekly_call <- paste(API_URL, "TIME_SERIES_WEEKLY", sep="")
monthly_call <- paste(API_URL, "TIME_SERIES_MONTHLY", sep="")
API_LIMIT <- 60

#Variables
ticker <- "AAPL"
cache_path <- "/home/fynchy/research/VALAPP/data/"

#cache these with option to update cache
#API calls
getBalance <- function(ticker) {
	URL <- paste(balance_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	annual <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	cnames <- names(results[[2]][[1]])
	colnames(annual) <- cnames
	quarterly <- data.frame(matrix(unlist(results[[3]]), nrow=length(results[[3]]), byrow=TRUE))
	cnames <- names(results[[3]][[1]])
	colnames(quarterly) <- cnames
	return(list(annual, quarterly))
}

getCash <- function(ticker) {
	URL <- paste(cash_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	annual <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	cnames <- names(results[[2]][[1]])
	colnames(annual) <- cnames
	quarterly <- data.frame(matrix(unlist(results[[3]]), nrow=length(results[[3]]), byrow=TRUE))
	cnames <- names(results[[3]][[1]])
	colnames(quarterly) <- cnames
	return(list(annual, quarterly))
}

getIncome <- function(ticker) {
	URL <- paste(income_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	annual <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	cnames <- names(results[[2]][[1]])
	colnames(annual) <- cnames
	quarterly <- data.frame(matrix(unlist(results[[3]]), nrow=length(results[[3]]), byrow=TRUE))
	cnames <- names(results[[3]][[1]])
	colnames(quarterly) <- cnames
	return(list(annual, quarterly))
}

getProfile <- function(ticker) {
	URL <- paste(company_profile_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	results <- t(unlist(results))
	return(results)
}

getDaily <- function(ticker) {
	URL <- paste(daily_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}

getWeekly <- function(ticker) {
	URL <- paste(weekly_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}

getMonthly <- function(ticker) {
	URL <- paste(monthly_call, "&symbol=", ticker, "&outputsize=full", "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	df <- data.frame(matrix(unlist(results[[2]]), nrow=length(results[[2]]), byrow=TRUE))
	rnames <- names(results[[2]])
	cnames <- names(results[[2]][[1]])
	df <- cbind(rnames, df)
	colnames(df) <- c("date", "open", "high", "low", "close", "volume")
	return(df)
}

#update per minute while running
getQuote <- function(ticker) {
	URL <- paste(quote_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	return(results)
}

#read cache
readCache <- function(ticker) {
	file_list <- list.files(cache_path)
	success <- FALSE
	if(length(file_list) > 0) {
		for(f in file_list) {
			if(f == ticker) {
				profile <- read.csv(paste(cache_path, ticker, "/profile.csv",sep=""))
				income_a <- read.csv(paste(cache_path, ticker, "/annual_income.csv",sep=""))
				income_q <- read.csv(paste(cache_path, ticker, "/quarterly_income.csv",sep=""))
				balance_a <- read.csv(paste(cache_path, ticker, "/annual_balance.csv",sep=""))
				balance_q <- read.csv(paste(cache_path, ticker, "/quarterly_balance.csv",sep=""))
				cash_a <- read.csv(paste(cache_path, ticker, "/annual_cash.csv",sep=""))
				cash_q <- read.csv(paste(cache_path, ticker, "/quarterly_cash.csv",sep=""))
				daily_data <- read.csv(paste(cache_path, ticker, "/daily.csv",sep=""))
				weekly_data <- read.csv(paste(cache_path, ticker, "/weekly.csv",sep=""))
				monthly_data <- read.csv(paste(cache_path, ticker, "/monthly.csv",sep=""))
				success <- TRUE
			}
		}
	}
	if(!success) {
		return(FALSE)
	} else {
		return(list(profile, daily_data, weekly_data, monthly_data, list(income_a, income_q), list(balance_a, balance_q), list(cash_a, cash_q)))
	}
}

#write cache - gets called from update
writeCache <- function(ticker, profile, daily_data, weekly_data, monthly_data, income, balance, cash) {
	if(!dir.exists(paste(cache_path, ticker, sep=""))) {
		dir.create(paste(cache_path, ticker, sep=""))
	}
	write.csv(file = paste(cache_path, ticker, "/profile.csv", sep=""), x = profile, row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/daily.csv", sep=""), x = daily_data, row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/weekly.csv", sep=""), x = weekly_data, row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/monthly.csv", sep=""), x = monthly_data, row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/annual_income.csv", sep=""), x = income[[1]], row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/quarterly_income.csv", sep=""), x = income[[2]], row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/annual_balance.csv", sep=""), x = balance[[1]], row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/quarterly_balance.csv", sep=""), x = balance[[2]], row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/annual_cash.csv", sep=""), x = cash[[1]], row.names=FALSE)
	write.csv(file = paste(cache_path, ticker, "/quarterly_cash.csv", sep=""), x = cash[[2]], row.names=FALSE)
}

#update cache
updateCache <- function(ticker) {
	profile <- getProfile(ticker)
	daily_data <- getDaily(ticker)
	income <- getIncome(ticker)
	balance <- getBalance(ticker)
	cash <- getCash(ticker)
	Sys.sleep(API_LIMIT)
	weekly_data <- getWeekly(ticker)
	monthly_data <- getMonthly(ticker)
	writeCache(ticker, profile, daily_data, weekly_data, monthly_data, income, balance, cash)
	Sys.sleep(API_LIMIT)
}

#bulk update cache for all tickers previously cached. time intensive
updateCached <- function() {
	tickers <- list.files(cache_path)
	if(length(tickers) > 0) {
		for(t in tickers) {
			updateCache(t)
		}
	}
}

#calculates the effective tax rates over 5 years
getEffectiveTax <- function(income) {
	IBT <- income[[1]]$incomeBeforeTax
	ITE <- income[[1]]$incomeTaxExpense
	rates <- ITE / IBT
	return(rates)
}


#metrics from profile
#earnings-per-share
EPS <- function(df) {
	return(df[[1]]$EPS)
}

#price-to-earnings
PE_ratio <- function(df) {
	return(df[[1]]$PERatio)
}

#price-earnings-to-growth
PEG_ratio <- function(df) {
	return(df[[1]]$PEGRatio)
}

#Operating Margin TTM
Operating_Margin <- function(df) {
	return(df[[1]]$OperatingMarginTTM)
}

#price-to-sales TTM
PS_ratio <- function(df) {
	return(df[[1]]$PriceToSalesRatioTTM)
}

#price-to-book
PB_ratio <- function() {
	return(df[[1]]$PriceToBookRatio)
}

#beta 
Beta <- function(df) {
	return(df[[1]]$Beta)
}

#EV/EBITDA
EV_EBITDA <- function(df) {
	return(df[[1]]$EVToEBITDA)
}
#metrics from profile


##company to company valuation metrics
#provide correlations between the historical data between two companies
dailyCorrelation <- function(df, df2) {
	return(cor(df[[2]]$close, df2[[2]]$close))
}

weeklyCorrelation <- function(df, df2) {
	return(cor(df[[3]]$close, df2[[3]]$close))
}

monthlyCorrelation <- function(df, df2) {
	return(cor(df[[4]]$close, df2[[4]]$close))
}

dailyVolumeDifference <- function(df, df2) {
	return(list(df[[2]]$volume, df2[[2]]$volume, abs(df[[2]]$volume - df2[[2]]$volume)))
}

weeklyVolumeDifference <- function(df, df2) {
	return(list(df[[3]]$volume, df2[[3]]$volume, abs(df[[3]]$volume - df2[[3]]$volume)))
}

monthlyVolumeDifference <- function(df, df2) {
	return(list(df[[4]]$volume, df2[[4]]$volume, abs(df[[4]]$volume - df2[[4]]$volume)))
}
##company to company valuation metrics

#alpha vantage calls
quote_daily <- getQuote(ticker)





#TODO
EBITDA
Cash Flow
Free Cash Flow
Free Cash Flow to Equity
Unleveraged Free CashF low (this is also free cash flow to firm?)
Leveraged Discounted Cash Flow
Unleveraged Discounted Cash Flow
Dividend Discount Model
Weighted Average Cost of Capital (This is sometimes used as the discount rate?)

getEBITDA <- function(df) {
	nt <- df[[5]][[1]]$netIncome
	ie <- df[[5]][[1]]$interestExpense
	it <- df[[5]][[1]]$incomeTaxExpense
	de <- df[[7]][[1]]$depreciation
	return(nt + ie + it + de)
}

getCashFlow <- function(df) {
	return(df[[7]][[1]]$operatingCashflow)
}

getFreeCashFlow <- function(df) {
	return(getCashFlow(df) - df[[7]][[1]]$cashflowFromInvestment)
}

getFreeCashFlowtoEquity <- function(df) {
	getFreeCashFlow(df) + 
}

getUnleveredFreeCashFlow <- function(df) [

}

getDCF <- function() {

}

getDDM <- function() {

}

getWACC <- function() {

}

getGDP <- function() {
	#see https://cran.r-project.org/web/packages/WDI/WDI.pdf
}

getTenYearYield <- function() {
	#see https://www.business-science.io/finance/2020/02/21/tidy-discounted-cash-flow.html
}

#price-to-cash-flow
PtCF_ratio <- function() {

}

DtE_ratio <- function() {

}

Working_Capital <- function() {

}

Inventory_Turnover_ration <- function() {

}
