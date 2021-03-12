#reads Cached data
readCache <- function(ticker, cache_path) {
	file_list <- list.files(cache_path)
	success <- FALSE
	financial <- c()
	if(length(file_list) > 0) {
		for(f in file_list) {
			if(f == ticker) {
				financial <- readRDS(file=paste(cache_path, ticker, "/financial.Rds",sep=""))
				profile <- readRDS(file=paste(cache_path, ticker, "/profile.Rds",sep=""))
				chart <- readRDS(file=paste(cache_path, ticker, "/chart.Rds",sep=""))
				advanced <- readRDS(file=paste(cache_path, ticker, "/advanced.Rds",sep=""))
				insider <- readRDS(file=paste(cache_path, ticker, "/insider.Rds",sep=""))
				institutional <- readRDS(file=paste(cache_path, ticker, "/institutional.Rds",sep=""))
				fund <- readRDS(file=paste(cache_path, ticker, "/fund.Rds",sep=""))
				cache_date <- readRDS(file=paste(cache_path, ticker, "/cache_date.Rds",sep=""))
				success <- TRUE

			}
		}
	}
	if(!success) {
		return(FALSE)
	} else {
		return(list(financial, profile, chart, advanced, cache_date, insider, institutional, fund))
	}
}

#write cache - gets called from update
writeCache<- function(ticker, financial, profile, chart, advanced, institutional, insider, cache_date, cache_path, fund) {
	if(!dir.exists(paste(cache_path, ticker, sep=""))) {
		dir.create(paste(cache_path, ticker, sep=""))
	}
	saveRDS(financial, file=paste(cache_path, ticker, "/financial.Rds",sep=""))
	saveRDS(profile, file=paste(cache_path, ticker, "/profile.Rds",sep=""))
	saveRDS(chart, file=paste(cache_path, ticker, "/chart.Rds",sep=""))
	saveRDS(advanced, file=paste(cache_path, ticker, "/advanced.Rds",sep=""))
	saveRDS(institutional, file=paste(cache_path, ticker, "/institutional.Rds",sep=""))
	saveRDS(insider, file=paste(cache_path, ticker, "/insider.Rds",sep=""))
	saveRDS(cache_date, file=paste(cache_path, ticker, "/cache_date.Rds",sep=""))
	saveRDS(fund, file=paste(cache_path, ticker, "/fund.Rds",sep=""))
}

#update cache
updateCache <- function(ticker, APIURL, apikey, cache_path) {
	profile <- getProfile(ticker, APIURL, apikey)
	chart <- getChartData(ticker, APIURL, apikey)
	advanced <- getAdvancedStats(ticker, APIURL, apikey)
	financial <- getFinance(ticker, APIURL, apikey)
	institutional <- getInstitutionalOwnership(ticker, APIURL, apikey)
	insider <- getInsiderSummary(ticker, APIURL, apikey)
	fund <- getFundOwnership(ticker, APIURL, apikey)
	cache_date <- cache_date <- c(paste(as.POSIXct(format(Sys.time()), tz="GMT")))
	writeCache(ticker, financial, profile, chart, advanced, institutional, insider, cache_date, cache_path, fund)
}

#bulk update cache for all tickers previously cached. time intensive
updateCached <- function(cache_path, APIURL, apikey) {
	tickers <- list.files(cache_path)
	if(length(tickers) > 0) {
		for(t in tickers) {
			updateCache(t, APIURL, apikey, cache_path)
		}
	}
}

#read from cache the available ticker list
readTickers <- function(cache_path) {
	file_list <- list.files(paste(cache_path, "/tickers.Rds",sep=""))
	if(length(file_list) > 0) {
		cache_date <- readRDS(file=paste(cache_path, "/tickers.Rds",sep=""))
	} else {
		return("Please update available tickers")
	}
}

#update the local cache with new available ticker list from API
updateTickers <- function(cache_path, APIURL, apikey) {
	available_tickers <- getTickers(APIURL, apikey)
	saveRDS(available_tickers, paste(cache_path, "/tickers.Rds",sep=""))
}
