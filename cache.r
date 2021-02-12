######################################################Polygon.io API
readCache_poly <- function(ticker) {
	file_list <- list.files(cache_path)
	success <- FALSE
	financial <- c()
	if(length(file_list) > 0) {
		for(f in file_list) {
			if(f == ticker) {
				financial <- readRDS(file=paste(cache_path, ticker, "/financial.Rds",sep=""))
				success <- TRUE
			}
		}
	}
	if(!success) {
		return(FALSE)
	} else {
		return(financial)
	}
}

#write cache - gets called from update
writeCache_poly <- function(ticker, financial) {
	if(!dir.exists(paste(cache_path, ticker, sep=""))) {
		dir.create(paste(cache_path, ticker, sep=""))
	}
	saveRDS(financial, file=paste(cache_path, ticker, "/financial.Rds",sep=""))
}
######################################################Polygon.io API

######################################################Alpha Vantage API
readCache_alpha <- function(ticker) {
	file_list <- list.files(cache_path)
	success <- FALSE
	financial <- c()
	if(length(file_list) > 0) {
		for(f in file_list) {
			if(f == ticker) {
				profile <- readRDS(file=paste(cache_path, ticker, "/profile.Rds",sep=""))
				daily <- readRDS(file=paste(cache_path, ticker, "/daily.Rds",sep=""))
				weekly <- readRDS(file=paste(cache_path, ticker, "/weekly.Rds",sep=""))
				monthly <- readRDS(file=paste(cache_path, ticker, "/monthly.Rds",sep=""))
				success <- TRUE
			}
		}
	}
	if(!success) {
		return(FALSE)
	} else {
		return(list(profile, daily, weekly, monthly))
	}
}

#write cache - gets called from update
writeCache_alpha <- function(ticker, profile, daily, weekly, monthly) {
	if(!dir.exists(paste(cache_path, ticker, sep=""))) {
		dir.create(paste(cache_path, ticker, sep=""))
	}
	saveRDS(profile, file=paste(cache_path, ticker, "/profile.Rds",sep=""))
	saveRDS(daily, file=paste(cache_path, ticker, "/daily.Rds",sep=""))
	saveRDS(weekly, file=paste(cache_path, ticker, "/weekly.Rds",sep=""))
	saveRDS(monthly, file=paste(cache_path, ticker, "/monthly.Rds",sep=""))
}
######################################################Alpha Vantage API

######################################################Common cache functions
#update cache
updateCache <- function(ticker) {
	profile <- getProfile(ticker)
	daily <- getProfile(ticker)
	weekly <- getProfile(ticker)
	monthly <- getProfile(ticker)
	financial <- getFinance(ticker)
	writeCache_poly(ticker, financial)
	writeCache_alpha(ticker, profile, daily, weekly, monthly)
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
######################################################Common cache functions
