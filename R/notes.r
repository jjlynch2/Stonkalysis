#write notes to cache directory
saveNotes <- function(notes, ticker, cache_path) {
	if(!dir.exists(paste(cache_path, ticker, sep=""))) {
		dir.create(paste(cache_path, ticker, sep=""))
	}
	saveRDS(notes, file=paste(cache_path, ticker, "/notes.Rds",sep=""))
}

#read notes from cache directory
readNotes <- function(ticker, cache_path) {
	if(file.exists(paste(cache_path, ticker, "/notes.Rds",sep=""))) {
		notes <- readRDS(file=paste(cache_path, ticker, "/notes.Rds",sep=""))
		return(notes)
	}
	return("Enter notes here....")
}
