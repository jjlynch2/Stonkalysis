#this needs a way to filter by date using the UI
#Need a way to choose the starting and stop date. Can I display that in the UI somehow? Calendar?
#could use to pre-filter whats available between them, then have two drop downs with the second filtered to be later than the first
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#

######################################################company to company valuation metrics
#dailyCorrelation <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	open <- cor(df1$open, df2$open)
#	high <- cor(df1$high, df2$high)
#	low <- cor(df1$low, df2$low)
#	close <- cor(df1$close, df2$close)
#	volume <- cor(df1$volume, df2$volume)
#	return(data.frame(open, high, low, close, volume))
#}

#weeklyCorrelation <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	open <- cor(df1$open, df2$open)
#	high <- cor(df1$high, df2$high)
#	low <- cor(df1$low, df2$low)
#	close <- cor(df1$close, df2$close)
#	volume <- cor(df1$volume, df2$volume)
#	return(data.frame(open, high, low, close, volume))
#}

#monthlyCorrelation <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	open <- cor(df1$open, df2$open)
#	high <- cor(df1$high, df2$high)
#	low <- cor(df1$low, df2$low)
#	close <- cor(df1$close, df2$close)
#	volume <- cor(df1$volume, df2$volume)
#	return(data.frame(open, high, low, close, volume))
#}

#dailyVolumeDifference <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	volumeMeanDiff <- mean(abs(df1$volume - df2$volume))
#	volumeSdDiff <- sd(abs(df1$volume - df2$volume))
#	return(data.frame(volumeMeanDiff, volumeSdDiff))
#}

#weeklyVolumeDifference <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	volumeMeanDiff <- mean(abs(df1$volume - df2$volume))
#	volumeSdDiff <- sd(abs(df1$volume - df2$volume))
#	return(data.frame(volumeMeanDiff, volumeSdDiff))
#}

#monthlyVolumeDifference <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	volumeMeanDiff <- mean(abs(df1$volume - df2$volume))
#	volumeSdDiff <- sd(abs(df1$volume - df2$volume))
#	return(data.frame(volumeMeanDiff, volumeSdDiff))
#}

#earningPerShareDifference <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	return(data.frame(first = df1$earningsPerBasicShare, second = df2$earningsPerBasicShare))
#}

#debtToEquityDifference <- function(df1, df2) {
#	df1 <- df1 %>% filter(date %in% df2$date)
#	df2 <- df2 %>% filter(date %in% df1$date)
#	return(data.frame(first = df1$debtToEquityRatio, second = df2$debtToEquityRatio))
#}
#Histos for financial ratios for annual + quartlery they can be in different tabs

######################################################company to company valuation metrics
