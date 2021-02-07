#trading algorithm #1

library(pracma)
training_data <- read.csv("GE.csv")
ma_LONG <- 26 #days for long
ma_SHORT <- 12 #days for short
ma_SIGNAL <- 9
ma_DAYS <- 26 #days to use for average of absolute maximas

#training
portfolio <- 1000
portfolio2 <- 1000
shares <- 0
shares2 <- 0

average_price <- c()
number_of_buys <- 0
number_of_sells <- 0
counter <- 0
count <- 0

#MACD
long <- movavg(training_data[,5],ma_LONG, type="e")
short <- movavg(training_data[,5],ma_SHORT, type="e")
MACD <- short - long
SIGNAL <- movavg(MACD,ma_SIGNAL, type="e")
hist_difference <- MACD - SIGNAL
#p_n_avg <- check_sign(hist_difference)
#p_avg <- p_n_avg[1]
#n_avg <- p_n_avg[2]
	
	
#calculates average of historical MACD differences
check_sign <- function(hist_difference) {
	pp_avg <- 0
	nn_avg <- 0
	for(i in 1:length(hist_difference)) {
		isS <- sign(hist_difference[i])
		if(!is.null(isS)) {
			if(isS) {
				pp_avg <- c(pp_avg, hist_difference[i])
			} else if (!isS) {
				nn_avg <- c(nn_avg, hist_difference[i])
			}
		}
	}
	if(length(nn_avg) > 1) {
		#if(length(nn_avg) > 2) {
		#	nn_avg <- (mean(nn_avg[-1]) - sd(nn_avg[-1]))
		#} else {
			nn_avg <- mean(nn_avg[-1])
		#}
	} else {
		nn_avg <- 0
	}
	if(length(pp_avg) > 1) {
	#	if(length(pp_avg) > 2) {
	#		pp_avg <- (mean(pp_avg[-1]) - sd(pp_avg[-1]))
	#	} else {
			pp_avg <- mean(pp_avg[-1])
	#	}
	} else {
		pp_avg <- 0
	}
	return(c(pp_avg,nn_avg))
}

#check if positive, negative, or zero
sign <- function(difference) {
	if(difference > 0) {
		return(TRUE)
	} else {
		if(difference == 0) {
			return(NULL)
		} else {
			return(FALSE)
		}
	}
}

#training
for(i in (ma_DAYS+1):nrow(training_data)) {
	if(counter >= 30) {
		portfolio <- portfolio + 1000
		portfolio2 <- portfolio2 + 1000
		counter <- 0
		count <- count + 1
	}
	counter <- counter + 1
	price <- training_data[i,5] #testing on closing price
	difference <- hist_difference[i]
	p_n_avg <- check_sign(hist_difference[(i - ma_DAYS):(i - 1)])
	p_avg <- p_n_avg[1]
	n_avg <- p_n_avg[2]

	if(i == (ma_DAYS+1)) { #initial investment
		can_buy <- floor(portfolio / price) 
		portfolio <- portfolio - (can_buy * price)
		number_of_buys <- number_of_buys + 1
		shares <- can_buy
		average_price <- price
		
		
		
		can_buy <- floor(portfolio2 / price) 
		portfolio2 <- portfolio2 - (can_buy * price)
		shares2 <- can_buy
		
	}

	isS <- sign(difference)
	if(!is.null(isS)) {
		if(isS) {
			if(difference > p_avg) {
				if(shares > 0) {
					if((mean(average_price) * shares) < (shares * price)) {
						portfolio <- portfolio + (shares * price)
						shares = 0
						number_of_sells <- number_of_sells + 1
						average_price <- c()
					}
				}
			}
		} else if (!isS) {
			if(difference < n_avg) {
				if(portfolio > price) {
					can_buy <- floor(portfolio / price) 
					if(can_buy > 0) { 
						portfolio <- portfolio - (can_buy * price)
						number_of_buys <- number_of_buys + 1
						shares <- shares + can_buy
						average_price <- c(average_price, price) 
					}
				}
				if(portfolio2 > price) {
					can_buy <- floor(portfolio2 / price) 
					if(can_buy > 0) { 
						portfolio2 <- portfolio2 - (can_buy * price)
						shares2 <- shares2 + can_buy
					}
				}
				
			}
		}
	}
	if(i == nrow(training_data)) {
		if(shares > 0) {
				portfolio <- (shares * price)
				shares = 0
				number_of_sells <- number_of_sells + 1
				average_price <- 0
		}
		investment <- (count * 1000) + 1000
		print(paste("Investment: ", investment, sep=""))
		print(paste("Portfolio: ", portfolio, sep=""))
		print(paste("Return: %", round(((portfolio - investment)/investment)*100, digits=2), sep=""))
		print(paste("Bought: ", number_of_buys, sep=""))
		print(paste("Sold: ", number_of_sells, sep=""))
		
		print(paste("Portfolio2: ", portfolio2 + (shares2 * price), sep=""))
		print(paste("Return: %", round((((portfolio2 + (shares2 * price)) - investment)/investment)*100, digits=2), sep=""))
		
	}
}

