output$am_1 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_1_data <- t(data.frame(am$marketcap, am$sharesOutstanding, am$week52high, am$week52low, am$week52change, am$day5ChangePercent, am$day30ChangePercent, am$avg10Volume, am$avg30Volume, am$day50MovingAvg, am$day200MovingAvg))
	rownames(am_1_data) <- c("Market Cap: ", "Shares Outstanding: ", "52 Week High: ", "52 Week Low: ", "52 Week Change: ", "5 Day Change: ", "30 Day Change: ", "10 Day Average Volume: ", "30 Day Average Volume: ", "50 Day Moving Average: ", "200 Day Moving Average: ") 
	return(am_1_data)
})

output$am_2 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_2_data <- t(data.frame(am$totalCash, am$currentDebt, am$revenue, am$totalRevenue, am$revenuePerShare, am$revenuePerEmployee, am$debtToEquity, am$grossProfit, am$profitMargin, am$enterpriseValue, am$enterpriseValueToRevenue))
	rownames(am_2_data) <- c("Total Cash: ", "Current Debt: ", "Revenue: ", "Total Revenue: ", "Revenue Per Share: ",  "Revenue Per Employee: ", "Debt to Equity: ", "Gross Profit: ",  "Profit Margin: ", "Enterprise Value: ",  "Enterprise Value to Revenue: ")
	return(am_2_data)
})
				
output$am_3 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_3_data <- t(data.frame( am$EBITDA,am$beta,am$ttmEPS,am$peRatio,am$forwardPERatio, am$peHigh,am$peLow,am$pegRatio,am$priceToSales,am$priceToBook,am$putCallRatio))
	rownames(am_3_data) <- c("EBITDA: ", "Beta: ", "TTM EPS: ", "PE Ratio: ", "Forward PE Ratio: ", "52 Week High PE Ratio: ", "52 Week Low PE Ratio: ", "PEG Ratio: ", "PS Ratio: ", "PB Ratio: ", "Put Call Ratio: ")
	return(am_3_data)
})
				
output$am_4 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_4_data <- t(data.frame( am$ttmDividendRate, am$dividendYield,am$nextDividendDate,am$exDividendDate,am$nextEarningsDate))
	rownames(am_4_data) <- c("TTM Dividend Rate: ", "TTM Dividend Yield: ", "Next Dividend Date: ", "Last Dividend Date: ", "Next Earnings Date: ")
	return(am_4_data)
})

output$profile1 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	company_profile <- ticker_df$ticker_df[[2]]
	for(i in 1:length(company_profile)) {
		if(is.null(company_profile[[i]])) {
			company_profile[[i]] <- "N/A"
		}
	}
	profile_data <- t(data.frame(readRDS(file=paste(cache_path, input$ticker, "/cache_date.Rds",sep="")), company_profile$symbol, company_profile$companyName,company_profile$exchange,company_profile$industry,company_profile$CEO,company_profile$sector,company_profile$employees,company_profile$address,company_profile$state,company_profile$city,company_profile$zip,company_profile$country,company_profile$phone))
	rownames(profile_data) <- c("Data last updated on: ", "Symbol: ", "Company name: ", "Exchange: ", "Industry: ", "CEO: ", "Sector: ", "Employees: ", "Address: ", "State: ", "City: ", "Zip code: ", "Country: ", "Phone #: ")
	return(profile_data)
})


output$profile2 <- renderUI({
	company_profile <- ticker_df$ticker_df[[2]]
	HTML(paste(company_profile$description))
})

output$ownership <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
	company_ownership <- ticker_df$ticker_df[[7]]
	output_build <- data.frame()
	if(length(company_ownership) > 0) {
		for(i in 1:length(company_ownership)) {
			for(o in 1:length(company_ownership[[i]])) {
				if(is.null(company_ownership[[i]][[o]])) {
					company_ownership[[i]][[o]] <- "N/A"
				}
			}
		}
		for(o in 1:length(company_ownership)) {
			output_build <- rbind(output_build, data.frame(company_ownership[[o]]$entityProperName, company_ownership[[o]]$reportDate, company_ownership[[o]]$reportedHolding, company_ownership[[o]]$adjHolding))			 
		}
		colnames(output_build) <- c("Institution: ", "Reported: ", "Holding: ", "Adjusted Holding: ")
	} else {
		output_build <- "No institutional holding reported"
	}
	return(output_build)
})

output$instutitional_ui_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[7]]) > 1) {
		company_ownership <- ticker_df$ticker_df[[7]]
		output$institutional_plot <- renderPlot({
				pie_df <- data.frame()
				for(o in 1:length(company_ownership)) {
					pie_df <- rbind(pie_df, data.frame(Institution = company_ownership[[o]]$entityProperName, Holding = company_ownership[[o]]$reportedHolding, Holdingp = 0))			 
				}
				totalp <- sum(pie_df$Holding)
				for(o in 1:nrow(pie_df)) {
					pie_df[o,3] <- round(100*(pie_df[o,2] / totalp), digits=0)
				}
				ggplot(pie_df, aes(x=Institution, y=Holding, fill=Institution)) + geom_bar(stat="identity") + labs(x="",y="Holding")  + theme(axis.text.y = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x =element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.background = element_rect(fill="#f5f5f5")) + geom_text(aes(label = paste0(Holdingp, "%")), position = position_stack(vjust=0.5))
		})
		plotOutput("institutional_plot")
	} else {
		HTML("<br>")
	}
})

output$fund_ui_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[7]]) > 1) {
		fund_ownership <- ticker_df$ticker_df[[8]]
		output$fund_plot <- renderPlot({
				pie_df <- data.frame()
				for(o in 1:length(fund_ownership)) {
					pie_df <- rbind(pie_df, data.frame(Fund = fund_ownership[[o]]$entityProperName, Holding = fund_ownership[[o]]$reportedHolding, Holdingp = 0))			 
				}
				totalp <- sum(pie_df$Holding)
				for(o in 1:nrow(pie_df)) {
					pie_df[o,3] <- round(100*(pie_df[o,2] / totalp), digits=0)
				}
				ggplot(pie_df, aes(x=Fund, y=Holding, fill=Fund)) + geom_bar(stat="identity") + labs(x="",y="Holding")  + theme(axis.text.y = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x =element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.background = element_rect(fill="#f5f5f5")) + geom_text(aes(label = paste0(Holdingp, "%")), position = position_stack(vjust=0.5))
		})
		plotOutput("fund_plot")
	} else {
		HTML("<br>")
	}
})

output$ownership_ui_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[6]]) > 1) {
		company_ownership <- ticker_df$ticker_df[[6]]
		output$ownership_plot <- renderPlot({
				bar_df <- data.frame(Name = "a", Traded = 0, Group = "Bought")
				for(o in 1:length(company_ownership)) {
					bar_df <- rbind(bar_df, data.frame(Name = company_ownership[[o]]$fullName, Traded = company_ownership[[o]]$totalBought, Group = "Bought"))
					bar_df <- rbind(bar_df, data.frame(Name = company_ownership[[o]]$fullName, Traded = company_ownership[[o]]$totalSold, Group = "Sold"))
				}
				bar_df <- bar_df[-1,]
				ggplot(bar_df, aes(x=Name, y=Traded, fill=Group)) + geom_bar(stat="identity") + coord_flip() + labs(x="",y="") + theme(axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.background = element_rect(fill="#f5f5f5")) +   scale_fill_manual(values=c("#008000", "#FF0000"))
		})
		plotOutput("ownership_plot")
	} else {
		HTML("<br>")
	}
})

output$fund <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
	company_fund <- ticker_df$ticker_df[[8]]
	output_build <- data.frame()
	if(length(company_fund) > 0 ) {
		for(i in 1:length(company_fund)) {
			for(o in 1:length(company_fund[[i]])) {
				if(is.null(company_fund[[i]][[o]])) {
					company_fund[[i]][[o]] <- "N/A"
				}
			}
		}
		for(o in 1:length(company_fund)) {
			output_build <- rbind(output_build, data.frame(company_fund[[o]]$entityProperName, company_fund[[o]]$report_date, company_fund[[o]]$reportedHolding, company_fund[[o]]$adjHolding))			 			 
		}
		colnames(output_build) <- c("Fund: ", "Reported: ", "Holding: ", "Adjusted Holding: ")
	} else {
		output_build <- "No fund holding reported"
	}
	return(output_build)
})

output$insider <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
	company_insider <- ticker_df$ticker_df[[6]]
	output_build <- data.frame()
	if(length(company_insider) > 0 ) {
		for(i in 1:length(company_insider)) {
			for(o in 1:length(company_insider[[i]])) {
				if(is.null(company_insider[[i]][[o]])) {
					company_insider[[i]][[o]] <- "N/A"
				}
			}
		}
		for(o in 1:length(company_insider)) {
			output_build <- rbind(output_build, data.frame(company_insider[[o]]$fullName, company_insider[[o]]$reportedTitle, company_insider[[o]]$totalBought, company_insider[[o]]$totalSold))			 			 
		}
		colnames(output_build) <- c("Name: ", "Title: ", "Bought: ", "Sold: ")
	} else {
		output_build <- "No insider trading reported"
	}
	return(output_build)
})
