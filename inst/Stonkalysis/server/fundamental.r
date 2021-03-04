output$am_1 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <<- ticker_df$ticker_df[[4]]
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
	am <<- ticker_df$ticker_df[[4]]
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
	am <<- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_3_data <- t(data.frame( am$EBITDA,am$beta,am$ttmEPS,am$peRatio,am$forwardPERatio, am$peHigh,am$peLow,am$pegRatio,am$priceToSales,am$priceToBook,am$putCallRatio))
	rownames(am_3_data) <- c("EBITDA: ", "Beta: ", "TTM EPS: ", "PE Ratio: ", "Forward PE Ratio: ", "52 Week High PE Ratio: ", "52 Week High PE Ratio: ", "PEG Ratio: ", "PS Ratio: ", "PB Ratio: ", "Put Call Ratio: ")
	return(am_3_data)
})
				
output$am_4 <- renderTable(colnames=FALSE, rownames=TRUE, width="100%", striped = TRUE,{
	am <<- ticker_df$ticker_df[[4]]
	for(i in 1:length(am)) {
		if(is.null(am[[i]])) {
			am[[i]] <- "N/A"
		}
	}
	am_4_data <- t(data.frame( am$ttmDividendRate, am$dividendYield,am$nextDividendDate,am$exDividendDate,am$nextEarningsDate))
	rownames(am_4_data) <- c("TTM Dividend Rate: ", "TTM Dividend Yield: ", "Next Dividend Date: ", "Last Dividend Date: ", "Next Earnings Date: ")
	return(am_4_data)
})
	
output$profile1_title <- renderUI({
	HTML(paste("<strong><h3><font color=\"#000000\">Overview</font></h3>"))
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
		output$institutional_plot <- renderPlot({
				company_ownership <- ticker_df$ticker_df[[7]]
				pie_df <- data.frame()
				for(o in 1:length(company_ownership)) {
					pie_df <- rbind(pie_df, data.frame(Institution = company_ownership[[o]]$entityProperName, Holding = company_ownership[[o]]$reportedHolding, Holdingp = 0))			 
				}
				totalp <- sum(pie_df$Holding)
				for(o in 1:nrow(pie_df)) {
					pie_df[o,3] <- round(100*(pie_df[o,2] / totalp), digits=0)
				}
				ggplot(pie_df, aes(x=Institution, y=Holding, fill=Institution)) + geom_bar(stat="identity")  + labs(x="",y="Holding")  + theme(axis.text.x = element_blank(), axis.ticks.x =element_blank(), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.background = element_rect(fill="#f5f5f5")) + geom_text(aes(label = paste0(Holdingp, "%")), position = position_stack(vjust=0.5))
		})
		plotOutput("institutional_plot")
	} else {
		HTML("<br>")
	}
})

output$ownership_ui_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[6]]) > 1) {
		output$ownership_plot <- renderPlot({
				company_ownership <- ticker_df$ticker_df[[6]]
				bar_df <- data.frame(Name = "a", Traded = 0, Group = "Bought")
				for(o in 1:length(company_ownership)) {
					bar_df <- rbind(bar_df, data.frame(Name = company_ownership[[o]]$fullName, Traded = company_ownership[[o]]$totalBought, Group = "Bought"))
					bar_df <- rbind(bar_df, data.frame(Name = company_ownership[[o]]$fullName, Traded = company_ownership[[o]]$totalSold, Group = "Sold"))
				}
				bar_df <- bar_df[-1,]
				ggplot(bar_df, aes(x=Name, y=Traded, fill=Group)) + geom_bar(stat="identity") + coord_flip() + labs(x="",y="") + theme(plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.background = element_rect(fill="#f5f5f5")) +   scale_fill_manual(values=c("#008000", "#FF0000"))
		})
		plotOutput("ownership_plot")
	} else {
		HTML("<br>")
	}
})

output$insider_title <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Insider Ownership</font></h3></strong>")
})

output$ownership_title <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Institutional Ownership</font></h3></strong>")
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

###chart plot
chart_df <- reactiveValues(chart_df = data.frame(Data = "no"))
chart_df2 <- reactiveValues(chart_df2 = data.frame(Data = "No historical data to display"))

output$plotly_control_ui <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- data.frame(matrix(unlist(ticker_df$ticker_df[[3]]), nrow=length(ticker_df$ticker_df[[3]]), byrow=TRUE),stringsAsFactors=FALSE)
		colnames(df1) <- names(ticker_df$ticker_df[[3]][[1]])
		chart_df2$chart_df2 <- df1
		dateRangeInput("chart_dates", "Select Date Range", start = df1[1,10], end = df1[nrow(df1),10], min = df1[1,10], max = df1[nrow(df1),10])
	} else {
		HTML("No historical data to chart")
	}
})

output$plotly_control_ui2 <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		checkboxGroupInput("plotly_options", label="Indicators", choices = c("volume", "ma5","ma10","ma20","ma60","Bollinger Bands"), selected = c("volume","ma20"), inline = TRUE)
	} else {
		HTML("")
	}
})

output$plotly_color <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		colourInput("colo1", "Increase color", "forestgreen")
	} else {
		HTML("")
	}
})

output$plotly_color2 <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		colourInput("colo2", "Decrease color", "red")
	} else {
		HTML("")
	}
})

observeEvent(input$chart_dates, {
	temp1 <- chart_df2$chart_df2[chart_df2$chart_df2[,10] >= input$chart_dates[1],]
	temp1 <- temp1[temp1[,10] <= input$chart_dates[2],]
	chart_df$chart_df <- temp1
})

output$plotly_chart_ui <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		temp1 <- chart_df2$chart_df2[chart_df2$chart_df2[,10] >= input$chart_dates[1],]
		temp1 <- temp1[temp1[,10] <= input$chart_dates[2],]
		C <- as.numeric(chart_df2$chart_df2[,1])
		output$plotly_chart <- renderPlotly({
			colors <- c(input$colo2, input$colo1)
			df1 <- chart_df$chart_df
			price <- list(
				name = "price",
				text = "",
				type = "candlestick",
				x = df1[,10],
				low = df1[,3],
				high = df1[,2],
				open = df1[,4],
				close = df1[,1],
				decreasing = list(line = list(color = input$colo2)),
				increasing = list(line = list(color = input$colo1))
			)
			fig <- plot_ly()
			fig <- add_trace(fig, type=price$type, x=price$x, low=price$low, high=price$high, open=price$open, close=price$close, decreasing=price$decreasing, increasing=price$increasing)
			for(o in input$plotly_options) {
				if(o == "ma5") {
					ma5 <- SMA(x = C, n = 5)
					ma5 <- ma5[which(chart_df2$chart_df2[,10] >= input$chart_dates[1])]
					ma5 <- ma5[which(temp1[,10] <= input$chart_dates[2])]
					nal <- length(which(is.na(ma5)))
					nal <- nal + 1
					ma5 <- na.omit(ma5)
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "blue",
							width = 1.3
						),
						mode = "lines",
						name = "MA5",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = ma5
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "ma10") {
					ma10 <- SMA(x = C, n = 10)
					ma10 <- ma10[which(chart_df2$chart_df2[,10] >= input$chart_dates[1])]
					ma10 <- ma10[which(temp1[,10] <= input$chart_dates[2])]
					nal <- length(which(is.na(ma10)))
					nal <- nal + 1
					ma10 <- na.omit(ma10)
					trace2 <- list(
						line = list(
							dash = "solid",
							color = "black",
							width = 1.3
						),
						mode = "lines",
						name = "MA10",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = ma10
					)
					fig <- add_trace(fig, line=trace2$line, mode=trace2$mode, name=trace2$name, text=trace2$text, type=trace2$type, x=trace2$x, y=trace2$y)
				}
				if(o == "ma20") {
					ma20 <- SMA(x = C, n = 20)
					ma20 <- ma20[which(chart_df2$chart_df2[,10] >= input$chart_dates[1])]
					ma20 <- ma20[which(temp1[,10] <= input$chart_dates[2])]
					nal <- length(which(is.na(ma20)))
					nal <- nal + 1
					ma20 <- na.omit(ma20)
					trace3 <- list(
						line = list(
							dash = "solid",
							color = "red",
							width = 1.3
						),
						mode = "lines",
						name = "MA20",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = ma20
					)
					fig <- add_trace(fig, line=trace3$line, mode=trace3$mode, name=trace3$name, text=trace3$text, type=trace3$type, x=trace3$x, y=trace3$y)
				}
				if(o == "ma60") {
					ma60 <- SMA(x = C, n = 60)
					ma60 <- ma60[which(chart_df2$chart_df2[,10] >= input$chart_dates[1])]
					ma60 <- ma60[which(temp1[,10] <= input$chart_dates[2])]
					nal <- length(which(is.na(ma60)))
					nal <- nal + 1
					ma60 <- na.omit(ma60)
					trace4 <- list(
						line = list(
							dash = "solid",
							color = "green",
							width = 1.3
						),
						mode = "lines",
						name = "MA60",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = ma60
					)
					fig <- add_trace(fig, line=trace4$line, mode=trace4$mode, name=trace4$name, text=trace4$text, type=trace4$type, x=trace4$x, y=trace4$y)
				}
				if(o == "Bollinger Bands") {
					HLC <- cbind(as.numeric(chart_df2$chart_df2[,2]), as.numeric(chart_df2$chart_df2[,3]), as.numeric(chart_df2$chart_df2[,1]))
					BB <- BBands(HLC = HLC)
					BB <- BB[which(chart_df2$chart_df2[,10] >= input$chart_dates[1]),]
					BB <- BB[which(temp1[,10] <= input$chart_dates[2]),]
					nal <- length(which(is.na(BB[,1])))
					nal <- nal + 1
					BB <- na.omit(BB)
					lower <- list(
						line = list(
							dash = "dash",
							color = "black",
							width = 1.3
						),
						mode = "lines",
						name = "Bollinger Band",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = BB[,1]
					)
					upper <- list(
						line = list(
							dash = "dash",
							color = "black",
							width = 1.3
						),
						mode = "lines",
						name = "Bollinger Band",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = BB[,3]
					)
					ma <- list(
						line = list(
							dash = "dash",
							color = "red",
							width = 1.3
						),
						mode = "lines",
						name = "Bollinger Band",
						text = "",
						type = "scatter",
						x = df1[nal:nrow(df1),10],
						y = BB[,2]
					)
					fig <- add_trace(fig, line=upper$line, mode=upper$mode, name=upper$name, text=upper$text, type=upper$type, x=upper$x, y=upper$y)
					fig <- add_trace(fig, line=lower$line, mode=lower$mode, name=lower$name, text=lower$text, type=lower$type, x=lower$x, y=lower$y)
					fig <- add_trace(fig, line=ma$line, mode=ma$mode, name=ma$name, text=ma$text, type=ma$type, x=ma$x, y=ma$y)
				}
			}
			fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price"), plot_bgcolor="#f5f5f5", paper_bgcolor="#f5f5f5", xaxis = list(rangeslider = list(visible = F)))
				if(any(input$plotly_options == "volume")) {
				fig2 <- df1
				fig2 <- fig2 %>% plot_ly(x=~date, y=~volume, type='bar', name = "Volume", color = ~close > open, colors = colors) 
				fig2 <- fig2 %>% layout(showlegend = FALSE, yaxis = list(title = "Volume"), plot_bgcolor="#f5f5f5", paper_bgcolor="#f5f5f5") 
				fig <- subplot(fig, fig2, heights = c(0.7, 0.2), nrows=2, shareX = TRUE, titleY = TRUE, titleX=FALSE)
			}
			return(fig)
		})
		plotlyOutput("plotly_chart")
	} else {
		HTML("No historical data to chart")
	}
})
###chart plot

###chart table
chart_data_table <- reactiveValues(chart_data_table = data.frame(Data = "No historical data to display"))

update_table_chart <- function() {
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- data.frame(matrix(unlist(ticker_df$ticker_df[[3]]), nrow=length(ticker_df$ticker_df[[3]]), byrow=TRUE),stringsAsFactors=FALSE)
		colnames(df1) <- names(ticker_df$ticker_df[[3]][[1]])
		chart_data_table$chart_data_table <- df1
		colnames(chart_data_table$chart_data_table) <- colnames(df1)
	} else {
		chart_data_table$chart_data_table <- data.frame(Data = "No historical data to display")
	}
}

observeEvent(input$ticker, {
	update_table_chart()
})

output$chart_table <- DT::renderDataTable ({
	DT::datatable(chart_data_table$chart_data_table, options = list(lengthMenu = c(5,10,15,20,25,50,100,200), pageLength = 15, scrollX=TRUE), rowname = FALSE)
})
###chart table

###chart plot
chart_dfv <- reactiveValues(chart_dfv = data.frame(Data = "no"))
chart_dfv2 <- reactiveValues(chart_dfv2 = data.frame(Data = "No historical data to display"))

output$plotly_control_ui_v <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- data.frame(matrix(unlist(ticker_df$ticker_df[[3]]), nrow=length(ticker_df$ticker_df[[3]]), byrow=TRUE),stringsAsFactors=FALSE)
		colnames(df1) <- names(ticker_df$ticker_df[[3]][[1]])
		chart_dfv2$chart_dfv2 <- df1
		dateRangeInput("chart_datesv", "Select Date Range", start = df1[1,10], end = df1[nrow(df1),10], min = df1[1,10], max = df1[nrow(df1),10])
	} else {
		HTML("No historical data to chart")
	}
})

output$plotly_control_ui2_v <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		checkboxGroupInput("plotly_options_v", label="Volatility Measures", choices = c("OHLC Yang and Zhang", "Close-to-Close","OHLC Garman and Klass","HL Parkinson","OHLC Rogers and Satchell","OHLC Garman and Klass (Yang and Zhang modification)"), selected = c("OHLC Yang and Zhang"), inline = TRUE)
	} else {
		HTML("")
	}
})

observeEvent(input$chart_datesv, {
	temp1 <- chart_dfv2$chart_dfv2[chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1],]
	temp1 <- temp1[temp1[,10] <= input$chart_datesv[2],]
	chart_dfv$chart_dfv <- temp1
})

output$table_title <- renderUI ({
	HTML(paste("</br><strong>Percentage of time the selected historical range was higher than the end date</strong></br>"))
})

output$plotly_chart_ui_v_p <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- chart_dfv$chart_dfv
		temp1 <- chart_dfv2$chart_dfv2[chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1],]
		temp1 <- temp1[temp1[,10] <= input$chart_datesv[2],]
		OHLC <- cbind(as.numeric(chart_dfv2$chart_dfv2[,4]), as.numeric(chart_dfv2$chart_dfv2[,2]), as.numeric(chart_dfv2$chart_dfv2[,3]), as.numeric(chart_dfv2$chart_dfv2[,1]))
		OHLC_V1 <- volatility(OHLC = OHLC, calc = "yang.zhang")
		OHLC_V1 <- OHLC_V1[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V1 <- OHLC_V1[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V2 <- volatility(OHLC = OHLC, calc = "close")
		OHLC_V2 <- OHLC_V2[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V2 <- OHLC_V2[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V3 <- volatility(OHLC = OHLC, calc = "garman.klass")
		OHLC_V3 <- OHLC_V3[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V3 <- OHLC_V3[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V4 <- volatility(OHLC = OHLC, calc = "parkinson")
		OHLC_V4 <- OHLC_V4[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V4 <- OHLC_V4[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V5 <- volatility(OHLC = OHLC, calc = "rogers.satchell")
		OHLC_V5 <- OHLC_V5[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V5 <- OHLC_V5[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V6 <- volatility(OHLC = OHLC, calc = "gk.yz")
		OHLC_V6 <- OHLC_V6[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V6 <- OHLC_V6[which(temp1[,10] <= input$chart_datesv[2])]
		output$table_vol_p <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			HVC <- data.frame(data="Please select a volatility measure above")
			HVC_1 <- 0
			for(o in input$plotly_options_v) {
				if(o == "OHLC Yang and Zhang") {
					total_num <- length(OHLC_V1)
					last <- OHLC_V1[total_num]
					above_num <- length(OHLC_V1[OHLC_V1 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "OHLC Yang and Zhang"
				}
				if(o == "Close-to-Close") {
					total_num <- length(OHLC_V2)
					last <- OHLC_V2[total_num]
					above_num <- length(OHLC_V2[OHLC_V2 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "Close-to-Close"
				}
				if(o == "OHLC Garman and Klass") {
					total_num <- length(OHLC_V3)
					last <- OHLC_V3[total_num]
					above_num <- length(OHLC_V3[OHLC_V3 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "OHLC Garman and Klass"
				}
				if(o == "HL Parkinson") {
					total_num <- length(OHLC_V4)
					last <- OHLC_V4[total_num]
					above_num <- length(OHLC_V4[OHLC_V4 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "HL Parkinson"
				}
				if(o == "OHLC Rogers and Satchell") {
					total_num <- length(OHLC_V5)
					last <- OHLC_V5[total_num]
					above_num <- length(OHLC_V5[OHLC_V5 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "OHLC Rogers and Satchell"
				}
				if(o == "OHLC Garman and Klass (Yang and Zhang modification)") {
					total_num <- length(OHLC_V6)
					last <- OHLC_V6[total_num]
					above_num <- length(OHLC_V6[OHLC_V6 > last])
					percent <- round((100 * (above_num / total_num)),digits=1)
					HVC <- cbind(HVC, data.frame(paste(percent, "%", sep="")))
					colnames(HVC)[ncol(HVC)] <- "OHLC Garman and Klass (Yang and Zhang modification)"
				}
			}
			HVC_l <- length(HVC)
			if(HVC_l == 1) {
				return(HVC)
			}
			if(HVC_l == 2) {
				cn <- colnames(HVC)
				HVC <- HVC[,-1]
				names(HVC) <- cn[2]
			}
			if(HVC_l > 2) {
				HVC <- HVC[,-1]
			}
			if(length(HVC) == 1) {
				return(t(data.frame(HVC)))
			} else {
				return(HVC)
			}
		})

		tableOutput("table_vol_p")
	}
})

output$plotly_chart_ui_v <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- chart_dfv$chart_dfv
		temp1 <- chart_dfv2$chart_dfv2[chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1],]
		temp1 <- temp1[temp1[,10] <= input$chart_datesv[2],]
		OHLC <- cbind(as.numeric(chart_dfv2$chart_dfv2[,4]), as.numeric(chart_dfv2$chart_dfv2[,2]), as.numeric(chart_dfv2$chart_dfv2[,3]), as.numeric(chart_dfv2$chart_dfv2[,1]))
		fig <- plot_ly()
		OHLC_V1 <- volatility(OHLC = OHLC, calc = "yang.zhang")
		OHLC_V1 <- OHLC_V1[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V1 <- OHLC_V1[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V2 <- volatility(OHLC = OHLC, calc = "close")
		OHLC_V2 <- OHLC_V2[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V2 <- OHLC_V2[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V3 <- volatility(OHLC = OHLC, calc = "garman.klass")
		OHLC_V3 <- OHLC_V3[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V3 <- OHLC_V3[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V4 <- volatility(OHLC = OHLC, calc = "parkinson")
		OHLC_V4 <- OHLC_V4[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V4 <- OHLC_V4[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V5 <- volatility(OHLC = OHLC, calc = "rogers.satchell")
		OHLC_V5 <- OHLC_V5[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V5 <- OHLC_V5[which(temp1[,10] <= input$chart_datesv[2])]
		OHLC_V6 <- volatility(OHLC = OHLC, calc = "gk.yz")
		OHLC_V6 <- OHLC_V6[which(chart_dfv2$chart_dfv2[,10] >= input$chart_datesv[1])]
		OHLC_V6 <- OHLC_V6[which(temp1[,10] <= input$chart_datesv[2])]
		
		output$plotly_chartv <- renderPlotly({
			for(o in input$plotly_options_v) {
				if(o == "OHLC Yang and Zhang") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "black",
							width = 1.3
						),
						mode = "lines",
						name = "OHLC Yang and Zhang",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V1
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "Close-to-Close") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "red",
							width = 1.3
						),
						mode = "lines",
						name = "Close-to-Close",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V2
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "OHLC Garman and Klass") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "blue",
							width = 1.3
						),
						mode = "lines",
						name = "OHLC Garman and Klass",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V3
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "HL Parkinson") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "green",
							width = 1.3
						),
						mode = "lines",
						name = "HL Parkinson",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V4
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "OHLC Rogers and Satchell") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "orange",
							width = 1.3
						),
						mode = "lines",
						name = "OHLC Rogers and Satchell",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V5
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(o == "OHLC Garman and Klass (Yang and Zhang modification)") {
					trace1 <- list(
						line = list(
							dash = "solid",
							color = "purple",
							width = 1.3
						),
						mode = "lines",
						name = "OHLC Garman and Klass (Yang and Zhang modification)",
						text = "",
						type = "scatter",
						x = df1[,10],
						y = OHLC_V6
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
			}
			fig <- fig %>% layout(showlegend = FALSE, plot_bgcolor="#f5f5f5", paper_bgcolor="#f5f5f5", xaxis = list(rangeslider = list(visible = F)))
			return(fig)
		})
		plotlyOutput("plotly_chartv")
	} else {
		HTML("No historical data to chart")
	}
})
###chart plot

###chart table
chart_data_table_v <- reactiveValues(chart_data_table_v = data.frame(Data = "No historical data to display"))

update_table_chart_v <- function() {
	if(length(ticker_df$ticker_df[[3]]) > 1) {
		df1 <- data.frame(matrix(unlist(ticker_df$ticker_df[[3]]), nrow=length(ticker_df$ticker_df[[3]]), byrow=TRUE),stringsAsFactors=FALSE)
		colnames(df1) <- names(ticker_df$ticker_df[[3]][[1]])
		OHLC <- cbind(as.numeric(df1[,4]), as.numeric(df1[,2]), as.numeric(df1[,3]), as.numeric(df1[,1]))
		V1 <- volatility(OHLC = OHLC, calc = "yang.zhang")
		V2 <- volatility(OHLC = OHLC, calc = "close")
		V3 <- volatility(OHLC = OHLC, calc = "garman.klass")
		V4 <- volatility(OHLC = OHLC, calc = "parkinson")
		V5 <- volatility(OHLC = OHLC, calc = "rogers.satchell")
		V6 <- volatility(OHLC = OHLC, calc = "gk.yz")
		dfv <- data.frame(yang.zhang = V1, close = V2, german.klass = V3, parkinson = V4, rogers.satchell = V5, gk.yz = V6)
		rownames(dfv) <- df1[,10]
		colnames(dfv) <- c("yang.zhang", "close", "german.klass", "parkinson", "rogers.satchell","gk.yz")
		chart_data_table_v$chart_data_table_v <- dfv
		colnames(chart_data_table_v$chart_data_table_v) <- colnames(dfv)
		rownames(chart_data_table_v$chart_data_table_v) <- rownames(dfv)
	} else {
		chart_data_table_v$chart_data_table_v <- data.frame(Data = "No historical data to display")
	}
}

observeEvent(input$ticker, {
	update_table_chart_v()
})

output$chart_table_v <- DT::renderDataTable ({
	DT::datatable(chart_data_table_v$chart_data_table_v, options = list(lengthMenu = c(5,10,15,20,25,50,100,200), pageLength = 15, scrollX=TRUE), rowname = TRUE)
})
###chart table
