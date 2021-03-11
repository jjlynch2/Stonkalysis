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
	DT::datatable(chart_data_table$chart_data_table, extensions = 'Buttons', options = list(dom = "Blfrtip", buttons = list("copy", list(extend = "collection", buttons = c("csv","excel","pdf"), text = "Download")), lengthMenu = c(5,10,15,20,25,50,100,nrow(chart_data_table$chart_data_table)), pageLength = 15, scrollX=TRUE), rowname = FALSE)
})
