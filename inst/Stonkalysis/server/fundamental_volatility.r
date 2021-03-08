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
