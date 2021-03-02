output$profile1 <- renderUI({
	company_profile <- ticker_df$ticker_df[[2]]
	HTML(paste("<strong><h3><font color=\"#000000\">Overview</font></h3>",
				"<font color=\"#00688B\">", "Symbol: ", "</font>", company_profile$symbol,
				"<br/>",
				"<font color=\"#00688B\">", "Company name: ", "</font>", company_profile$companyName,
				"<br/>",
				"<font color=\"#00688B\">", "Exchange: ", "</font>", company_profile$exchange,
				"<br/>",
				"<font color=\"#00688B\">", "Industry: ", "</font>", company_profile$industry,
				"<br/>",
				"<font color=\"#00688B\">", "CEO: ", "</font>", company_profile$CEO,
				"<br/>",
				"<font color=\"#00688B\">", "Sector: ", "</font>", company_profile$sector,
				"<br/>",
				"<font color=\"#00688B\">", "Employees: ", "</font>", company_profile$employees,
				"<br/>",
				"<font color=\"#00688B\">", "Address: ", "</font>", company_profile$address,
				"<br/>",
				"<font color=\"#00688B\">", "State: ", "</font>", company_profile$state,
				"<br/>",
				"<font color=\"#00688B\">", "City: ", "</font>", company_profile$city,
				"<br/>",
				"<font color=\"#00688B\">", "Zip code: ", "</font>", company_profile$zip,
				"<br/>",
				"<font color=\"#00688B\">", "Country: ", "</font>", company_profile$country,
				"<br/>",
				"<font color=\"#00688B\">", "Phone #: ", "</font>", company_profile$phone,
				'</strong>'
	))
})

output$profile2 <- renderUI({
	company_profile <- ticker_df$ticker_df[[2]]
	HTML(paste("<strong><h3><font color=\"#000000\">Description</font></h3>",
				company_profile$description,
				'</strong>'
	))
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
		checkboxGroupInput("plotly_options", label="Indicators", choices = c("volume", "ma5","ma10","ma20","ma60"), selected = c("volume","ma20"), inline = TRUE)
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
	input$chart_dates[1]
	input$chart_dates[2]
	temp1 <- chart_df2$chart_df2[chart_df2$chart_df2[,10] >= input$chart_dates[1],]
	temp1 <- temp1[temp1[,10] <= input$chart_dates[2],]
	chart_df$chart_df <- temp1
})

output$plotly_chart_ui <- renderUI ({
	if(length(ticker_df$ticker_df[[3]]) > 1) {
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
				if(any(input$plotly_options == "ma5")) {
					ma5 <- movavg(as.numeric(df1[,1]),5, type="e")
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
						x = df1[,10],
						y = ma5
					)
					fig <- add_trace(fig, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
				}
				if(any(input$plotly_options == "ma10")) {
					ma10 <- movavg(as.numeric(df1[,1]),10, type="e")
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
						x = df1[,10],
						y = ma10
					)
					fig <- add_trace(fig, line=trace2$line, mode=trace2$mode, name=trace2$name, text=trace2$text, type=trace2$type, x=trace2$x, y=trace2$y)
				}
				if(any(input$plotly_options == "ma20")) {
					ma20 <- movavg(as.numeric(df1[,1]),20, type="e")
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
						x = df1[,10],
						y = ma20
					)
					fig <- add_trace(fig, line=trace3$line, mode=trace3$mode, name=trace3$name, text=trace3$text, type=trace3$type, x=trace3$x, y=trace3$y)
				}
				if(any(input$plotly_options == "ma60")) {
					ma60 <- movavg(as.numeric(df1[,1]),60, type="e")
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
						x = df1[,10],
						y = ma60
					)
					fig <- add_trace(fig, line=trace4$line, mode=trace4$mode, name=trace4$name, text=trace4$text, type=trace4$type, x=trace4$x, y=trace4$y)
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


output$ownership <- renderUI({
	company_ownership <- ticker_df$ticker_df[[7]]
	output_build <- "<strong>"
	if(length(company_ownership) > 0) {
		for(o in 1:length(company_ownership)) {
			output_build <- paste(output_build, "<font color=\"#00688B\">", "Institution: ", "</font>", company_ownership[[o]]$entityProperName,
							"<br/>",
							"<font color=\"#00688B\">", "Reported: ", "</font>", company_ownership[[o]]$reportDate,
							"<br/>",
							"<font color=\"#00688B\">", "Holding: ", "</font>", company_ownership[[o]]$reportedHolding,
							"<br/>",
							"<font color=\"#00688B\">", "Adjusted Holding: ", "</font>", company_ownership[[o]]$adjHolding,
							"<br/>",
							"<br/>"
			)				 
		}
		output_build <- paste(output_build, "</strong>")
	} else {
		output_build <- paste(output_build, "No institutional holding reported</strong>")
	}
	HTML(output_build)
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

output$insider <- renderUI({
	company_insider <- ticker_df$ticker_df[[6]]
	output_build <- "<strong>"
	if(length(company_insider) > 0 ) {
		for(o in 1:length(company_insider)) {
			output_build <- paste(output_build, "<font color=\"#00688B\">", "Name: ", "</font>", company_insider[[o]]$fullName,
							"<br/>",
							"<font color=\"#00688B\">", "Title: ", "</font>", company_insider[[o]]$reportedTitle,
							"<br/>",
							"<font color=\"#00688B\">", "Bought: ", "</font>", company_insider[[o]]$totalBought,
							"<br/>",
							"<font color=\"#00688B\">", "Sold: ", "</font>", company_insider[[o]]$totalSold,
							"<br/>",
							"<br/>"
			)				 
		}
		output_build <- paste(output_build, "</strong>")
	} else {
		output_build <- paste(output_build, "No insider trading reported</strong>")
	}
	HTML(output_build)
})
