annual_range <- reactiveValues(annual_range = c(1:2))

output$annual_color_o <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		colourInput("annual_color", "Bar color", "#2c3e50")
	} else {
		HTML("")
	}
})

output$annual_control <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		years_length <- ticker_df$ticker_df[[1]][[1]]
		years <- c()
		for(i in 1:length(years_length)) {
			years <- c(years, years_length[[i]]$fiscalYear)
		}
		sliderTextInput("annual_control", label="Select years", choices=c(years), selected = c(years[1], years[length(years)]))
	} else {
		HTML("")
	}
})

observeEvent(input$annual_control, {
	years_length <- ticker_df$ticker_df[[1]][[1]]
	years_index <- c()
	year_true <- FALSE
	for(i in 1:length(years_length)) {
		if(years_length[[i]]$fiscalYear == input$annual_control[1]) {
			year_true <- TRUE
		}
		if(year_true) {
			years_index <- c(years_index, i)
		}
		if(years_length[[i]]$fiscalYear == input$annual_control[2]) {
			break
		}
	}
	
	if(length(years_index) == 1) {
		if(years_index == 1) {
			annual_range$annual_range <- c(1:2)
		} else {
			annual_range$annual_range <- c((years_index[1] - 1), years_index[1])
		}
	} else {
		annual_range$annual_range <- years_index
	}
})

output$income_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		income <- ticker_df$ticker_df[[1]][[1]]
		output$income_plot <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(income[[o]]$incomeNet, income[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Income", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Income), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("income_plot")
	} else {
		HTML("<br>")
	}
})

output$income_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		income_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$income_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(income_table_df[[o]]$fiscalYear, income_table_df[[o]]$incomeNet, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Net Income", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("income_table_render")
	} else {
		HTML("No income reported")
	}
})

output$revenue_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		revenue <- ticker_df$ticker_df[[1]][[1]]
		output$revenue_plot <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(revenue[[o]]$revenue, revenue[[o]]$fiscalYear))	
			}
			colnames(p_df) <- c("Revenue", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Revenue), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("revenue_plot")
	} else {
		HTML("<br>")
	}
})

output$revenue_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		revenue_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$revenue_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(revenue_table_df[[o]]$fiscalYear, revenue_table_df[[o]]$revenue, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Total Revenue", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("revenue_table_render")
	} else {
		HTML("No revenue reported")
	}
})

output$operating_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		operating_income <- ticker_df$ticker_df[[1]][[1]]
		output$operating_plot <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(operating_income[[o]]$incomeOperating, operating_income[[o]]$fiscalYear))		
			}
			colnames(p_df) <- c("Operating", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Operating), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_plot")
	} else {
		HTML("<br>")
	}
})

output$operating_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		operating_income_table <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$operating_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(operating_income_table[[o]]$fiscalYear, operating_income_table[[o]]$incomeOperating, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Operating Income", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("operating_table_render")
	} else {
		HTML("No operating income reported")
	}
})

output$operating_cashflow_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		operating <- ticker_df$ticker_df[[1]][[1]]
		output$operating_cashflow_p <- renderPlot({
				p_df <- data.frame()
				for(o in annual_range$annual_range) {
					p_df <- rbind(p_df, data.frame(operating[[o]]$cashFlowOperating, operating[[o]]$fiscalYear))			 
				}
				colnames(p_df) <- c("Operating", "Year")
				p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
				ggplot(p_df) + geom_bar(aes(x=Year, y=Operating), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$operating_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		operating_cashflow_df <- ticker_df$ticker_df[[1]][[1]]	
		range <- annual_range$annual_range
		output$operating_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(operating_cashflow_df[[o]]$fiscalYear, operating_cashflow_df[[o]]$cashFlowOperating, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Cash Flow from Operations", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("operating_cashflow_table_render")
	} else {
		HTML("No cash flow from operations reported")
	}
})

output$investing_cashflow_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		investing <- ticker_df$ticker_df[[1]][[1]]
		output$investing_cashflow_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(investing[[o]]$cashFlowInvesting, investing[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Investing", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Investing), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("investing_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$investing_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		investing_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$investing_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(investing_table_df[[o]]$fiscalYear, investing_table_df[[o]]$cashFlowInvesting, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Cash Flow from Investing", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("investing_cashflow_table_render")
	} else {
		HTML("No cash flow from investing reported")
	}
})

output$financing_cashflow_plot <- renderUI ({
	financing <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$financing_cashflow_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(financing[[o]]$cashFlowFinancing, financing[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Financing", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Financing), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("financing_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$financing_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		financing_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$financing_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(financing_table_df[[o]]$fiscalYear, financing_table_df[[o]]$cashFlowFinancing, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Cash Flow from Financing", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("financing_cashflow_table_render")
	} else {
		HTML("No cash flow from financing reported")
	}
})

output$assets_balance_plot <- renderUI ({
	assets <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$assets_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(assets[[o]]$assetsUnadjusted, assets[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Assets", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Assets), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("assets_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$assets_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		assets_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$assets_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(assets_balance_table_df[[o]]$fiscalYear, assets_balance_table_df[[o]]$assetsUnadjusted, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Total Assets", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("assets_balance_table_render")
	} else {
		HTML("Total assets not reported")
	}
})

output$liabilities_balance_plot <- renderUI ({
	liabilities <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$liabilities_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(liabilities[[o]]$liabilities, liabilities[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Liabilities", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Liabilities), stat="identity", fill = input$annual_color) + labs(x="",y="") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("liabilities_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$liabilities_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		liabilities_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$liabilities_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(liabilities_balance_table_df[[o]]$fiscalYear, liabilities_balance_table_df[[o]]$liabilities, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Total Liabilities", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("liabilities_balance_table_render")
	} else {
		HTML("Total liabilities not reported")
	}
})

output$debt_to_asset_balance_plot <- renderUI ({
	debt_to_asset <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$debt_to_asset_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(round((debt_to_asset[[o]]$liabilities / debt_to_asset[[o]]$assetsUnadjusted),digits=2) * 100, debt_to_asset[[o]]$fiscalYear))			 
			} 
			colnames(p_df) <- c("debttoasset", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=debttoasset), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("debt_to_asset_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$debt_to_asset_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		debt_to_asset_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$debt_to_asset_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(debt_to_asset_balance_table_df[[o]]$fiscalYear, round((debt_to_asset_balance_table_df[[o]]$liabilities / debt_to_asset_balance_table_df[[o]]$assetsUnadjusted),digits=2) * 100, yoy = NA, yoyv = NA))	 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			for(o in 1:nrow(p_df)) {
				p_df[o,2] <- paste(p_df[o,2], "%", sep="")
			}
			colnames(p_df) <- c("Fiscal Year", "Debt to Asset Ratio", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("debt_to_asset_balance_table_render")
	} else {
		HTML("Debt to asset ratio could not be calculated")
	}
})

output$eps_plot <- renderUI ({
	eps_df <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$eps_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(round((eps_df[[o]]$incomeNet - eps_df[[o]]$dividendsPreferred) / eps_df[[o]]$sharesOutstandingPeDateBs,digits=2) , eps_df[[o]]$fiscalYear))			 
			} 
			colnames(p_df) <- c("EPS", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=EPS), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("eps_plot_p")
	} else {
		HTML("<br>")
	}
})

output$eps_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		eps_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$eps_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(eps_df[[o]]$fiscalYear, round((eps_df[[o]]$incomeNet - eps_df[[o]]$dividendsPreferred) / eps_df[[o]]$sharesOutstandingPeDateBs,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Earings per Share", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("eps_table_render")
	} else {
		HTML("Earnings per share could not be calculated")
	}
})

output$roe_plot <- renderUI ({
	roe_df <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$roe_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(round(roe_df[[o]]$incomeNet / roe_df[[o]]$equityShareholder,digits=2) , roe_df[[o]]$fiscalYear))			 
			} 
			colnames(p_df) <- c("ROE", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=ROE), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("roe_plot_p")
	} else {
		HTML("<br>")
	}
})

output$roe_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		roe_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$roe_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(roe_df[[o]]$fiscalYear, round(roe_df[[o]]$incomeNet / roe_df[[o]]$equityShareholder,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Return on Equity", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("roe_table_render")
	} else {
		HTML("Return on equity could not be calculated")
	}
})

output$roa_plot <- renderUI ({
	roa_df <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$roa_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in annual_range$annual_range) {
				p_df <- rbind(p_df, data.frame(round(roa_df[[o]]$incomeNet / roa_df[[o]]$assetsUnadjusted,digits=2) , roa_df[[o]]$fiscalYear))			 
			} 
			colnames(p_df) <- c("ROA", "Year")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=ROA), stat="identity", fill = input$annual_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("roa_plot_p")
	} else {
		HTML("<br>")
	}
})

output$roa_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		roa_df <- ticker_df$ticker_df[[1]][[1]]
		range <- annual_range$annual_range
		output$roa_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(roa_df[[o]]$fiscalYear, round(roa_df[[o]]$incomeNet / roa_df[[o]]$assetsUnadjusted,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Year", "Return on Assets", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("roa_table_render")
	} else {
		HTML("Return on assets could not be calculated")
	}
})
