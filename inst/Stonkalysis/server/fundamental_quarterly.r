quarterly_range <- reactiveValues(quarterly_range = c(1:2))

output$quarterly_color_o <- renderUI ({
	if(length(ticker_df$ticker_df) > 0) {
		if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
			colourInput("quarterly_color", "Bar color", "#2c3e50")
		} else {
			HTML("")
		}
	}
})

output$quarterly_control <- renderUI ({
	if(length(ticker_df$ticker_df) > 0) {
		if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
			quarters_length <- ticker_df$ticker_df[[1]][[2]]
			quarters <- c()
			for(i in 1:length(quarters_length)) {
				quarters <- c(quarters, paste(quarters_length[[i]]$fiscalYear, quarters_length[[i]]$fiscalQuarter, sep=":"))
			}
			sliderTextInput("quarterly_control", label="Select quarters", choices=c(quarters), selected = c(quarters[1], quarters[length(quarters)]))
		} else {
			HTML("")
		}
	}
})

observeEvent(input$quarterly_control, {
	quarters_length <- ticker_df$ticker_df[[1]][[2]]
	quarters_index <- c()
	quarter_true <- FALSE
	for(i in 1:length(quarters_length)) {
		if(paste(quarters_length[[i]]$fiscalYear, quarters_length[[i]]$fiscalQuarter, sep=":") == input$quarterly_control[1]) {
			quarter_true <- TRUE
		}
		if(quarter_true) {
			quarters_index <- c(quarters_index, i)
		}
		if(paste(quarters_length[[i]]$fiscalYear, quarters_length[[i]]$fiscalQuarter, sep=":") == input$quarterly_control[2]) {
			break
		}
	}
	
	if(length(quarters_index) == 1) {
		if(quarters_index == 1) {
			quarterly_range$quarterly_range <- c(1:2)
		} else {
			quarterly_range$quarterly_range <- c((quarters_index[1] - 1), quarters_index[1])
		}
	} else {
		quarterly_range$quarterly_range <- quarters_index
	}
})

output$income_plots_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		income <- ticker_df$ticker_df[[1]][[2]]
		output$income_plot_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(income[[o]]$incomeNet, paste(income[[o]]$fiscalYear, income[[o]]$fiscalQuarter, sep=":")))			 
			}
			colnames(p_df) <- c("Income", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Income), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("income_plot_quarterly")
	} else {
		HTML("<br>")
	}
})

output$income_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		income_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$income_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(income_table_df[[o]]$fiscalYear, income_table_df[[o]]$fiscalQuarter, sep=":"), income_table_df[[o]]$incomeNet, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Net Income", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("income_table_render_quarterly")
	} else {
		HTML("No income reported")
	}
})

output$revenue_plots_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		revenue <- ticker_df$ticker_df[[1]][[2]]
		output$revenue_plot_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(revenue[[o]]$revenue, paste(revenue[[o]]$fiscalYear, revenue[[o]]$fiscalQuarter, sep=":")))	
			}
			colnames(p_df) <- c("Revenue", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Revenue), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("revenue_plot_quarterly")
	} else {
		HTML("<br>")
	}
})

output$revenue_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		revenue_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$revenue_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(revenue_table_df[[o]]$fiscalYear, revenue_table_df[[o]]$fiscalQuarter, sep=":"), revenue_table_df[[o]]$revenue, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Total Revenue", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("revenue_table_render_quarterly")
	} else {
		HTML("No revenue reported")
	}
})

output$operating_plots_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		operating_income <- ticker_df$ticker_df[[1]][[2]]
		output$operating_plot_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(operating_income[[o]]$incomeOperating, paste(operating_income[[o]]$fiscalYear, operating_income[[o]]$fiscalQuarter, sep=":")))		
			}
			colnames(p_df) <- c("Operating", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Operating), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_plot_quarterly")
	} else {
		HTML("<br>")
	}
})

output$operating_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		operating_income_table <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$operating_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(operating_income_table[[o]]$fiscalYear, operating_income_table[[o]]$fiscalQuarter, sep=":"), operating_income_table[[o]]$incomeOperating, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Operating Income", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("operating_table_render_quarterly")
	} else {
		HTML("No operating income reported")
	}
})

output$operating_cashflow_plot_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		operating <- ticker_df$ticker_df[[1]][[2]]
		output$operating_cashflow_p_quarterly <- renderPlot({
				p_df <- data.frame()
				for(o in quarterly_range$quarterly_range) {
					p_df <- rbind(p_df, data.frame(operating[[o]]$cashFlowOperating, paste(operating[[o]]$fiscalYear, operating[[o]]$fiscalQuarter, sep=":")))			 
				}
				colnames(p_df) <- c("Operating", "Quarter")
				p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
				ggplot(p_df) + geom_bar(aes(x=Quarter, y=Operating), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_cashflow_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$operating_cashflow_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		operating_cashflow_df <- ticker_df$ticker_df[[1]][[2]]	
		range <- quarterly_range$quarterly_range
		output$operating_cashflow_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(operating_cashflow_df[[o]]$fiscalYear, operating_cashflow_df[[o]]$fiscalQuarter, sep=":"), operating_cashflow_df[[o]]$cashFlowOperating, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Cash Flow from Operations", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("operating_cashflow_table_render_quarterly")
	} else {
		HTML("No cash flow from operations reported")
	}
})

output$investing_cashflow_plot_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		investing <- ticker_df$ticker_df[[1]][[2]]
		output$investing_cashflow_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(investing[[o]]$cashFlowInvesting, paste(investing[[o]]$fiscalYear, investing[[o]]$fiscalQuarter, sep=":")))			 
			}
			colnames(p_df) <- c("Investing", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Investing), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("investing_cashflow_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$investing_cashflow_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		investing_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$investing_cashflow_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(investing_table_df[[o]]$fiscalYear, investing_table_df[[o]]$fiscalQuarter, sep=":"), investing_table_df[[o]]$cashFlowInvesting, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Cash Flow from Investing", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("investing_cashflow_table_render_quarterly")
	} else {
		HTML("No cash flow from investing reported")
	}
})

output$financing_cashflow_plot_quarterly <- renderUI ({
	financing <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$financing_cashflow_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(financing[[o]]$cashFlowFinancing, paste(financing[[o]]$fiscalYear, financing[[o]]$fiscalQuarter, sep=":")))			 
			}
			colnames(p_df) <- c("Financing", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Financing), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("financing_cashflow_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$financing_cashflow_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		financing_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$financing_cashflow_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(financing_table_df[[o]]$fiscalYear, financing_table_df[[o]]$fiscalQuarter, sep=":"), financing_table_df[[o]]$cashFlowFinancing, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 		
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Cash Flow from Financing", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("financing_cashflow_table_render_quarterly")
	} else {
		HTML("No cash flow from financing reported")
	}
})

output$assets_balance_plot_quarterly <- renderUI ({
	assets <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$assets_balance_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(assets[[o]]$assetsUnadjusted, paste(assets[[o]]$fiscalYear, assets[[o]]$fiscalQuarter, sep=":")))			 
			}
			colnames(p_df) <- c("Assets", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Assets), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("assets_balance_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$assets_balance_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		assets_balance_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$assets_balance_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(assets_balance_table_df[[o]]$fiscalYear, assets_balance_table_df[[o]]$fiscalQuarter, sep=":"), assets_balance_table_df[[o]]$assetsUnadjusted, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Total Assets", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("assets_balance_table_render_quarterly")
	} else {
		HTML("Total assets not reported")
	}
})

output$liabilities_balance_plot_quarterly <- renderUI ({
	liabilities <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$liabilities_balance_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(liabilities[[o]]$liabilities, paste(liabilities[[o]]$fiscalYear, liabilities[[o]]$fiscalQuarter, sep=":")))			 
			}
			colnames(p_df) <- c("Liabilities", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=Liabilities), stat="identity", fill = input$quarterly_color) + labs(x="",y="") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("liabilities_balance_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$liabilities_balance_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		liabilities_balance_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$liabilities_balance_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(liabilities_balance_table_df[[o]]$fiscalYear, liabilities_balance_table_df[[o]]$fiscalQuarter, sep=":"), liabilities_balance_table_df[[o]]$liabilities, yoy = NA, yoyv = NA))
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Total Liabilities", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("liabilities_balance_table_render_quarterly")
	} else {
		HTML("Total liabilities not reported")
	}
})

output$debt_to_asset_balance_plot_quarterly <- renderUI ({
	debt_to_asset <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$debt_to_asset_balance_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(round((debt_to_asset[[o]]$liabilities / debt_to_asset[[o]]$assetsUnadjusted),digits=2) * 100, paste(debt_to_asset[[o]]$fiscalYear, debt_to_asset[[o]]$fiscalQuarter, sep=":")))			 
			} 
			colnames(p_df) <- c("debttoasset", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=debttoasset), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("debt_to_asset_balance_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$debt_to_asset_balance_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		debt_to_asset_balance_table_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$debt_to_asset_balance_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(debt_to_asset_balance_table_df[[o]]$fiscalYear, debt_to_asset_balance_table_df[[o]]$fiscalQuarter, sep=":"), round((debt_to_asset_balance_table_df[[o]]$liabilities / debt_to_asset_balance_table_df[[o]]$assetsUnadjusted),digits=2) * 100, yoy = NA, yoyv = NA))	 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			for(o in 1:nrow(p_df)) {
				p_df[o,2] <- paste(p_df[o,2], "%", sep="")
			}
			colnames(p_df) <- c("Fiscal Quarter", "Debt to Asset Ratio", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
		}) 
		tableOutput("debt_to_asset_balance_table_render_quarterly")
	} else {
		HTML("Debt to asset ratio could not be calculated")
	}
})

output$eps_plot_quarterly <- renderUI ({
	eps_df <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$eps_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(round((eps_df[[o]]$incomeNet - eps_df[[o]]$dividendsPreferred) / eps_df[[o]]$sharesOutstandingPeDateBs,digits=2) , paste(eps_df[[o]]$fiscalYear, eps_df[[o]]$fiscalQuarter, sep=":")))			 
			} 
			colnames(p_df) <- c("EPS", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=EPS), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("eps_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$eps_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		eps_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$eps_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(eps_df[[o]]$fiscalYear, eps_df[[o]]$fiscalQuarter, sep=":"), round((eps_df[[o]]$incomeNet - eps_df[[o]]$dividendsPreferred) / eps_df[[o]]$sharesOutstandingPeDateBs,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Earings per Share", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("eps_table_render_quarterly")
	} else {
		HTML("Earnings per share could not be calculated")
	}
})

output$roe_plot_quarterly <- renderUI ({
	roe_df <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$roe_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(round(roe_df[[o]]$incomeNet / roe_df[[o]]$equityShareholder,digits=2) , paste(roe_df[[o]]$fiscalYear, roe_df[[o]]$fiscalQuarter, sep=":")))			 
			} 
			colnames(p_df) <- c("ROE", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=ROE), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("roe_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$roe_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		roe_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$roe_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(roe_df[[o]]$fiscalYear, roe_df[[o]]$fiscalQuarter, sep=":"), round(roe_df[[o]]$incomeNet / roe_df[[o]]$equityShareholder,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Return on Equity", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("roe_table_render_quarterly")
	} else {
		HTML("Return on equity could not be calculated")
	}
})

output$roa_plot_quarterly <- renderUI ({
	roa_df <- ticker_df$ticker_df[[1]][[2]]
	if(length(ticker_df$ticker_df[[1]][[2]]) > 1) {
		output$roa_plot_p_quarterly <- renderPlot({
			p_df <- data.frame()
			for(o in quarterly_range$quarterly_range) {
				p_df <- rbind(p_df, data.frame(round(roa_df[[o]]$incomeNet / roa_df[[o]]$assetsUnadjusted,digits=2) , paste(roa_df[[o]]$fiscalYear, roa_df[[o]]$fiscalQuarter, sep=":")))			 
			} 
			colnames(p_df) <- c("ROA", "Quarter")
			p_df[,2] <- factor(p_df[,2], levels = p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Quarter, y=ROA), stat="identity", fill = input$quarterly_color) + labs(x="",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14), axis.text.y = element_text(size = 14), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("roa_plot_p_quarterly")
	} else {
		HTML("<br>")
	}
})

output$roa_table_quarterly <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[2]]) >= 1) {
		roa_df <- ticker_df$ticker_df[[1]][[2]]
		range <- quarterly_range$quarterly_range
		output$roa_table_render_quarterly <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			oi <- 1
			for(o in range) {
				p_df <- rbind(p_df, data.frame(paste(roa_df[[o]]$fiscalYear, roa_df[[o]]$fiscalQuarter, sep=":"), round(roa_df[[o]]$incomeNet / roa_df[[o]]$assetsUnadjusted,digits=2), yoy = NA, yoyv = NA))			 
				if (oi > 1) {
					p_df[oi,4] = paste(round(((p_df[oi,2] - p_df[(oi-1),2]) / p_df[(oi-1),2]) * 100, digits = 2), "%", sep="")
					p_df[oi,3] = p_df[oi,2] - p_df[(oi-1),2]
				} 	
				oi <- oi + 1
			}
			colnames(p_df) <- c("Fiscal Quarter", "Return on Assets", "YoY", "YoY %")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			p_df[,3] <- as.character(p_df[,3])
			p_df[,4] <- as.character(p_df[,4])
			return(p_df)
			return(p_df)
		}) 
		tableOutput("roa_table_render_quarterly")
	} else {
		HTML("Return on assets could not be calculated")
	}
})
