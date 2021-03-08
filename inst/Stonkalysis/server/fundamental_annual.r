output$netincome <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Net Income</font></h3></strong>")
})

output$income_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		income <- ticker_df$ticker_df[[1]][[1]]
		output$income_plot <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(income)) {
				p_df <- rbind(p_df, data.frame(income[[o]]$incomeNet, income[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Income", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Income), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("income_plot")
	} else {
		HTML("<br>")
	}
})

output$income_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		income_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$income_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(income_table_df)) {
				p_df <- rbind(p_df, data.frame(income_table_df[[o]]$fiscalYear, income_table_df[[o]]$incomeNet, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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

output$revenue <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Total Revenue</font></h3></strong>")
})

output$revenue_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		revenue <- ticker_df$ticker_df[[1]][[1]]
		output$revenue_plot <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(revenue)) {
				p_df <- rbind(p_df, data.frame(revenue[[o]]$revenue, revenue[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Revenue", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Revenue), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("revenue_plot")
	} else {
		HTML("<br>")
	}
})

output$revenue_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		revenue_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$revenue_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(revenue_table_df)) {
				p_df <- rbind(p_df, data.frame(revenue_table_df[[o]]$fiscalYear, revenue_table_df[[o]]$revenue, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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


output$operating <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Operating Income</font></h3></strong>")
})

output$operating_plots <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		operating_income <- ticker_df$ticker_df[[1]][[1]]
		output$operating_plot <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(operating_income)) {
				p_df <- rbind(p_df, data.frame(operating_income[[o]]$incomeOperating, operating_income[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Operating", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Operating), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_plot")
	} else {
		HTML("<br>")
	}
})

output$operating_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		operating_income_table <- ticker_df$ticker_df[[1]][[1]]
		output$operating_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(operating_income_table)) {
				p_df <- rbind(p_df, data.frame(operating_income_table[[o]]$fiscalYear, operating_income_table[[o]]$incomeOperating, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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

output$operating_cashflow <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Cash Flow from Operations</font></h3></strong>")
})

output$operating_cashflow_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		operating <- ticker_df$ticker_df[[1]][[1]]
		output$operating_cashflow_p <- renderPlot({
				p_df <- data.frame()
				for(o in 1:length(operating)) {
					p_df <- rbind(p_df, data.frame(operating[[o]]$cashFlowOperating, operating[[o]]$fiscalYear))			 
				}
				colnames(p_df) <- c("Operating", "Year")
				p_df[,2] <- as.character(p_df[,2])
				ggplot(p_df) + geom_bar(aes(x=Year, y=Operating), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("operating_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$operating_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		operating_cashflow_df <- ticker_df$ticker_df[[1]][[1]]		
		output$operating_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(operating_cashflow_df)) {
				p_df <- rbind(p_df, data.frame(operating_cashflow_df[[o]]$fiscalYear, operating_cashflow_df[[o]]$cashFlowOperating, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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

output$investing_cashflow <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Cash Flow from Investing</font></h3></strong>")
})

output$investing_cashflow_plot <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		investing <- ticker_df$ticker_df[[1]][[1]]
		output$investing_cashflow_p <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(investing)) {
				p_df <- rbind(p_df, data.frame(investing[[o]]$cashFlowInvesting, investing[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Investing", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Investing), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("investing_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$investing_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		investing_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$investing_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(investing_table_df)) {
				p_df <- rbind(p_df, data.frame(investing_table_df[[o]]$fiscalYear, investing_table_df[[o]]$cashFlowInvesting, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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

output$financing_cashflow <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Cash Flow from Financing</font></h3></strong>")
})

output$financing_cashflow_plot <- renderUI ({
	financing <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$financing_cashflow_p <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(financing)) {
				p_df <- rbind(p_df, data.frame(financing[[o]]$cashFlowFinancing, financing[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Financing", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Financing), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("financing_cashflow_p")
	} else {
		HTML("<br>")
	}
})

output$financing_cashflow_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		financing_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$financing_cashflow_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(financing_table_df)) {
				p_df <- rbind(p_df, data.frame(financing_table_df[[o]]$fiscalYear, financing_table_df[[o]]$cashFlowFinancing, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 			 
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

output$assets_balance <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Total Assets</font></h3></strong>")
})

output$assets_balance_plot <- renderUI ({
	assets <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$assets_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(assets)) {
				p_df <- rbind(p_df, data.frame(assets[[o]]$assetsUnadjusted, assets[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Assets", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Assets), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("assets_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$assets_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		assets_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$assets_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(assets_balance_table_df)) {
				p_df <- rbind(p_df, data.frame(assets_balance_table_df[[o]]$fiscalYear, assets_balance_table_df[[o]]$assetsUnadjusted, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 	
				
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

output$liabilities_balance <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Total Liabilities</font></h3></strong>")
})

output$liabilities_balance_plot <- renderUI ({
	liabilities <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$liabilities_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(liabilities)) {
				p_df <- rbind(p_df, data.frame(liabilities[[o]]$liabilities, liabilities[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("Liabilities", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_bar(aes(x=Year, y=Liabilities), stat="identity", fill = "dodgerblue") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("liabilities_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$liabilities_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		liabilities_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$liabilities_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(liabilities_balance_table_df)) {
				p_df <- rbind(p_df, data.frame(liabilities_balance_table_df[[o]]$fiscalYear, liabilities_balance_table_df[[o]]$liabilities, yoy = NA, yoyv = NA))
				if (o > 1) {
					p_df[o,4] = paste(round(((p_df[o,2] - p_df[(o-1),2]) / p_df[(o-1),2]) * 100, digits = 2), "%", sep="")
					p_df[o,3] = p_df[o,2] - p_df[(o-1),2]
				} 		 
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

output$debt_to_asset_balance <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Debt to Asset Ratio</font></h3></strong>")
})

output$debt_to_asset_balance_plot <- renderUI ({
	debt_to_asset <- ticker_df$ticker_df[[1]][[1]]
	if(length(ticker_df$ticker_df[[1]][[1]]) > 1) {
		output$debt_to_asset_balance_plot_p <- renderPlot({
			p_df <- data.frame()
			for(o in 1:length(debt_to_asset)) {
				p_df <- rbind(p_df, data.frame(round((debt_to_asset[[o]]$liabilities / debt_to_asset[[o]]$assetsUnadjusted),digits=2) * 100, debt_to_asset[[o]]$fiscalYear))			 
			}
			colnames(p_df) <- c("debttoasset", "Year")
			p_df[,2] <- as.character(p_df[,2])
			ggplot(p_df) + geom_line(aes(x=Year, y=debttoasset), size = 1.5, group = 1, color="dodgerblue") + geom_point(aes(x=Year, y=debttoasset), size = 3, color="black") + labs(x="Fiscal Year",y="")  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.background = element_rect(fill = "#f5f5f5"), panel.background = element_rect(fill = "#f5f5f5"), legend.position = "none")
		}) 
		plotOutput("debt_to_asset_balance_plot_p")
	} else {
		HTML("<br>")
	}
})

output$debt_to_asset_balance_table <- renderUI ({
	if(length(ticker_df$ticker_df[[1]][[1]]) >= 1) {
		debt_to_asset_balance_table_df <- ticker_df$ticker_df[[1]][[1]]
		output$debt_to_asset_balance_table_render <- renderTable(colnames=TRUE, rownames=FALSE, width="100%", striped = TRUE,{
			p_df <- data.frame()
			for(o in 1:length(debt_to_asset_balance_table_df)) {
				p_df <- rbind(p_df, data.frame(debt_to_asset_balance_table_df[[o]]$fiscalYear, paste(round((debt_to_asset_balance_table_df[[o]]$liabilities / debt_to_asset_balance_table_df[[o]]$assetsUnadjusted),digits=2) * 100,"%",sep="")))	 
			}
			colnames(p_df) <- c("Fiscal Year", "Debt to Asset Ratio")
			p_df[,1] <- as.character(p_df[,1])
			p_df[,2] <- as.character(p_df[,2])
			return(p_df)
		}) 
		tableOutput("debt_to_asset_balance_table_render")
	} else {
		HTML("Debt to asset ratio could not be calculated")
	}
})
