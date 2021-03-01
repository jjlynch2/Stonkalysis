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

output$ownership <- renderUI({
	company_ownership <- ticker_df$ticker_df[[7]]
	output_build <- "<strong>"
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
	HTML(output_build)
})

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





output$insider_title <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Insider Ownership</font></h3></strong>")
})

output$ownership_title <- renderUI({
	HTML("<strong><h3><font color=\"#000000\">Institutional Ownership</font></h3></strong>")
})

output$insider <- renderUI({
	company_insider <- ticker_df$ticker_df[[6]]
	output_build <- "<strong>"
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
	HTML(output_build)
})
