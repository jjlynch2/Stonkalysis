ttm_data_table <- reactiveValues(ttm_data_table = data.frame(TTM = "no TTM financial data reported"))

update_table_ttm <- function() {
	if(length(ticker_df$ticker_df[[1]][[3]]) > 0) {
		ttm_data_table$ttm_data_table <- t(as.data.frame(ticker_df$ticker_df[[1]][[3]][[1]]))
		colnames(ttm_data_table$ttm_data_table) <- "TTM"
	} else {
		ttm_data_table$ttm_data_table <- data.frame(TTM = "no TTM financial data reported")
	}
}


output$financial_table_ttm <- DT::renderDataTable ({
	DT::datatable(ttm_data_table$ttm_data_table, extensions = 'Buttons', options = list(dom = "Blfrtip", buttons = list("copy", list(extend = "collection", buttons = c("csv","excel","pdf"), text = "Download")), lengthMenu = c(5,10,15,20,25,50,100,nrow(ttm_data_table$ttm_data_table)), pageLength = 15), rowname = TRUE)
})

annual_data_table <- reactiveValues(annual_data_table = data.frame(annual = "no annual financial data reported"))

update_table_annual <- function() {
	if(length(ticker_df$ticker_df[[1]][[1]]) > 0) {
		annual_df <- data.frame(rep(0,length(ticker_df$ticker_df[[1]][[1]][[1]])))
		cn <- c()
		for(o in 1:length(ticker_df$ticker_df[[1]][[1]])) {
			cn <- c(cn, ticker_df$ticker_df[[1]][[1]][[o]]$fiscalYear)
			annual_df <- cbind(annual_df, t(data.frame(ticker_df$ticker_df[[1]][[1]][[o]])))			 
		}
		annual_df <- annual_df[,-1]
		colnames(annual_df) <- cn
		annual_data_table$annual_data_table <- annual_df
	} else {
		annual_data_table$annual_data_table <- data.frame(annual = "no annual financial data reported")
	}
}

output$financial_table_annual <- DT::renderDataTable ({
	DT::datatable(annual_data_table$annual_data_table, extensions = 'Buttons', options = list(dom = "Blfrtip", buttons = list("copy", list(extend = "collection", buttons = c("csv","excel","pdf"), text = "Download")), lengthMenu = c(5,10,15,20,25,50,100,nrow(annual_data_table$annual_data_table)), pageLength = 15, scrollX=TRUE), rowname = TRUE)
})

quarterly_data_table <- reactiveValues(quarterly_data_table = data.frame(quarterly = "no quarterly financial data reported"))

update_table_quarterly <- function() {
	if(length(ticker_df$ticker_df[[1]][[2]]) > 0) {
		quarterly_df <- data.frame(rep(0,length(ticker_df$ticker_df[[1]][[2]][[1]])))
		cn <- c()
		for(o in 1:length(ticker_df$ticker_df[[1]][[2]])) {
			cn <- c(cn, paste(ticker_df$ticker_df[[1]][[2]][[o]]$fiscalYear, " Quarter: ", ticker_df$ticker_df[[1]][[2]][[o]]$fiscalQuarter, sep=""))
			quarterly_df <- cbind(quarterly_df, t(data.frame(ticker_df$ticker_df[[1]][[2]][[o]])))			 
		}
		quarterly_df <- quarterly_df[,-1]
		colnames(quarterly_df) <- cn
		quarterly_data_table$quarterly_data_table <- quarterly_df
	} else {
		quarterly_data_table$quarterly_data_table <- data.frame(quarterly = "no quarterly financial data reported")
	}
}


output$financial_table_quarterly <- DT::renderDataTable ({
	DT::datatable(quarterly_data_table$quarterly_data_table, extensions = 'Buttons', options = list(dom = "Blfrtip", buttons = list("copy", list(extend = "collection", buttons = c("csv","excel","pdf"), text = "Download")), lengthMenu = c(5,10,15,20,25,50,100,nrow(quarterly_data_table$quarterly_data_table)), pageLength = 15, scrollX=TRUE), rowname = TRUE)
})
