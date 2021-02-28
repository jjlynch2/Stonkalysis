library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
options(warn = -1)
options(scipen = 999)

server <- function(input, output) { 
	cache_date_table <- reactiveValues(cache_date_table = data.frame())
	av_tickers <- reactiveValues(av_tickers = list.files(cache_path, recursive = FALSE, full.names=FALSE))
	ticker_choice <- reactiveValues(ticker_choice = getTickers())

	update_table <- function() {
		showModal(modalDialog(title = "Updating table...", easyClose = FALSE, footer = NULL))
		cache_date <- data.frame()
		for(t in av_tickers$av_tickers) {
			p <- readRDS(file=paste(cache_path, t, "/profile.Rds",sep=""))
			p <- p$companyName
			d <- readRDS(file=paste(cache_path, t, "/cache_date.Rds",sep=""))
			cache_date <- rbind(cache_date, c(t, p, d))
		}
		colnames(cache_date) <- c("Ticker","Company Name", "Cache Date")
		cache_date_table$cache_date_table <- cache_date
		removeModal()
	}

	observeEvent(TRUE, {
		update_table()
	})

	output$cache_table <- DT::renderDataTable ({
		DT::datatable(cache_date_table$cache_date_table, options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rowname = FALSE)
	})
		
	output$tickers <- renderUI({
		temp <- av_tickers$av_tickers
		selectInput(inputId = "ticker", label = "Select Ticker", choices = temp)
	})
	
	
	output$available_tickers <- renderUI({
		av_tickers$av_tickers <- list.files(cache_path, recursive = FALSE, full.names=FALSE)
		for(t in av_tickers$av_tickers) {
			ticker_choice$ticker_choice <- ticker_choice$ticker_choice[ticker_choice$ticker_choice != t]
		}
		selectizeInput(inputId = "add_ticker", label = "Available Tickers", choices = c(ticker_choice$ticker_choice), multiple = TRUE)
	})
	
	output$delete_tickers <- renderUI({
		temp <- av_tickers$av_tickers
		selectizeInput(inputId = "delete_ticker", label = "Cached Tickers", choices = c(temp), multiple = TRUE)
	})
								
	observeEvent(input$delete_ticker_button, {
		showModal(modalDialog(title = "Deleting ticker from cache...", easyClose = FALSE, footer = NULL))
		if(length(input$delete_ticker) > 0) {
			for(t in input$delete_ticker) {
				unlink(paste(cache_path, t,sep=""), recursive=TRUE)
			}
			av_tickers$av_tickers <- list.files(cache_path, recursive = FALSE, full.names=FALSE)
			for(t in av_tickers$av_tickers) {
				ticker_choice$ticker_choice <- ticker_choice$ticker_choice[ticker_choice$ticker_choice != t]
			}
			update_table()
		}
		removeModal()
	})
	
	observeEvent(input$add_tickers, {
		showModal(modalDialog(title = "Adding ticker to cache...", easyClose = FALSE, footer = NULL))
		if(length(input$add_ticker) > 0) {
			for(t in input$add_ticker) {
				updateCache(t)
			}
			av_tickers$av_tickers <- list.files(cache_path, recursive = FALSE, full.names=FALSE)
			for(t in av_tickers$av_tickers) {
				ticker_choice$ticker_choice <- ticker_choice$ticker_choice[ticker_choice$ticker_choice != t]
			}
			update_table()
		}
		removeModal()
	})
	
	output$cached_tickers <- renderUI({
		temp <- av_tickers$av_tickers
		selectizeInput(inputId = "update_ticker", label = "Cached Tickers", choices = c(temp), multiple = TRUE)
	})
	
	
	observeEvent(input$update_tickers, {
		showModal(modalDialog(title = "Updating cache...", easyClose = FALSE, footer = NULL))
		if(length(input$update_ticker) > 0) {
			for(t in input$update_ticker) {
				updateCache(t)
			}
			update_table()
		}
		removeModal()
	})
}