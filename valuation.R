#EBITDA = Net income + Tax + Interest + Depreciation & Amortization
#EBIT = Net income + Tax + Interest
#Operating Cash Flow = Net income + Depreciation & Amortization + Non-cash items (Stock-based compensation, unrealized gain/loss, write-downs) - changes in net working capital
#Free Cash Flow = Operating Cash Flow - Capital Expenditures
#Free Cash Flow to Equity = Operating Cash Flow - Capital Expenditures + Net Debt Issued
#Free Cash Flow to Firm = EBIT - Tax + Depreciation & Amortization - Increase in non-working capital - Capex


#Make the equations adjustable in case the data is wrong.

#TODO
#EBITDA
#Cash Flow
#Free Cash Flow
#Free Cash Flow to Equity
#Unleveraged Free CashF low (this is also free cash flow to firm?)
#Leveraged Discounted Cash Flow
#Unleveraged Discounted Cash Flow
#Dividend Discount Model
#Weighted Average Cost of Capital (This is sometimes used as the discount rate?)
#API for ten year yield from teasury 
#API for GDP see WDI


#update per minute while running
getQuote <- function(ticker) {
	URL <- paste(quote_call, "&symbol=", ticker, "&apikey=", apikey, sep="")
	results <- fromJSON(file = URL)
	return(results)
}

#calculates the effective tax rates over 5 years
getEffectiveTax <- function(income) {
	IBT <- income[[1]]$incomeBeforeTax
	ITE <- income[[1]]$incomeTaxExpense
	rates <- ITE / IBT
	return(rates)
}

#metrics from profile
#earnings-per-share
EPS <- function(df) {
	return(df[[1]]$EPS)
}

#price-to-earnings
PE_ratio <- function(df) {
	return(df[[1]]$PERatio)
}

#price-earnings-to-growth
PEG_ratio <- function(df) {
	return(df[[1]]$PEGRatio)
}

#Operating Margin TTM
Operating_Margin <- function(df) {
	return(df[[1]]$OperatingMarginTTM)
}

#price-to-sales TTM
PS_ratio <- function(df) {
	return(df[[1]]$PriceToSalesRatioTTM)
}

#price-to-book
PB_ratio <- function() {
	return(df[[1]]$PriceToBookRatio)
}

#beta 
Beta <- function(df) {
	return(df[[1]]$Beta)
}

#EV/EBITDA
EV_EBITDA <- function(df) {
	return(df[[1]]$EVToEBITDA)
}
#metrics from profile

##company to company valuation metrics
#provide correlations between the historical data between two companies



quote_daily <- getQuote(ticker)

getEBITDA <- function(df) {
	nt <- df[[5]][[1]]$netIncome
	ie <- df[[5]][[1]]$interestExpense
	it <- df[[5]][[1]]$incomeTaxExpense
	de <- df[[7]][[1]]$depreciation
	return(nt + ie + it + de)
}

getEBIT <- function(df) {
	nt <- df[[5]][[1]]$netIncome
	ie <- df[[5]][[1]]$interestExpense
	it <- df[[5]][[1]]$incomeTaxExpense
	return(nt + ie + it)

}

totalDebt <- function(df) {
	df[[5]][[2]]$shortTermDebt
	df[[5]][[2]]$longTermDebt
	df[[5]][[2]]$capitalLeaseObligations
	

	df[[5]]
}

getCashFlow <- function(df) {
	return(df[[7]][[1]]$operatingCashflow)
}

getFreeCashFlow <- function(df) {
	return(getCashFlow(df) - df[[7]][[1]]$cashflowFromInvestment)
}

getFreeCashFlowtoEquity <- function(df) {
	getFreeCashFlow(df) + 
}

getUnleveredFreeCashFlow <- function(df) [

}

getDCF <- function() {

}

getDDM <- function() {

}

getWACC <- function() {

}

getGDP <- function() {
	#see https://cran.r-project.org/web/packages/WDI/WDI.pdf
}

getTenYearYield <- function() {
	#see https://www.business-science.io/finance/2020/02/21/tidy-discounted-cash-flow.html
}

#price-to-cash-flow
PtCF_ratio <- function() {

}

DtE_ratio <- function() {

}

Working_Capital <- function() {

}

Inventory_Turnover_ration <- function() {

}
