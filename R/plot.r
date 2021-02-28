###

#Tables then plots within the same tab?


#getCandleStick <- function(df1, colors = c("red","forestgreen") {
#	fig <- plot_ly(df1, x = ~date, xend = ~date, color = ~close > open, colors = colors, hoverinfo = "none") 
#	fig <- fig %>% add_segments(y = ~low, yend = ~high, size = I(1)) 
#	fig <- fig %>% add_segments(y = ~open, yend = ~close, size = I(3)) 
#	fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "Price")) 
#	#fig <- fig %>% rangeslider() #may not need this if I can setup start / stop dates in drop downs
#	fig2 <- df1
#	fig2 <- fig2 %>% plot_ly(x=~date, y=~volume, type='bar', name = "", color = ~close > open, colors = colors) 
#	fig2 <- fig2 %>% layout(showlegend = FALSE, yaxis = list(title = "Volume")) 
#	fig <- subplot(fig, fig2, heights = c(0.7, 0.2), nrows=2, shareX = TRUE, titleY = TRUE)
#	return(fig)
#}

