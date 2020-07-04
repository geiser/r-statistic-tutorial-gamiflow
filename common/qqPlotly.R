library(plotly)

qqPlotly <- function(x, y, width = 400, height = 400) {
  names(y) <- x
  y <- sort(y)
  
  i <- 1:length(y)
  fi <- (i - 0.5) / length(y)
  x.norm <- qnorm(fi)
  
  plot_df <- as.data.frame(cbind(x=as.double(x.norm), y=as.double(y), wid = names(y)))
  
  # compute the intercept and slope of the line that passes
  y <- as.numeric(quantile(y, c(0.25, 0.75), type = 5))
  x <- qnorm( c(0.25, 0.75))
  slope <- as.numeric(diff(y)) / diff(x)
  int <- y[1] - slope * x[1]
  
  x0=floor(as.numeric(min(plot_df$x)))-0.5
  x1=floor(as.numeric(max(plot_df$x)))+0.5
  y0=(slope*x0)+as.numeric(int)-0.5
  y1=(slope*x1)+as.numeric(int)+0.5
  
  p <- plot_ly(width = width, height = height)
  p <- add_trace(p, x=seq(x0,x1, by=(x1-x0)/(nrow(plot_df)-1))
                 , y=seq(y0,y1, by=(y1-y0)/(nrow(plot_df)-1))
                 , mode="lines", line=list(color='blue', width=1))
  p <- add_trace(p, data=plot_df, type='scatter', x=~x, y=~y, text=~wid
                 , marker = list(size=10, color='red', line = list(color='red', width=2)))
  p <- layout(p, showlegend=F 
              , xaxis = list(title = "theoretical", showline = T, zeroline = F, range=c(x0,x1))
              , yaxis = list(title = "sample", showline = T, zeroline = F, range=c(y0,y1)))
  return(p)
}
