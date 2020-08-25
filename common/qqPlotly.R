wants <- c('stats', 'robcbi', 'plotly')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

source(paste0(getwd(),"/common/getQQline.R"))

library(plotly)
library(stats)

qqPlotly <- function(x, y, width = 400, height = 400) {
  names(y) <- x
  y <- sort(y)
  x.norm <- qqnorm(y, plot.it = F)
  qqline <- getQQline(y)
  
  plot_df <- as.data.frame(cbind(x=as.double(x.norm$x), y=as.double(x.norm$y), wid = names(y)))
  
  x0=seq(floor(as.numeric(min(x.norm$x)))-1.5, floor(as.numeric(max(x.norm$x)))+1.5)
  y0=(qqline$slope*x0)+as.numeric(qqline$intercept)
  
  p <- plot_ly(width = width, height = height)
  p <- add_trace(p, x=x0, y=y0, mode="lines", line=list(color='blue', width=1))
  p <- add_trace(p, data=plot_df, type='scatter', x=~x, y=~y, text=~wid
                 , marker = list(size=10, color='red', line = list(color='red', width=2)))
  p <- layout(p, showlegend=F 
              , xaxis = list(title = "theoretical", showline = T, zeroline = F, range=c(min(x0),max(x0)))
              , yaxis = list(title = "sample", showline = T, zeroline = F, range=c(min(y0),min(y0))))
  return(p)
}
