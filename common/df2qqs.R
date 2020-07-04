library(stats)

df2qqs <- function(data, group) {
  data <- as.data.frame(data)
  for (iv in group) {
    if (is.numeric(data[[iv]])) {
      quantiles <- quantile(data[[iv]])
      data[[iv]] <- sapply(data[[iv]], FUN = function(x) {
        if (x <= quantiles[[2]]) "low"
        else if (x >= quantiles[[4]]) "high"
        else "medium"
      })
      data[[iv]] <- factor(data[[iv]], levels=c("low", "medium", "high"))
    }
  }
  return(data)
}
