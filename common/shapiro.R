library(stats)
library(rstatix)

source(paste0(getwd(),'/common/dealing_with_groups.R'))

shapiro_by_res <- function(data, dvs, between, within=c(), dv.var = NULL) {
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) data <- data[which(data[[dv.var]] == dv),]
    sformula <- paste(dv ,'~', paste0(between, collapse = "*"))
    mdl <- lm(as.formula(sformula), data = data)
    
    shapiro <- shapiro.test(residuals(mdl))
    shapiro <- add_significance(data.frame(
      statistic = shapiro$statistic,
      p = shapiro$p.value,
      normality = if (shapiro$p.value < 0.05) 'NO' else 'YES'
    ))
    if (!is.null(dv.var)) shapiro <- cbind(var = dv, shapiro)
    return(shapiro)
  }))
}

shapiro_per_group <- function(data, dvs, group, dv.var = NULL) {
  shapiro_df <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) data <- data[which(data[[dv.var]] == dv),]
    dat <- remove_min_per_group(data, group)
    dat <- group_by_at(df2qqs(dat, group), vars(group))
    df <- tryCatch(shapiro_test(dat, vars=dv), error = function(e) NULL)
    if (!is.null(df)) {
      freq <- freq_table(as.data.frame(dat), vars = group)
      df <- merge(freq, as.data.frame(add_significance(df)))
      if (!is.null(dv.var)) df <- cbind(var = dv, df)
      return(df)
    }
  }))
  return(shapiro_df)
}
