library(rstatix)
library(rshinystatistics)

subset_by_tbl <- function(data, tbl, group = intersect(colnames(data), colnames(tbl))) {
  data <- df2qqs(data, group)
  df <- do.call(rbind, lapply(seq(1:nrow(tbl)), FUN = function(i) {
    idx <- rep(T, nrow(data))
    for (group in group) { idx <- idx & (data[[group]] == tbl[[group]][i]) }
    return(data[idx,])
  }))
  return(df)
}

remove_min_per_group <- function(data, group, min = 3) {
  data <- df2qqs(data, group = group)
  freq <- freq_table(data, vars = group)
  freq <- freq[!freq$n < min,]
  return(subset_by_tbl(data, freq, group))
}

data_with_min_per_group <- function(data, group, min = 3) {
  data <- df2qqs(data, group)
  freq <- freq_table(data, vars = group)
  freq <- freq[freq$n < min,]
  return(subset_by_tbl(data, freq, group))
}

get_ids_of_min_per_group <- function(dat, group, wid, min = 3) {
  df <- data_with_min_per_group(dat, group, min)
  return(unique(df[[wid]]))
}

