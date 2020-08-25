


get_choices <- function(df, type = "id", wid = c()) {
  ids <- colnames(df)[sapply(colnames(df), function (x) { anyDuplicated(df[[x]]) == 0 })]
  two_between <- colnames(df)[sapply(colnames(df), function (x) { length(unique(df[[x]])) == 2 })]
  
  between <- setdiff(colnames(df), c(wid, "row.pos"))
  
  dvs <- setdiff(colnames(df), c(wid, "row.pos"))
  dvs <- dvs[sapply(dvs, FUN = function(x) is.numeric(df[[x]]))]
  
  if (type == "id") return(ids)
  else if (type == "between") return(between)
  else if (type == "two-between" || type == "two-group") return(two_between)
  else if (type == "dv" || type == "covar") return(dvs)
}


