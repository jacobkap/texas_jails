fix_months <- function(data) {
  month_fix <- data.frame(month = tolower(month.abb),
                          month_name = tolower(month.name),
                          stringsAsFactors = FALSE)
  data <- suppressMessages(dplyr::left_join(data, month_fix))
  data$month[!is.na(data$month_name)] <- data$month_name[!is.na(data$month_name)]
  data$month[data$month == "sept"] <- "september"
  data$month_name <- NULL
  return(data)
}