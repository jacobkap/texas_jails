# # # download_pregnant()
# source('C:/Users/user/Dropbox/R_project/texas_jails/R/utils.R')
# pregnant <- get_all_pregnant()
# sapply(pregnant, unique)
# setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/clean_data")
# readr::write_csv(pregnant, path = "texas_jails_pregnant_inmates_2012_2017.csv")

download_pregnant <- function() {
  library(rvest)
  url <- "https://www.tcjs.state.tx.us/index.php?linkID=329"
  page <- read_html(url)
  links <-
    page %>%
    html_nodes("a") %>%
    html_attr("href")
  links <- links[grepl("pregnant female reporting", links, ignore.case = TRUE)]
  links <- gsub("^..", "", links)
  links <- paste0("https://www.tcjs.state.tx.us/", links)
  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/pregnant")
  for (link in links) {
    link_name <- gsub("https://www.tcjs.state.tx.us//docs/PregnantFemaleReports/", "", link)
    download.file(link, destfile = link_name, mode = "wb")
  }
}

get_all_pregnant <- function() {
  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/pregnant")
  data <- data.frame(stringsAsFactors = FALSE)
  files <- list.files()
  for (file in files) {
    temp <- get_pregnant(file)
    data <- dplyr::bind_rows(data, temp)
  }
  data$month <- factor(data$month, levels = tolower(month.name))
  data <-
    data %>%
    dplyr::select(county,
                  year,
                  month,
                  everything()) %>%
    dplyr::arrange(desc(year), desc(month), county) %>%
    dplyr::mutate(month = as.character(month))

  return(data)
}

get_pregnant <- function(file) {

  txt   <- suppressMessages(pdftools::pdf_text(file))
  txt   <- unlist(strsplit(txt, split = "\n"))
  txt   <- trimws(txt)

  txt <- txt[grep("pregnant", txt, ignore.case = TRUE)[1] + 1:length(txt)]
  txt <- gsub(" ([0-9]+) ", "    \\1    ", txt)
  txt <- gsub(" ([0-9]+)$", "    \\1    ", txt)
  txt <- data.frame(stringr::str_split_fixed(txt, "\\s{3,}", 10), stringsAsFactors = FALSE)
  names(txt) <- rep("", 10)

  counties <- stack(txt[, c(1, 3, 5, 7, 9)])
  pregnant <- stack(txt[, c(2, 4, 6, 8, 10)])
  counties <- counties[, 1]
  pregnant <- pregnant[, 1]
  temp <- data.frame(county = counties,
                     num_pregnant_women = pregnant,
                     stringsAsFactors = FALSE)
  temp$num_pregnant_women <- as.numeric(temp$num_pregnant_women)
  temp <- temp[temp$county != "", ]
  #
  #
  #     total <- data.frame(county = "total in state",
  #                         num_pregnant_women = sum(temp$num_pregnant_women,
  #                                                  na.rm = TRUE),
  #                         stringsAsFactors = FALSE)
  #     temp <- dplyr::bind_rows(temp, total)

  temp$link <- paste0("https://www.tcjs.state.tx.us//docs/PregnantFemaleReports/",
                      file)

  temp$year   <- gsub(".* ([0-9]+).*\\.pdf", "\\1", file)
  temp$year   <- as.numeric(temp$year)
  temp$month  <-   gsub(".*Reporting ([[:alpha:]]+) [0-9]+.*", "\\1", file,
                        ignore.case = TRUE)
  temp$month  <- tolower(temp$month)
  temp        <- fix_months(temp)
  temp$month  <- tolower(temp$month)
  temp$county <- tolower(temp$county)
  temp$county <- gsub("([0-9])\\(p\\)", "(private facility - \\1)", temp$county)
  temp$county <- gsub("\\(p\\)", "(private facility)", temp$county)


  return(temp)
}

