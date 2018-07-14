# download_immigrant()
# source('C:/Users/user/Dropbox/R_project/texas_jails/R/utils.R')
# immigrant <- get_all_immigrant()
# sapply(immigrant, unique)
# setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/clean_data")
# readr::write_csv(immigrant, path = "texas_jails_immigrant_detainer_2011_2017.csv")

download_immigrant <- function() {
  library(rvest)
  url <- "https://www.tcjs.state.tx.us/index.php?linkID=328"
  page <- read_html(url)
  links <-
    page %>%
    html_nodes("a") %>%
    html_attr("href")
  links <- links[grepl("detainerreports", links, ignore.case = TRUE)]

  links <- gsub("^..", "", links)
  links <- paste0("https://www.tcjs.state.tx.us/", links)
  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/immigrant_detainer")
  for (link in links) {
    link_name <- gsub("https://www.tcjs.state.tx.us//docs/ImmigrationDetainerReports/", "", link)
    download.file(link, destfile = link_name, mode = "wb")

  }
}

get_all_immigrant <- function() {
  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/immigrant_detainer")
  data <- data.frame(stringsAsFactors = FALSE)
  files <- list.files(full.names = TRUE)
  for (file in files) {
    temp <- get_immigrant(file)
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

get_immigrant <- function(file) {
    txt   <- suppressMessages(pdftools::pdf_text(file))
    txt   <- unlist(strsplit(txt, split = "\n"))
    txt   <- trimws(txt)

    txt <- txt[grep("county", txt, ignore.case = TRUE)[1]:length(txt)]

    txt   <- txt[grep("^Page |County |:..:.. |immigration detainer|total",
                      txt, ignore.case = TRUE, invert = TRUE)]

    # Some files have a date column so this removes it
    txt <- gsub("[0-9]+/[0-9]+/[0-9]+", "         ", txt)
    txt <- gsub("([0-9]+) ", "\\1    ", txt)

    txt        <- data.frame(stringr::str_split_fixed(txt, "\\s{2,}", 4),
                             stringsAsFactors = FALSE)
    names(txt) <- c("county",
                    "num_of_inmates",
                    "num_of_inmate_days",
                    "cost_in_dollars")
    txt <- txt[grep("^[0-9]", txt$county, invert = TRUE), ]
    txt$cost_in_dollars    <- readr::parse_number(txt$cost_in_dollars)
    txt$num_of_inmates     <- readr::parse_number(txt$num_of_inmates)
    txt$num_of_inmate_days <- readr::parse_number(txt$num_of_inmate_days)


    # total <-
    #   txt %>%
    #   dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)
    # total$county <- "total in state"
    # txt <- dplyr::bind_rows(txt, total)


    txt$link <- paste0("https://www.tcjs.state.tx.us//docs/ImmigrationDetainerReports/",
                       file)
    txt$year  <- gsub(".* ([0-9]+).*\\.pdf", "\\1", file)
    txt$year[nchar(txt$year) == 2] <- paste0("20", txt$year[nchar(txt$year) == 2])
    txt$year <- as.numeric(txt$year)
    file <- gsub(" old ", " ", file, ignore.case = TRUE)
    txt$month <- gsub(".*(Report |Reports/)([[:alpha:]]+) .*\\.pdf", "\\2", file)
    if (grepl("ID Report", file)) {
      txt$month <- gsub("^.*/([[:alpha:]]+) .*", "\\1", file)
    }
    txt$month  <- tolower(txt$month)
    txt        <- fix_months(txt)
    txt$month  <- tolower(txt$month)

    txt$county <- tolower(txt$county)
    txt$county <- gsub("([0-9])\\(p\\)", "(private facility - \\1)", txt$county)
    txt$county <- gsub("\\(p\\)", "(private facility)", txt$county)

    return(txt)

}

