# download_jail_pop()
# source('C:/Users/user/Dropbox/R_project/texas_jails/R/utils.R')
# jail_pop <- get_all_jail_pop()
# # summary(jail_pop)
# # table(jail_pop$year)
# # table(jail_pop$month)
# # unique(jail_pop$county)
# setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/clean_data")
# readr::write_csv(jail_pop, path = "texas_jails_jail_pop_1992_2017.csv")

download_jail_pop <- function() {
  library(rvest)
  url <- "https://www.tcjs.state.tx.us/index.php?linkID=326"

  page <- read_html(url)

  links <-
    page %>%
    html_nodes("a") %>%
    html_attr("href")
  links <- links[grepl("Abbreviated Pop", links, ignore.case = TRUE)]

  links <- gsub("^../", "", links)
  links <- paste0("https://www.tcjs.state.tx.us/", links)


  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/jail_pop")
  for (link in links) {
    link_name <- gsub("https://www.tcjs.state.tx.us/docs/AbbreviatedPopReports/", "", link)
    download.file(link, destfile = link_name, mode = "wb")

  }
}

get_all_jail_pop <- function() {
  setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/jail_pop")
  data <- data.frame(stringsAsFactors = FALSE)
  files <- list.files(full.names = TRUE)
  for (file in files) {

    temp <- get_jail_pop(file)
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

get_jail_pop <- function(file) {

    txt   <- suppressMessages(pdftools::pdf_text(file))
    txt   <- unlist(strsplit(txt, split = "\n"))
    txt   <- trimws(txt)

    txt <- txt[grep("county", txt, ignore.case = TRUE)[1] + 1:length(txt)]
    txt <- txt[grep("^conv. felon|sentenced to|county jail|pretrial|time|felons|page |total |convicted",
                    txt, ignore.case = TRUE, invert = TRUE)]
    txt <- txt[grep("co. jail|trial|highlighted|notes|privately|jail pop|facility|texas commission|inmates",
                    txt, ignore.case = TRUE, invert = TRUE)]
    txt <- txt[grep("[[:alpha:]]+ [0-9]+, [0-9]{4}|[[:alpha:]]+[0-9]+, [0-9]{4}|#REF!",
                    txt, ignore.case = TRUE, invert = TRUE)]
    txt <- txt[!is.na(txt)]

    # The NA text will get turned into real NA when parse_number is run

    # convicted felons sentenced to county jail time
    substr(txt, 30, 40)[trimws(substr(txt, 30, 40)) == ""] <- "   NA   "
    # pretrial SJF
    substr(txt, 95, 105)[trimws(substr(txt, 95, 105)) == ""] <- "   NA   "
    # convicted SJF sentenced to county jail time
    substr(txt, 106, 117)[trimws(substr(txt, 106, 117)) == ""] <- "   NA   "
    # convicted SJF sentenced to state jail time
    substr(txt, 118, 128)[trimws(substr(txt, 118, 128)) == ""] <- "   NA   "
    # percent of capacity
    substr(txt, 174, 184)[trimws(substr(txt, 174, 184)) == ""] <- "   NA   "

    txt <- gsub("([0-9]+) ", "\\1    ", txt)
    txt <- data.frame(stringr::str_split_fixed(txt, "\\s{2,}", 20),
                      stringsAsFactors = FALSE)
    names(txt) <- c("county",
                    "num_pretrial_felons",
                    "num_convicted_felons",
                    "num_convicted_felony_sentenced_jail",
                    "num_parole_violators",
                    "num_parole_violators_new_charge",
                    "num_pretrial_misdemeanor",
                    "num_convicted_misdemeanor",
                    "num_bench_warrants",
                    "num_federal",
                    "num_pretrial_state_jail_felons",
                    "num_convicted_state_jail_felons_to_county_jail_time",
                    "num_convicted_state_jail_felons_to_state_jail_time",
                    "total_others",
                    "total_local",
                    "total_contract",
                    "total_population",
                    "total_capacity",
                    "percent_of_capacity",
                    "available_beds")

    txt[2:20] <- sapply(txt[2:20], readr::parse_number)
#
#     total <-
#       txt %>%
#       dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)
#     total$county <- "total in state"
#     total[total == 0] <- NA
#     txt <- dplyr::bind_rows(txt, total)



    txt$link <- paste0("https://www.tcjs.state.tx.us/docs/AbbreviatedPopReports/",
                           file)
    txt$year  <- gsub(".* ([0-9]+).*\\.pdf", "\\1",
                      file)
    txt$year  <- as.numeric(txt$year)
    txt$month <- gsub(".* ([[:alpha:]]+) [0-9]{4}.pdf", "\\1",
                      file,
                      ignore.case = TRUE)
    txt$month  <- tolower(txt$month)
    txt  <- fix_months(txt)
    txt$month  <- tolower(txt$month)

    txt$county <- tolower(txt$county)
    txt$county <- gsub("([0-9])\\(p.*", "(private facility - \\1)",
                       txt$county)
    txt$county <- gsub("\\(p.*", "(private facility)",
                       txt$county)
    txt$county <- gsub(" c$", " (city facility)", txt$county)
    txt$county <- gsub(" \\(c.*", " (city facility)", txt$county)

    txt$county <- gsub("crystl cty", "crystal city",
                       txt$county)
    txt$county <- gsub("throckmort$", "throckmorton",
                       txt$county)
    txt$county <- gsub("augustin$", "augustine",
                       txt$county)
    txt$county <- gsub("reeves det*", "reeves",
                       txt$county)
    txt$county <- gsub("lasalle*", "la salle",
                       txt$county)
    return(txt)

}

