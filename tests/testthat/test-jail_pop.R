context("jail_pop")

file1 <- system.file("data/jail_pop",
                     "Abbreviated Pop Rpt April 1992.pdf",
                     package = "texasjails")
file2 <- system.file("data/jail_pop",
                     "Abbreviated Pop Rpt April 1998.pdf",
                     package = "texasjails")
file3 <- system.file("data/jail_pop",
                     "Abbreviated Pop Rpt April 2017.pdf",
                     package = "texasjails")
file4 <- system.file("data/jail_pop",
                     "Abbreviated Pop Rpt Aug 2007.pdf",
                     package = "texasjails")
file5 <- system.file("data/jail_pop",
                     "Abbreviated Pop Rpt Dec 2008.pdf",
                     package = "texasjails")


file1 <- get_jail_pop(file1)
file2 <- get_jail_pop(file2)
file3 <- get_jail_pop(file3)
file4 <- get_jail_pop(file4)
file5 <- get_jail_pop(file5)

test_that("year is correct", {
  expect_equal(unique(file1$year), 1992)
  expect_equal(unique(file2$year), 1998)
  expect_equal(unique(file3$year), 2017)
  expect_equal(unique(file4$year), 2007)
  expect_equal(unique(file5$year), 2008)
})

test_that("month is correct", {
  expect_equal(unique(file1$month), "april")
  expect_equal(unique(file2$month), "april")
  expect_equal(unique(file3$month), "april")
  expect_equal(unique(file4$month), "august")
  expect_equal(unique(file5$month), "december")
})

test_that("counties are correct", {
  expect_equal(head(file1$county), c("anderson", "andrews", "angelina",
                                     "angelina (private facility)", "aransas", "archer"))
  expect_equal(tail(file1$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))

  expect_equal(head(file2$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file2$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))

  expect_equal(head(file3$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file3$county), c("wood", "yoakum", "young", "zapata",
                                     "zavala", "zavala (private facility)"))

  expect_equal(head(file4$county), c("anderson", "andrews", "angelina",
                                     "angelina (private facility)", "aransas", "archer"))
  expect_equal(tail(file4$county), c("wood", "yoakum", "young",
                                     "zapata", "zavala", "zavala (private facility)"))

  expect_equal(head(file5$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file5$county), c("mclennan (private facility)",
                                     "newton (private facility)",
                                     "polk (private facility)",
                                     "val verde (private facility)",
                                     "zavala (private facility)",
                                     "littlefield (city facility)"))
})


test_that("pretrial felons", {
  expect_equal(head(file1$num_pretrial_felons), c(32, 15, 40, 0, 15, 3))
  expect_equal(tail(file1$num_pretrial_felons), c(16, 20, 8, 11, 0, 5))

  expect_equal(head(file2$num_pretrial_felons), c(52, 21, 37, 5, 3, 0))
  expect_equal(tail(file2$num_pretrial_felons), c(10, 34, 5, 7, 7, 3))

  expect_equal(head(file3$num_pretrial_felons), c(96, 26, 132, 33, 7, 2))
  expect_equal(tail(file3$num_pretrial_felons), c(69, 5, 20, 15, 30, 0))

  expect_equal(head(file4$num_pretrial_felons), c(72, 14, 70, 0, 25, 5))
  expect_equal(tail(file4$num_pretrial_felons), c(38, 8, 14, 16, 0, 0))

  expect_equal(head(file5$num_pretrial_felons), c(92, 10, 91, 24, 7, 1))
  expect_equal(tail(file5$num_pretrial_felons), c(0, 0, 0, 43, 0, 0))
})

test_that("convicted felons", {
  expect_equal(head(file1$num_convicted_felons), c(50, 4, 56, 478, 7, 0))
  expect_equal(tail(file1$num_convicted_felons), c(15, 23, 0, 3, 0, 8))

  expect_equal(head(file2$num_convicted_felons), c(4, 4, 16, 4, 9, 3))
  expect_equal(tail(file2$num_convicted_felons), c(2, 4, 0, 3, 0, 5))

  expect_equal(head(file3$num_convicted_felons), c(1, 0, 24, 13, 0, 0))
  expect_equal(tail(file3$num_convicted_felons), c(12, 3, 4, 4, 1, 0))

  expect_equal(head(file4$num_convicted_felons), c(5, 16, 33, 0, 17, 3))
  expect_equal(tail(file4$num_convicted_felons), c(8, 2, 6, 2, 0, 0))

  expect_equal(head(file5$num_convicted_felons), c(8, 9, 42, 12, 0, 0))
  expect_equal(tail(file5$num_convicted_felons), c(0, 0, 0, 16, 0, 0))
})

test_that("convicted felons sentenced to county jail time", {
  expect_true(is.na(unique(file1$num_convicted_felony_sentenced_jail)))

  expect_equal(head(file2$num_convicted_felony_sentenced_jail), c(0, 0, 9, 0, 0, 0))
  expect_equal(tail(file2$num_convicted_felony_sentenced_jail), c(0, 1, 0, 0, 0, 0))

  expect_equal(head(file3$num_convicted_felony_sentenced_jail), c(0, 1, 4, 0, 0, 0))
  expect_equal(tail(file3$num_convicted_felony_sentenced_jail), c(1, 0, 1, 0, 0, 0))

  expect_equal(head(file4$num_convicted_felony_sentenced_jail), c(0, 2, 23, 0, 1, 0))
  expect_equal(tail(file4$num_convicted_felony_sentenced_jail), c(6, 0, 0, 0, 2, 0))

  expect_equal(head(file5$num_convicted_felony_sentenced_jail), c(0, 0, 9, 0, 0, 1))
  expect_equal(tail(file5$num_convicted_felony_sentenced_jail), c(0, 0, 0, 3, 0, 0))
})

test_that("parole vioators", {
  expect_equal(head(file1$num_parole_violators), c(2, 2, 13, 0, 4, 0))
  expect_equal(tail(file1$num_parole_violators), c(2, 1, 0, 0, 2, 0))

  expect_equal(head(file2$num_parole_violators), c(7, 1, 4, 5, 0, 0))
  expect_equal(tail(file2$num_parole_violators), c(3, 3, 0, 2, 0, 0))

  expect_equal(head(file3$num_parole_violators), c(2, 2, 8, 7, 1, 0))
  expect_equal(tail(file3$num_parole_violators), c(8, 0, 3, 2, 2, 0))

  expect_equal(head(file4$num_parole_violators), c(4, 0, 10, 0, 5, 2))
  expect_equal(tail(file4$num_parole_violators), c(4, 0, 2, 0, 2, 0))

  expect_equal(head(file5$num_parole_violators), c(6, 0, 6, 5, 1, 0))
  expect_equal(tail(file5$num_parole_violators), c(0, 0, 0, 6, 0, 0))
})

test_that("federal", {
  expect_equal(head(file1$num_federal), c(0, 0, 0, 0, 0, 0))
  expect_equal(tail(file1$num_federal), c(0, 0, 0, 0, 0, 0))

  expect_equal(head(file2$num_federal), c(0, 0, 0, 0, 0, 0))
  expect_equal(tail(file2$num_federal), c(2, 0, 0, 0, 41, 37))

  expect_equal(head(file3$num_federal), c(0, 0, 0, 69, 1, 0))
  expect_equal(tail(file3$num_federal), c(0, 0, 0, 65, 23, 0))

  expect_equal(head(file4$num_federal), c(0, 0, 24, 0, 59, 0))
  expect_equal(tail(file4$num_federal), c(0, 0, 0, 144, 40, 493))

  expect_equal(head(file5$num_federal), c(0, 0, 33, 73, 0, 0))
  expect_equal(tail(file5$num_federal), c(189, 0, 895, 96, 219, 0))
})

test_that("pretrial SJF", {
  expect_true(is.na(unique(file1$num_pretrial_state_jail_felons)))

  expect_equal(head(file2$num_pretrial_state_jail_felons), c(0, 2, 0, 0, 0, 0))
  expect_equal(tail(file2$num_pretrial_state_jail_felons), c(12, 5, 0, 9, 0, 0))

  expect_equal(head(file3$num_pretrial_state_jail_felons), c(25, 3, 34, 10, 8, 0))
  expect_equal(tail(file3$num_pretrial_state_jail_felons), c(22, 1, 7, 5, 0, 0))

  expect_equal(head(file4$num_pretrial_state_jail_felons), c(6, 0, 21, 0, 16, 0))
  expect_equal(tail(file4$num_pretrial_state_jail_felons), c(13, 3, 10, 6, 0, 0))

  expect_equal(head(file5$num_pretrial_state_jail_felons), c(0, 1, 23, 4, 1, 0))
  expect_equal(tail(file5$num_pretrial_state_jail_felons), c(0, 0, 0, 0, 0, 0))
})

test_that("percent of capacity", {
  expect_equal(head(file1$percent_of_capacity), c(87.6, 50, 115.32, 98.58, 92.31, 66.67))
  expect_equal(tail(file1$percent_of_capacity), c(68.06, 64.1, 92.31, 61.76, 0, 20.9))

  expect_equal(head(file2$percent_of_capacity), c(71.32, 84, 80.18, 30.65, 75, 62.5))
  expect_equal(tail(file2$percent_of_capacity), c(65, 82.05, 46.15, 90.48, 106.25, 95.45))

  expect_equal(head(file3$percent_of_capacity), c(56.33, 68, 89.96, 86.32, 60.42, 50))
  expect_equal(tail(file3$percent_of_capacity), c(91.08, 29.17, 43.06, 52.92, 95.45, 0))

  expect_equal(head(file4$percent_of_capacity), c(39.86, 94, 91.04, 97.3, 100, 116.67))
  expect_equal(tail(file4$percent_of_capacity), c(64.97, 53.85, 92.31, 77.08, 72.73, 96.12))

  expect_equal(head(file5$percent_of_capacity), c(60.14, 72, 90.68, 91.88, 33.33, 37.5))
  expect_equal(tail(file5$percent_of_capacity), c(94.17, 92.17, 91.18, 96.7, 53.4, 80.16))
})