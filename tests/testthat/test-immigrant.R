context("immigrant")

file1 <- system.file("data/immigrant_detainer",
                     "April 2015 ID Report.pdf",
                     package = "texasjails")
file2 <- system.file("data/immigrant_detainer",
                     "Immigration Detainer Report April 2017.pdf",
                     package = "texasjails")
file3 <- system.file("data/immigrant_detainer",
                     "Immigration Detainer Report Feb 2016.pdf",
                     package = "texasjails")
file4 <- system.file("data/immigrant_detainer",
                     "Immigration Detainer Report March 12.pdf",
                     package = "texasjails")
file5 <- system.file("data/immigrant_detainer",
                     "Immigration Detainer Report Nov 13.pdf",
                     package = "texasjails")


file1 <- get_immigrant(file1)
file2 <- get_immigrant(file2)
file3 <- get_immigrant(file3)
file4 <- get_immigrant(file4)
file5 <- get_immigrant(file5)

test_that("year is correct", {
  expect_equal(unique(file1$year), 2015)
  expect_equal(unique(file2$year), 2017)
  expect_equal(unique(file3$year), 2016)
  expect_equal(unique(file4$year), 2012)
  expect_equal(unique(file5$year), 2013)
})

test_that("month is correct", {
  expect_equal(unique(file1$month), "april")
  expect_equal(unique(file2$month), "april")
  expect_equal(unique(file3$month), "february")
  expect_equal(unique(file4$month), "march")
  expect_equal(unique(file5$month), "november")
})


test_that("counties are correct", {
  expect_equal(head(file1$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file1$county), c("kleberg", "knox", "la salle", "lamar", "lamb", "lampasas"))

  expect_equal(head(file2$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file2$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))

  expect_equal(head(file3$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file3$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))

  expect_equal(head(file4$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file4$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))

  expect_equal(head(file5$county), c("anderson", "andrews", "angelina",
                                     "aransas", "archer", "armstrong"))
  expect_equal(tail(file5$county), c("wise", "wood", "yoakum",
                                     "young", "zapata", "zavala"))
})


test_that("number of inmates are correct", {
  expect_equal(head(file1$num_of_inmates), c(1, 2, 0, 13, 0, 0))
  expect_equal(tail(file1$num_of_inmates), c(3, 0, 0, 1, 0, 0))

  expect_equal(head(file2$num_of_inmates), c(3, 1, 4, 16, 0, 0))
  expect_equal(tail(file2$num_of_inmates), c(2, 0, 1, 1, 8, 0))

  expect_equal(head(file3$num_of_inmates), c(1, 2, 0, 17, 0, 0))
  expect_equal(tail(file3$num_of_inmates), c(2, 0, 1, 0, 9, 0))

  expect_equal(head(file4$num_of_inmates), c(2, 2, 3, 9, 0, 0))
  expect_equal(tail(file4$num_of_inmates), c(10, 3, 5, 2, 20, 3))

  expect_equal(head(file5$num_of_inmates), c(1, 2, 1, 14, 0, 1))
  expect_equal(tail(file5$num_of_inmates), c(11, 1, 1, 0, 20, 0))
})

test_that("number of inmate days are correct", {
  expect_equal(head(file1$num_of_inmate_days), c(16, 6, 0, 38, 0, 0))
  expect_equal(tail(file1$num_of_inmate_days), c(53, 0, 0, 2, 0, 0))

  expect_equal(head(file2$num_of_inmate_days), c(93, 31, 88, 32, 0, 0))
  expect_equal(tail(file2$num_of_inmate_days), c(62, 0, 31, 31, 113, 0))

  expect_equal(head(file3$num_of_inmate_days), c(31, 62, 0, 43, 0, 0))
  expect_equal(tail(file3$num_of_inmate_days), c(62, 0, 31, 0, 144, 0))

  expect_equal(head(file4$num_of_inmate_days), c(44, 43, 49, 18, 0, 0))
  expect_equal(tail(file4$num_of_inmate_days), c(230, 82, 117, 58, 373, 82))

  expect_equal(head(file5$num_of_inmate_days), c(31, 62, 31, 38, 0, 31))
  expect_equal(tail(file5$num_of_inmate_days), c(215, 31, 14, 0, 300, 0))
})

test_that("number of cost is correct", {
  expect_equal(head(file1$cost_in_dollars), c(352, 270, 0, 1843.38, 0, 0))
  expect_equal(tail(file1$cost_in_dollars), c(2157.63, 0, 0, 59.24, 0, 0))

  expect_equal(head(file2$cost_in_dollars), c(2046, 1395, 4105.2, 1595.2, 0, 0))
  expect_equal(tail(file2$cost_in_dollars), c(1673.38, 0, 1453.90, 784.30, 2827.26, 0))

  expect_equal(head(file3$cost_in_dollars), c(682, 2790, 0, 2085.93, 0, 0))
  expect_equal(tail(file3$cost_in_dollars), c(1654.16, 0, 1453.9, 0, 3602.88, 0))

  expect_equal(head(file4$cost_in_dollars), c(968, 2263.52, 2285.85, 745.2, 0, 0))
  expect_equal(tail(file4$cost_in_dollars), c(5862.7, 3541.58, 5487.3, 1331.68, 9272.78, 2402.6))

  expect_equal(head(file5$cost_in_dollars), c(682, 3263.68, 1446.15, 1573.2, 0, 2201))
  expect_equal(tail(file5$cost_in_dollars), c(5719, 1338.89, 656.6, 0, 7446, 0))
})
