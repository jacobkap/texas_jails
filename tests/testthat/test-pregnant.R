context("pregnant")

file1 <- system.file("data/pregnant",
                     "Pregnant Female Reporting Apr 2012.pdf",
                     package = "texasjails")
file2 <- system.file("data/pregnant",
                     "Pregnant Female Reporting Jan 2012.pdf",
                     package = "texasjails")
file3 <- system.file("data/pregnant",
                     "Pregnant Female Reporting July 2017.pdf",
                     package = "texasjails")
file4 <- system.file("data/pregnant",
                     "Pregnant Female Reporting May 2013.pdf",
                     package = "texasjails")
file5 <- system.file("data/pregnant",
                     "Pregnant Female Reporting Sept 2015.pdf",
                     package = "texasjails")


file1 <- get_pregnant(file1)
file2 <- get_pregnant(file2)
file3 <- get_pregnant(file3)
file4 <- get_pregnant(file4)
file5 <- get_pregnant(file5)

test_that("year is correct", {
  expect_equal(unique(file1$year), 2012)
  expect_equal(unique(file2$year), 2012)
  expect_equal(unique(file3$year), 2017)
  expect_equal(unique(file4$year), 2013)
  expect_equal(unique(file5$year), 2015)
})

test_that("month is correct", {
  expect_equal(unique(file1$month), "april")
  expect_equal(unique(file2$month), "january")
  expect_equal(unique(file3$month), "july")
  expect_equal(unique(file4$month), "may")
  expect_equal(unique(file5$month), "september")
})


test_that("counties are correct", {
  expect_equal(head(file1$county), c("anderson", "andrews", "angelina",
                                    "aransas", "archer", "armstrong"))
  expect_equal(tail(file1$county), c("wood", "yoakum", "young",
                                    "zapata", "zavala", "zavala (private facility)"))

  expect_equal(head(file2$county), c("anderson", "andrews", "angelina",
                                    "aransas", "archer", "armstrong"))
  expect_equal(tail(file2$county), c("wood", "yoakum", "young",
                                    "zapata", "zavala", "zavala (private facility)"))

  expect_equal(head(file3$county), c("anderson", "andrews", "angelina",
                                    "aransas", "archer", "armstrong"))
  expect_equal(tail(file3$county), c("wood", "yoakum", "young",
                                    "zapata", "zavala", "zavala (private facility)"))

  expect_equal(head(file4$county), c("anderson", "andrews", "angelina",
                                    "aransas", "archer", "armstrong"))
  expect_equal(tail(file4$county), c("wood", "yoakum", "young",
                                    "zapata", "zavala", "zavala (private facility)"))

  expect_equal(head(file5$county), c("anderson", "andrews", "angelina",
                                    "aransas", "archer", "armstrong"))
  expect_equal(tail(file5$county), c("wood", "yoakum", "young",
                                    "zapata", "zavala", "zavala (private facility)"))
})


test_that("number of pregnant women are correct", {
  expect_equal(head(file1$num_pregnant_women), c(1, 1, 2, 3, 0, 0))
  expect_equal(tail(file1$num_pregnant_women), c(2, 0, 1, 0, 0, 0))

  expect_equal(head(file2$num_pregnant_women), c(0, 1, 0, 6, 0, 0))
  expect_equal(tail(file2$num_pregnant_women), c(2, 0, 1, 0, 0, 0))

  expect_equal(head(file3$num_pregnant_women), c(0, 0, 0, 3, 0, 0))
  expect_equal(tail(file3$num_pregnant_women), c(1, 0, 0, 1, 0, 0))

  expect_equal(head(file4$num_pregnant_women), c(2, 0, 1, 0, 0, 0))
  expect_equal(tail(file4$num_pregnant_women), c(0, 1, 0, 0, 0, 1))

  expect_equal(head(file5$num_pregnant_women), c(1, 0, 1, 1, 0, 0))
  expect_equal(tail(file5$num_pregnant_women), c(0, 0, 2, 0, 0, 0))
})
