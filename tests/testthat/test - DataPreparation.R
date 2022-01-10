library(testthat)

test_that("Unzipper", {

  dir <- "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests/zipTest"
  zipDir <- "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests"
  expect_true(!dir.exists(dir))

  ariadne::unzipper(directoryZipFiles = zipDir,
                    absolutePathDirectoryToCreate = dir

  )
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)

})



test_that("kaplanMeierPlotPreparationDT", {
  dirs <- list.dirs(path = "C:/Users/Alex/D/projects/Regeneron/ariadne/dataForTests/KMTest",
                    full.names = T,
                    recursive = F)

  out <- ariadne::kaplanMeierPlotPreparationDT(
    targetIds = c(101:112),
    outcomeIds = c(202:212),
    directories = dirs,
    kaplanMeierDataCsv = "cohort_time_to_event.csv"
  )
 expect_error(ariadne::kaplanMeierPlotPreparationDT(
   targetIds = c(101:112),
   outcomeIds = c(202:212),
   directories = dirs,
   kaplanMeierDataCsv = "cohort_time_to_event.csv"
 ), NA)
 expect_type(out, 'list')
 expect_true(is.data.frame(out))
 expect_gte(nrow(out), 1)

})

test_that("Covariate Data Preparation", {
  dirs <- list.dirs(path = "C:/Users/Alex/D/projects/PIONEER-Odysseus/UnzippedData",
                    full.names = T,
                    recursive = F)
  dt <- dirs[c(1, 2)]

  out <- ariadne::prepareCovariatesData(
    listOfDirectories,
    filterWindowIds = NULL,
    cohortIds = c(103, 112),
    covariateName = "covariate.csv",
    covariateValueName = "covariate_value.csv"
  )

  expect_true(is.data.frame(out))

  expect_gte(nrow(out), 1)

  expect_equal(length(unique(out$cohort_id)), 2)

})


test_that("Prepare Feature Proportion Data", {
  dirs <- list.dirs(path = "C:/Users/Alex/D/projects/PIONEER-Odysseus/UnzippedData",
                    full.names = T,
                    recursive = F)
  dt <- dirs[c(1, 2)]

  out <- ariadne::prepareFeatureProportionData(
    cohortIds = c(103, 112),
    listOfDirectories = dt,
    filterWindowIds = c(2, 3)
  )
  expect_error(ariadne::prepareFeatureProportionData(
    cohortIds = c(103, 112),
    listOfDirectories = dt,
    filterWindowIds = c(2, 3)
  ), NA)

  expect_true(is.data.frame(out))

  expect_gte(nrow(out), 1)

  expect_equal(length(unique(out$cohort_id)), 2)

})


test_that("Prepare Feature Proportion Data", {
  dirs <- list.dirs(path = "C:/Users/Alex/D/projects/PIONEER-Odysseus/UnzippedData",
                    full.names = T,
                    recursive = F)
  dt <- dirs[c(1, 2)]

  out <- ariadne::prepareCovariatesData(
    cohortIds = c(103, 112),
    listOfDirectories = dt,
    filterWindowIds = c(2, 3)
  )


  out <- ariadne::prepareCovariatesDataToPlotting(
    preparedCovariatesData = out,
    cohortIds = c(103, 112)
  )

  expect_true(is.data.frame(out))

  expect_gte(nrow(out), 1)

  expect_true("SMD" %in% names(out))

})
