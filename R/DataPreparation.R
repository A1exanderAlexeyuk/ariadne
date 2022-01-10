#' Unzip data
#'
#' @description unzipper function what unzip given data in directory given as an
#' argument
#'
#'
#'
#'
#' @param directoryZipFiles directory with zip files
#'
#' @param absolutePathDirectoryToCreate path to directory where to put unzipped files
#' if doesn't exist folder will be created
#' @export
#'
unzipper <- function(directoryZipFiles,
                     absolutePathDirectoryToCreate
){
  listOfFiles <- list.files(path = directoryZipFiles,
                            recursive = TRUE,
                            pattern = "\\.zip$",
                            full.names = T)
  lapply(listOfFiles, function(file) {
    folder <- gsub(".zip","", gsub(".*/","", file))

    dir.create(path = paste0(gsub("\\\\",
                                  '/',
                                  absolutePathDirectoryToCreate),
                             "/", folder), recursive = TRUE)

    utils::unzip(file, exdir = paste0(gsub("\\\\",
                                           '/',
                                           absolutePathDirectoryToCreate),
                                      "/", folder))
    return(NULL)
  })
}



#' Prepare data to KM plotting
#'
#' @param directories directories where stored time_to_event.csv
#'
#' @param targetIds target cohort ids to filter
#'
#' @param outcomeIds outcome cohort ids to filter
#'
#' @returns data.table dataframe
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
kaplanMeierPlotPreparationDT <- function(targetIds,
                                         outcomeIds,
                                         directories,
                                         kaplanMeierDataCsv) {
  unionAcrossDatabases <- lapply(directories, function(directory) {
    timeToEventTable <- data.table::fread(paste0(gsub("\\\\",
                                          '/',directory), "/", kaplanMeierDataCsv))  %>%
      subset(
        target_id %in% targetIds &
          outcome_id %in% outcomeIds
      )

  })
  data.table::rbindlist(unionAcrossDatabases)
}


#' Prepare covariate data
#'
#' @param listOfDirectories directories where stored covariate.csv and covariate_value.csv
#'
#' @param filterWindowIds window_ids to filter
#'
#' @param cohortIds target cohort ids to filter
#'
#' @param covariateName name of csv contains covariate data (usually covariate.csv
#' or covariate_ref.csv)
#'
#' @param covariateValueName name of csv contains covariate data (usually covariate_value.csv
#'
#'
#' @returns data.table dataframe
#'
#' @importFrom magrittr %>%
#'
#' @importFrom data.table :=
#'
#' @export
#' @examples
#' data <- prepareCovariatesData(
#' listOfDirectories = directiries,
#' filterWindowIds = NULL,
#' cohortIds = c(103, 112),
#' covariateName = "covariate.csv",
#' covariateValueName = "covariate_value.csv"
#' )
prepareCovariatesData <- function(
                              listOfDirectories,
                              filterWindowIds = NULL,
                              cohortIds,
                              covariateName = "covariate.csv",
                              covariateValueName = "covariate_value.csv"
                              ) {
  listOfDF <- lapply(listOfDirectories, function(directory) {

    covariate <- data.table::fread(paste0(gsub("\\\\",
                                               '/',directory), "/", covariateName))
    covariateValue <- data.table::fread(paste0(gsub("\\\\",
                                                     '/',directory), "/", covariateValueName))
    covariatesForPlotting <- data.table::merge.data.table(x = covariate,
                                                          y = covariateValue,
                                                          by = "covariate_id") %>%
      subset(
        cohort_id  %in% c(cohortIds)
      ) %>%
      subset(
        mean > 0
      )

    if(!is.null(filterWindowIds)) {
      covariatesForPlotting[,
                            window_id := data.table::fcase(
                              covariate_id %% 10 == 4 , 4,
                              covariate_id %% 10 == 3 , 3,
                              covariate_id %% 10 == 2 , 2,
                              covariate_id %% 10 == 1 , 1
                            )]

      covariatesForPlotting <- subset(covariatesForPlotting, window_id %in% filterWindowIds)
    } else {
      covariatesForPlotting
    }
  })
  return(data.table::rbindlist(listOfDF))
}

#' @export
#'
prepareFeatureProportionData <- function(listOfDirectories,
                                        filterWindowIds = NULL,
                                        cohortIds){
  listOfDF <- lapply(listOfDirectories, function(directory) {
    fp <- data.table::fread(paste0(gsub("\\\\",
                                               '/',directory), "/", "feature_proportions.csv"))
    featureProportionsConsolidated <- fp %>%
      subset(cohort_id  %in% c(cohortIds) & mean > 0
      )
    if(!is.null(filterWindowIds)) {
      featureProportionsConsolidated <- subset(featureProportionsConsolidated,
                                               window_id %in% filterWindowIds)
    } else {
      featureProportionsConsolidated
    }
  })
  return(data.table::rbindlist(listOfDF))
}

#' Prepare covariate data for comparative analysis
#'
#' @param preparedCovariatesData csv or data table form prepareCovariatesData function
#'
#'
#' @param targetCogortId1 target cohort id 1
#'
#' @param targetCogortId2  target cohort id 2
#'
#'
#' @returns  merged data table with calculated SMD (SMD column)
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#'
#' dataF <- prepareCovariatesDataToPlotting(preparedCovariatesData = data,
#'cohortIds = c(103, 112)
#'
#'
prepareCovariatesDataToPlotting <- function(preparedCovariatesData,
                                            cohortIds
){

  dataToPlot <- data.table::merge.data.table(x = subset(preparedCovariatesData,
                                                        cohort_id == cohortIds[2]),
                                             y = subset(preparedCovariatesData,
                                                        cohort_id == cohortIds[1]),
                                             by = c("covariate_id",
                                                  "database_id"),
                                             all = FALSE) %>% data.table::setDT()
    dataToPlot[,
               SMD := (mean.y - mean.x)/
                 sqrt(mean.y*(1-mean.y) +
                        (mean.x*(1-mean.x))/2)
              ]
}
