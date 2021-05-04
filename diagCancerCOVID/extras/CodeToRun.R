# Make sure to install all dependencies (not needed if already done):
# install.packages("SqlRender")
# install.packages("DatabaseConnector")
# install.packages("ggplot2")
# install.packages("ParallelLogger")
# install.packages("readr")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("RJSONIO")
# install.packages("devtools")
# devtools::install_github("FeatureExtraction")
# devtools::install_github("ROhdsiWebApi")
# devtools::install_github("CohortDiagnostics")


# Load the package
library(diagCancerCOVID)

# Optional: specify where the temporary files will be created:
options(andromedaTempFolder = "C/andromedaTemp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()


# Details for connecting to the server:
# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
#                                                                 server = Sys.getenv("PDW_SERVER"),
#                                                                 user = NULL,
#                                                                 password = NULL,
#                                                                 port = Sys.getenv("PDW_PORT"))

oracleTempSchema <- NULL
cdmDatabaseSchema <- "omop20v2"
cohortDatabaseSchema <- "results20v2"
cohortTable <- "CovidMultiStateCohortsCancer"
databaseId <- "SIDIAP"
databaseName <- "SIDIAP"
databaseDescription <- "SIDIAP"

# Use this to run the cohorttDiagnostics. The results will be stored in the diagnosticsExport subfolder of the outputFolder. This can be shared between sites.
diagCancerCOVID::runCohortDiagnostics(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     oracleTempSchema = oracleTempSchema,
                                     outputFolder = outputFolder,
                                     databaseId = databaseId,
                                     databaseName = databaseName,
                                     databaseDescription = databaseDescription,
                                     createCohorts = TRUE,
                                     runInclusionStatistics = TRUE,
                                     runIncludedSourceConcepts = TRUE,
                                     runOrphanConcepts = FALSE,
                                     runTimeDistributions = FALSE,
                                     runBreakdownIndexEvents = TRUE,
                                     runIncidenceRates = FALSE,
                                     runCohortOverlap = TRUE,
                                     runCohortCharacterization = FALSE,
                                     runTemporalCohortCharacterization = FALSE,
                                     minCellCount = 5)

# To view the results:
# Optional: if there are results zip files from multiple sites in a folder, this merges them, which will speed up starting the viewer:
CohortDiagnostics::preMergeDiagnosticsFiles(file.path(outputFolder, "diagnosticsExport"))

# Use this to view the results. Multiple zip files can be in the same folder. If the files were pre-merged, this is automatically detected: 
CohortDiagnostics::launchDiagnosticsExplorer(file.path(outputFolder, "diagnosticsExport"))

