context('Validate arguments')
library(EMLassemblyline)

testthat::test_that("make_eml()", {
  
  # make_eml() ----------------------------------------------------------------
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
  x$x$template$taxonomic_coverage.txt <- NULL
  
  x$data.path <- system.file('/examples/pkg_260/data_objects', package = 'EMLassemblyline')
  x$data.table <- c("decomp.csv", "nitrogen.csv")
  x$data.table.name <- c("Decomp file name", "Nitrogen file name")
  x$data.table.description <- c("Decomp file description", "Nitrogen file description")
  x$data.table.quote.character  <- c("\\'", "\\'")
  x$data.table.url <- c("https://url/to/decomp.csv", "https://url/to/nitrogen.csv")
  x$dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
  x$eml.path <- system.file('/examples/pkg_260/eml', package = 'EMLassemblyline')
  x$geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
  x$geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
  x$maintenance.description <- 'Completed'
  x$other.entity <- c("ancillary_data.zip", "processing_and_analysis.R")
  x$other.entity.name <- c("ancillary_data file name", "processing_and_analysis file name")
  x$other.entity.description <- c("ancillary_data file description", "processing_and_analysis file description")
  x$other.entity.url <- c("https://url/to/ancillary_data.zip", "https://url/to/processing_and_analysis.R")
  x$package.id <- "edi.100.1"
  x$path <- system.file('/examples/pkg_260/metadata_templates', package = 'EMLassemblyline')
  x$provenance <- NULL
  x$return.obj <- T
  x$temporal.coverage <- c('2014-05-01', '2015-10-31')
  x$user.domain <- c("EDI", "LTER")
  x$user.id <- c("userid1", "userid2")
  x$write.file <- F
  
  # dataset.title - Warn if missing

  x1 <- x
  x1$dataset.title <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "A dataset title is required")
  
  # data.table.description - Warn if missing
  
  x1 <- x
  x1$data.table.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Data table descriptions are recommended")
  
  # data.table.description - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.description <- x1$data.table.description[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table descriptions are missing")
  
  # data.table.description - When length doesn't match data.table undescribed
  # tables will not have a description
  
  x1 <- x
  x1$data.table.description <- x1$data.table.description[1]
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(!is.na(r$dataset$dataTable[[1]]$entityDescription))
  expect_true(is.na(r$dataset$dataTable[[2]]$entityDescription))
  
  # data.table.name - Warn if using defaults
  
  x1 <- x
  x1$data.table.name <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0(
      "Data table names are missing. A short name for each data table is ",
      "recommended. Defaulting to data table file names."))
  
  # data.table.name - Warn if length doesn't match data.table

  x1 <- x
  x1$data.table.name <- x1$data.table.name[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0("One or more data table names are missing. Defaulting to ",
                    "data table file names"))
  
  # data.table.quote.character - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.quote.character <- x1$data.table.quote.character[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table quote characters are missing.")
  
  # data.table.url - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.url <- x1$data.table.url[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table URLs are missing.")

  # geographic.corrdinates - Expected when using geographic.description 
  
  x1 <- x
  x1$geographic.coordinates <- NULL
  x1$x$template$geographic_coverage.txt <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Geographic coordinates are missing.")
  
  # geographic.description - Expected when using geographic.coordinates
  
  x1 <- x
  x1$geographic.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Geographic description is missing.")

  # maintenance.description
  
  x1 <- x
  x1$maintenance.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = 'A maintenance description is recommended.')

  # other.entity.description - Warn if missing
  
  x1 <- x
  x1$other.entity.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Other entity descriptions are recommended")
  
  # other.entity.description - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.description <- x1$other.entity.description[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more other entity descriptions are missing")

  # other.entity.name - Warn if using defaults
  
  x1 <- x
  x1$other.entity.name <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0(
      "Other entity names are missing. A short name for each other entity is ",
      "recommended. Defaulting to other entity file names."))
  
  # other.entity.name - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.name <- x1$other.entity.name[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0("One or more other entity names are missing. Defaulting to ",
                    "other entity file names"))
  
  # other.entity.url - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.url <- x1$other.entity.url[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more other entity URLs are missing.")
  
  # package.id - If malformed EDI or LTER package ID then warning.
  
  x1 <- x
  x1$package.id <- "knb-lter-ntl.5"
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Warning: 'package.id' is not valid for EDI or LTER. Expected ")
  
  # provenance
  
  x1 <- x
  x1$provenance <- c("edi.349.1", "knb-lter-ntl.3.28", "knbee-lter-ntl.3.28")
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Unable to generate provenance metadata for unrecognized identifier")
  
  # temporal.coverage - Warn if missing
  
  x1 <- x
  x1$temporal.coverage <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Temporal coverage is recommended")
  
  # temporal.coverage - Warn if missing start or end date
  
  x1 <- x
  x1$temporal.coverage <- x1$temporal.coverage[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Temporal coverage requires a begin and end date")

  # write.file - eml.path is required
  
  x1 <- x
  x1$path <- NULL
  x1$eml.path <- NULL
  x1$write.file <- TRUE
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
})
