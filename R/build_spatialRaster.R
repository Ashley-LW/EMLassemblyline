#' Build spatialRaster node
#'
#' @param spatial.raster
#'     (character) Full file path
#' @param spatial.raster.name 
#'     (character) Name of \code{spatial.raster}
#' @param spatial.raster.description 
#'     (character) Description of \code{spatial.raster}
#' @param spatial.raster.geographic.description 
#'     (character) Raster geographic coverage
#' @param rasterAttributes 
#'     (data.frame) Attributes table
#' @param rasterFactors 
#'     (data.frame) Factor codes table
#'
#' @return
#'     (emld; list) EML spatialRaster node
#' 
#' @export
#'
#' @examples
#' raster.attribs <- data.frame(
#'   attributeName = "cover_value",
#'   attributeDefinition = 'Shrub cover fraction (0-1)',
#'   unit = 'dimensionless',
#'   numberType = 'real',
#'   missingValueCode = 0,
#'   missingValueCodeExplanation = 'No data')
#' 
#' # (NOTE: These metadata don't pertain to this .tif)
#' covercode <- c('0' = "no data", '1' = "non-shrub", '2' = "shrub")
#' raster.factor <- data.frame(
#'   attributeName = "cover_code",
#'   code = names(covercode),
#'   definition = unname(covercode))
#' 
#' build_spatialRaster(
#'   spatial.raster = "C:/Users/Colin/Documents/EDI/data_sets/fix_15/data_objects/shrub_cover.tif", 
#'   spatial.raster.description = "1-hectare JER-CDRRC shrub cover geotiff (cover fraction from 0 to 1)", 
#'   spatial.raster.geographic.description = "JER and CDRRC, southern New Mexico", 
#'   rasterAttributes = raster.attribs, 
#'   rasterFactors = raster.factor,
#'   baseURL = "https://jornada-ldc2.jrn.nmsu.edu/tif/")
#' 
build_spatialRaster <- function(
  spatial.raster,
  spatial.raster.name,
  spatial.raster.description,
  spatial.raster.geographic.description,
  rasterAttributes,
  rasterFactors = NULL){
  
  # Read data -----------------------------------------------------------------
  # TODO: Move to template_arguments()
  
  rasterObject <- raster::raster(spatial.raster)
  
  # Get spatial reference and convert to EML standard--------------------------
  
  # First read the proj4 string
  proj4str <- raster::crs(rasterObject, asText = T)
  print(paste('Spatial reference in proj4 format is:', proj4str))
  
  # Assign EML-compliant name - a manually determined translation of proj4str
  # Allowed values for EML seem to be enumerated here: 
  #  https://eml.ecoinformatics.org/schema/eml-spatialReference_xsd.html#SpatialReferenceType
  # Searching projection names at https://spatialreference.org is helpful
  # for the translation

  # Try and match proj4str from object to eml_horizCoordSysDef --------------
  # TODO: Do values outside the emlProjection invalidate the EML?
  # TODO: How were the enumerated emlProject values created?
  
  # Get EML coordinate list
  eml_horizCoordSysDef <- get_coord_list()
  
  # browser()
  
  # TODO: Add this to template_spatial_raster_attributes() so the ESRIWKT
  # value can be manually added if spatialreference.org is offline
  #
  # Get mapping between EPSG projection codes and proj4
  epsg <- rgdal::make_EPSG()
  # Get EPSG for proj4
  use_i <- !is.na(epsg$prj4) & (epsg$prj4 == proj4str)
  # Convert EPSG to ESRIWKT
  r <- httr::GET(
    url = "https://spatialreference.org/ref/epsg/4326/esriwkt/")
  output <- httr::content(r, as = 'text', encoding = 'UTF-8')
  # Extract GEOGCS from ESRIWKT

  proj <- rgdal::projInfo("proj")
  datum <- rgdal::projInfo("datum")
  ellps <- rgdal::projInfo("ellps")

  if (proj4str=="+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"){
    emlProjection <- "NAD_1983_UTM_Zone_13N"
  }
  print(paste("Translated to", emlProjection, 'in EML spatialReference schema'))
  
  
  # Determine coverage (bbox) of raster ---------------------------------------
  
  # For EML, this apparently needs to be in decimal degrees, so convert
  # to SpatialPolygons and then reproject with spTransform
  extent <- as(raster::extent(rasterObject), "SpatialPolygons")
  sp::proj4string(extent) <- proj4str
  extent.geo <- sp::spTransform(extent, CRS("+proj=longlat +datum=WGS84 +no_defs 
                                            +ellps=WGS84 +towgs84=0,0,0"))
  print('Determining spatial coverage...')
  spatialCoverage <- EML::set_coverage(geographicDescription = spatial.raster.geographic.description,
                                       west = extent.geo@bbox["x", "min"],
                                       east = extent.geo@bbox["x", "max"],
                                       north = extent.geo@bbox["y", "max"],
                                       south = extent.geo@bbox["y", "min"])
  
  
  # projections----------------------------------------------------------------
  projections <- list(section = list(
    paste0("<title>Raster derived coordinate reference system</title>\n<para>",
           proj4str, "</para>")
  ))
  
  
  # Create attributes table----------------------------------------------------
  # EML schema reference:
  # https://eml.ecoinformatics.org/eml-schema.html#the-eml-attribute-module---attribute-level-information-within-dataset-entities
  # EML R package reference: 
  # https://ropensci.github.io/EML/articles/creating-EML.html#attribute-metadata
  #
  # Must provide attributes table, if raster has factors add a factors table
  print('Building attributes...')
  
  # If rasterFactors is not NULL
  if (!is.null(rasterFactors)){
    # Build attribute list with rasterAttributes and rasterFactors
    # rasterAttributes must contain:
    #     attributeName
    #     attriuteDefinition
    # rasterFactors enumerates each code and its definition for attributeName
    attr_list <- EML::set_attributes(attributes=rasterAttributes,
                                     factors=rasterFactors, 
                                     col_classes = "factor")
    
  } else {  # If factorTable IS NULL
    # Build attribute list with rasterAttributes
    # rasterAttributes (for numeric data) must contain:
    #     attributeName
    #     attriuteDefinition
    #     unit (such as "dimensionless")
    #     numberType ('real', 'natural', 'integer', 'whole')
    #
    # CAP's capeml has a way to determine raster value number type
    # see here: https://github.com/CAPLTER/capeml/blob/master/R/create_spatialRaster.R
    #
    # This will fail if the units are not appropriate
    attr_list <- EML::set_attributes(attributes=rasterAttributes,
                                     col_classes="numeric")
  }
  
  
  # set authentication (md5)---------------------------------------------------
  print('Calculating MD5 sum...')
  fileAuthentication <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(spatial.raster)
  
  
  # set file size--------------------------------------------------------------
  fileSize <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(spatial.raster))
  
  
  # set file format------------------------------------------------------------
  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat=EML::eml$externallyDefinedFormat(
      formatName=file_ext(spatial.raster))
  )
  
  # Create rasterPhysical pieces-----------------------------------------------
  rasterBaseName <- basename(spatial.raster)
  directoryName <- dirname(spatial.raster)
  directoryNameFull <- sub("/$", "", path.expand(directoryName))
  pathToFile <- path.expand(spatial.raster)
  
  # set distribution
  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(baseURL, rasterBaseName))
  )
  
  # build physical
  spatialRasterPhysical <- EML::eml$physical(
    objectName = rasterBaseName,
    authentication = fileAuthentication,
    size = fileSize,
    dataFormat = fileDataFormat,
    distribution = fileDistribution
  )
  
  
  # build spatialRaster--------------------------------------------------------
  print('Building spatialRaster entity...')
  newSR <- EML::eml$spatialRaster(
    entityName = rasterBaseName,
    entityDescription = spatial.raster.description,
    physical = spatialRasterPhysical,
    coverage = spatialCoverage,
    additionalInfo = projections,
    attributeList = attr_list,
    spatialReference = EML::eml$spatialReference(
      horizCoordSysName = emlProjection),
    numberOfBands = bandnr(rasterObject),
    rows = nrow(rasterObject),
    columns = ncol(rasterObject),
    horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport="Unknown"), # Add field in template
    verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport="Unknown"), # Add field in template
    cellSizeXDirection = xres(rasterObject),
    cellSizeYDirection = yres(rasterObject),
    rasterOrigin = "Upper Left",
    verticals = 1,
    cellGeometry = "pixel",
    id = rasterBaseName
  )
  
  return(newSR)
}



# Jeanette Clark's helpers ----------------------------------------------------

#' Get list of EML Coordinate Reference Systems
#'
#' Get a data.frame of EML coordinate reference systems that can
#' be searched and filtered more easily than the raw XML file.
#'
#' @export
#'
get_coord_list <- function(){
  geo_list <- EML::read_eml("https://raw.githubusercontent.com/NCEAS/eml/4417cbf6588fdca4e06bd67190a9d7a18a8e944f/eml-spatialReferenceDictionary.xml")
  coord_df <- data.frame(horizCoordSysDef = rep(NA, length(geo_list$horizCoordSysDef)),
                         geogCoordSys = rep(NA, length(geo_list$horizCoordSysDef)),
                         projection = rep(NA, length(geo_list$horizCoordSysDef)),
                         datum = rep(NA, length(geo_list$horizCoordSysDef)),
                         proj_unit = rep(NA, length(geo_list$horizCoordSysDef)))
  for (i in 1:length(geo_list$horizCoordSysDef)){
    coord_df$horizCoordSysDef[i] <- geo_list$horizCoordSysDef[[i]]$name
    if (!is.null(geo_list$horizCoordSysDef[[i]]$projCoordSys)){
      coord_df$geogCoordSys[i]  <- geo_list$horizCoordSysDef[[i]]$projCoordSys$geogCoordSys$name
      coord_df$datum[i]  <- geo_list$horizCoordSysDef[[i]]$projCoordSys$geogCoordSys$datum$name
      coord_df$projection[i] <- geo_list$horizCoordSysDef[[i]]$projCoordSys$projection$name
      coord_df$proj_unit[i] <- geo_list$horizCoordSysDef[[i]]$projCoordSys$projection$unit$name
    } else {
      coord_df$geogCoordSys[i]  <- geo_list$horizCoordSysDef[[i]]$geogCoordSys$name
      coord_df$datum[i]  <- geo_list$horizCoordSysDef[[i]]$geogCoordSys$datum$name
      coord_df$projection[i] <- NA
      coord_df$proj_unit[i] <- NA
    }
  }
  return(coord_df)
}

#' Get raster info from a file on disk
#'
#' This function populates a spatialRaster element with the
#' required elements by reading a local raster file in. The
#' `coord_name` argument can be found by examining the data.frame
#' that `get_coord_list()` returns against the proj4string of the
#' raster file.
#'
#' @param path (char) Path to a raster file
#' @param coord_name (char) horizCoordSysDef name
#' @param attributes (data.frame) attribute list as a data.frame for raster
#'
#'
#' @export
eml_get_raster_metadata <- function(path, coord_name = NULL, attributes){
  raster_obj <- raster::raster(path)
  
  message(paste("Reading raster object with proj4string of ", raster::crs(raster_obj)@projargs))
  if (is.null(coord_name)){
    coord_name <- raster::crs(raster_obj)@projargs
  }
  if (identical(raster::origin(raster_obj), c(0,0))){
    raster_orig <- "Upper Left"
  } else if(!identical(raster::origin(raster_obj), c(0,0))){
    message("Raster origin not at 0,0")
    raster_orig <- "unknown"
  }
  raster_info <- list(entityName = basename(path),
                      attributeList = set_attributes(attributes),
                      spatialReference = list(horizCoordSysName = coord_name),
                      horizontalAccuracy = list(accuracyReport = "unknown"),
                      verticalAccuracy = list(accuracyReport = "unknown"),
                      cellSizeXDirection = raster::res(raster_obj)[1],
                      cellSizeYDirection = raster::res(raster_obj)[2],
                      numberOfBands = raster::nbands(raster_obj),
                      rasterOrigin = raster_orig,
                      rows = dim(raster_obj)[1],
                      columns = dim(raster_obj)[2],
                      verticals = dim(raster_obj)[3],
                      cellGeometry = "pixel")
  return(raster_info)
}