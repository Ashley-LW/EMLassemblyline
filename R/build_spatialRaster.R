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
  
  # Assign EML-compliant name - a manually determined translation of proj4str
  # Allowed values for EML seem to be enumerated here: 
  #  https://eml.ecoinformatics.org/schema/eml-spatialReference_xsd.html#SpatialReferenceType
  # Searching projection names at https://spatialreference.org is helpful
  # for the translation

  # Try and match proj4str from object to eml_horizCoordSysDef --------------
  # TODO: Do values outside the emlProjection invalidate the EML?
  # TODO: How were the enumerated emlProject values created?
  
  # browser()
  
  # ---------------------------------------------------------------------------

  # TODO: Add the EML spatial reference dictionary to EMLassemblyline as
  # view_spatial_reference_dictionary()
  
  # ---------------------------------------------------------------------------
  
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




#' Add proj4 and EPSG codes to the EML spatial reference dictionary
#' 
#' The EML spatial reference dictionary contains coordinate reference system 
#' (CRS) names that are loosely based on ESRI but are not common among other 
#' spatial software libraries. This function adds the exact ESRI names and 
#' corresponding proj4 and EPSG codes to improve metadata links.
#' 
#' @details
#'     This function uses spatialreference.org to translate between CRS names
#'     and codes.
#'     
#'     This function assumes ...
#' 
#' @note
#'     This function may not match all CRS names in the EML dictionary.
#'
#' @return
#'     (xml_document, xml_node) The EML spatial reference dictionary proj4 and
#'     EPSIG codes added when matches are available using the tags:
#'     \item{proj4}{The corresponding proj4 code}
#'     \item{epsg}{The corresponding EPSG code}
#'     
#' @export
#'
add_proj4_and_epsg_to_spatialReferenceDictionary <- function() {
  
  # Read the EML spatial reference dictionary
  dict <- xml2::read_xml(
    "https://raw.githubusercontent.com/NCEAS/eml/master/eml-spatialReferenceDictionary.xml")
  
  # Translate ESRI codes in spatialreference.org to proj4
  esri_proj4 <- esri_to_proj4()
  
  # Get PROJCS (a format variant of EML horizCoordSys names) and GEOGCS from EPSG codes
  epsg_proj4 <- rgdal::make_EPSG()
  epsg_proj4 <- epsg_proj4[!is.na(epsg_proj4$code), ]
  test <- lapply(
    epsg_proj4$code,
    function(x) {
      message("Getting GEOGCS and DATUM for EPSG code ", x)
      r <- httr::GET(
        url = paste0("https://spatialreference.org/ref/epsg/", x, "/esriwkt/"))
      txt <- httr::content(r, as = 'text', encoding = 'UTF-8')
      c(
        epsg_code = x,
        projcs = stringr::str_remove_all(
          stringr::str_extract(txt, "(?<=PROJCS\\[).*(?=,GEOGCS)"), "\""),
        geogcs = stringr::str_remove_all(
          stringr::str_extract(txt, "(?<=GEOGCS\\[).*(?=,DATUM)"), "\""),
        datum = stringr::str_remove_all(
          stringr::str_extract(txt, "(?<=DATUM\\[).*(?=,SPHEROID)"), "\""))
    })
  
  df <- data.table::rbindlist(test)
  df$epsg_code <- epsg_proj4$code
  # TODO: Are all ref systems of EML in df?
  use_i <- eml_horizCoordSysDef$horizCoordSysDef %in% df$GEOGCS
  use_i <- df$GEOGCS %in% eml_horizCoordSysDef$horizCoordSysDef
  sum(use_i)

  
}






#' Get map between ESRI Coordinate System Names and proj4 codes from spatialreference.org
#'
#' @return
#'     A data frame wtih columns:
#'     \item{esri}{ESRI Coordinate System Names}
#'     \item{proj4}{proj4 codes}
#' 
#' @export
#'
esri_to_proj4 <- function() {
  message("Searching spatialreference.org")
  output <- list()
  n <- 1
  continue <- TRUE
  while (isTRUE(continue)) {
    r <- httr::GET(
      url = paste0("https://spatialreference.org/ref/esri/?page=", n))
    txt <- httr::content(r, as = 'parsed', encoding = 'UTF-8')
    if (any(xml2::xml_text(xml2::xml_find_all(txt, ".//p")) == 
            "No results found.")) {
      continue <- FALSE
    } else {
      esri_proj4 <- lapply(
        xml2::xml_text(xml2::xml_find_all(txt, ".//li")),
        function(x) {
          code <- stringr::str_extract(x, "(?<=:).*(?=:)")
          name <- stringr::str_extract(x, "(?<=:[:blank:]).*")
          r <- httr::GET(
            url = paste0(
              "https://spatialreference.org/ref/esri/", code, "/proj4/"))
          txt <- httr::content(r, as = 'text', encoding = 'UTF-8')
          list(esri = name, proj4 = txt)
        })
      output <- c(output, esri_proj4)
      n <- n + 1
    }
  }
  output <- data.table::rbindlist(output)
  output[output$proj4 != "", ]
}






#' Recreate the EML spatialReferenceDictionary and add EPSG and proj4 codes
#'
#' @return
#'     (xml_document, xml_node) The EML spatialReferenceDictionary with EPSG
#'     and proj4 codes added
#'     
#' @details
#'     Because the horizCoordSysDef and geogCoordSys values have an arbitrary
#'     format, the other components of the CRS are used as a key and means 
#'     of validation.
#'     
#' @export
#'
create_spatialReferenceDictionary <- function() {
  
  epsg_proj4 <- rgdal::make_EPSG()
  epsg_proj4 <- epsg_proj4[!is.na(epsg_proj4$code), ]
  
  test <- lapply(
    epsg_proj4$code,
    function(x) {
      message("Getting GEOGCS and DATUM for EPSG code ", x)
      r <- httr::GET(
        url = paste0("https://spatialreference.org/ref/epsg/", x, "/esriwkt/"))
      txt <- httr::content(r, as = 'text', encoding = 'UTF-8')
      
      # Parse content ---------------------------------------------------------
      
      # horizCoordSysDef
      projcs <- stringr::str_extract(txt, '(?<=PROJCS\\[").*?(?=")')
      # geogCoordSys
      geogcs <- stringr::str_extract(txt, '(?<=GEOGCS\\[").*?(?=")')
      # datum
      datum <- stringr::str_extract(txt, '(?<=DATUM\\[").*?(?=")')
      # spheroid
      spheroid <- unlist(stringr::str_split(
        stringr::str_extract(txt, '(?<=SPHEROID\\[).*?(?=\\])'), ","))
      spheroid_name <- stringr::str_remove_all(spheroid[1], '\"')
      spheroid_semiAxisMajor <- spheroid[2]
      spheroid_denomFlatRatio <- spheroid[3]
      # primeMeridian
      primem <-  unlist(stringr::str_split(
        stringr::str_extract(txt, '(?<=PRIMEM\\[).*?(?=\\])'), ","))
      primem_name <- stringr::str_remove_all(primem[1], '\"')
      primem_longitude <- primem[2]
      # unit
      unit <- unlist(stringr::str_split(
        stringr::str_extract(txt, '(?<=UNIT\\[).*?(?=\\])'), ","))
      unit_name <- stringr::str_to_lower(
        stringr::str_remove_all(unit[1], '\"'))
      
      # Construct XML ---------------------------------------------------------
      # Translate to EML spatial reference dictionary .xml. This is a best attempt 
      # because rules for constructing EML  horizCoordSysDef, geogCoordSys
      # and dataum appear a bit arbitrary and unclear.
      
    })
  
}