# Instructions for the EMLassemblyline

### Overview

Below are instructions for operating the `EMLassemblyline`. We recommend scripting your workflow and re-installing `EMLassemblyline` periodically to expedite revisioning and ensure the latest features are available.

### Contents:

* [Install](#install)
* [Create directories](#create-working-directory)
* [Select intellectual rights license](#select-intellectual-rights-license)
* [Identify data types](#identify-data-types)
* [Import templates](#import-templates)
* [Abstract](#abstract)
* [Methods](#methods)
* [Additional information](#additional-information)
* [Keywords](#keywords)
* [Personnel](#personnel)
* [Attributes](#attributes)
* [Categorical variables](#categorical-variables)
* [Geographic coverage](#geographic-coverage)
* [Make EML](#make-eml)
* [Upload you data package](#upload-your-data-package)



## Install
[back to top](#contents)

`EMLassemblyline` is under constant revision and improvement. Please reinstall the R package periodically to ensure the latest functionality is available to you. Installation from GitHub requires the `devtools` package.

```
# Install devtools
install.packages("devtools")

# Load devtools
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
```

__NOTE: EML Version 1.99.0 breaks EMLassemblyline__ We are upgrading `EMLassemblyline` to work with the newly refurbished `EML` library. If you receive an error when installing `EMLassemblyline`, reinstall `EML` version 1.0.3 following these instructions:

```
# For MacOS
package_url <- 'https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.5/EML_1.0.3.tgz'
install.packages(package_url, repos = NULL, type = 'source')

# For Windows
package_url <- 'https://cran.r-project.org/bin/windows/contrib/3.5/EML_1.0.3.zip'
install.packages(package_url, repos = NULL, type = 'source')
```


## Create directories
[back to top](#contents)

Create directories to store your dataset, metadata templates created and used by `EMLassemblyline`, and for export of EML from `EMLassemblyline`. These files can be co-located in a single directory or organized in their own respective directories. Either way, start by creating a directory for your metadata templates. Name this directory after your dataset. Replace spaces with underscores (e.g. `name of your directory` should be `name_of_your_directory`).

Move copies of the final versions of your data tables into this directory or store them in their own separate directory. These tables should be the final versions of the data you are ready to publish.

Rename these files following these rules:

* replace symbols with words
* replace parentheses with underscores
* replace periods with underscores
* replace blank spaces with underscores

e.g. `name.of.(your) d@t@.file` should be `name_of_your_data_file`


## Select intellectual rights license
[back to top](#contents)

There are 2 options for intellectual rights licenses:

1. __CC0__ the most accomodating of data reuse ... This data package is released to the “public domain” under Creative Commons CC0 1.0 “No Rights Reserved” (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein “website”) in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available “as is” and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you.

2. __CCBY__ requires attribution ... This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data ("Data User" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available "as is." The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.


## Identify data types
[back to top](#contents)

`EMLassemblyline` works for tabular data files and .zip directories. Other formats will be supported in the future.

#### table

A flat file composed of columns containing variables and rows containing observations. Column names must follow these rules:

* replace symbols with words
* replace parentheses with underscores
* replace periods with underscores
* replace blank spaces with underscores

e.g. `land.cover.use (%)` should be `percent_land_cover_use`

#### .zip directory

A .zip directory containing anything you want to put in it. .zip directory name should follow the same naming rules as for a table.

## Import templates
[back to top](#contents)

Run the function `import_templates` in the RStudio Console to populate the metadata directory with template files for you to complete. You will need to supply a few arguments to this function:

1. **path** A path for the directory to which metadata templates will be imported.
2. **data.path** A path to the directory containing your data entities. The default value is set to "path", i.e. the location to which your metadata templates will be imported.
3. **license** The license for your dataset ("CC0" or "CCBY").
4. **data.files** A list of the data tables of your dataset. File extension is not required. Do not include .zip directories here, they will be added in the `make_eml` step.

```
# Load EMLassemblyline
library(EMLassemblyline)

# Import templates for an example dataset licensed under CC0, with 2 tables located in at "path"
import_templates(path = "/Users/csmith/Desktop/gleon_chloride",
                 license = "CC0",
                 data.files = c("lake_chloride_concentrations",
                                "lake_characteristics"))
                                
# Import templates for data stored in a different directory ("data.path") than where the templates will be imported to
import_templates(path = "/Users/csmith/Desktop/gleon_chloride",
                 data.path = "/Users/csmith/Desktop/gleon_chloride/data",
                 license = "CC0",
                 data.files = c("lake_chloride_concentrations",
                                "lake_characteristics"))
                                
```


## Abstract
[back to top](#contents)

Open the file `abstract.txt` and write an abstract for your dataset. The abstract should cover what, why, when, where, and how for your dataset. Write your abstract in plain text.

Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable). The reason for this is that the EML schema only allows characters that are apart of the unicode character set. 

NOTE: You can create your abstract in Microsoft Word and then copy over to `abstract.txt` but first you will need to remove any non-unicode characters. To do this go to [this web service](http://utils.paranoiaworks.org/diacriticsremover/) and paste your abstract into the window. Click the button "Remove Diacritics" to remove these non-compliant characters, then copy the resultant text into `abstract.txt`. You will want to give your abstract one last look over after performing this operation to ensure no information has been lost.


## Methods
[back to top](#contents)

Open the file `methods.txt` and describe the methods for your dataset. Be specific, include instrument descriptions, or point to a protocol online. If this dataset is a synthesis of other datasets please specify dataset origins, preferably their DOI or URL plus general citation information. 

Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable). The reason for this is that the EML schema only allows characters that are apart of the unicode character set. 

NOTE: You can create your methods in Microsoft Word and then copy over to `methods.txt` but first you will need to remove any non-unicode characters. To do this go to [this web service](http://utils.paranoiaworks.org/diacriticsremover/) and paste your methods into the window. Click the button "Remove Diacritics" to remove these non-compliant characters, then copy the resultant text into `methods.txt`. You will want to give your methods one last look over after performing this operation to ensure no information has been lost.


## Additional information
[back to top](#contents)

`additional_info.txt` is a good place for text based information about your dataset that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset). If you have this information and would like to share it then open `additional_info.txt` in a text editor and add it. You can delete this file if you won't be using it, or you can keep it around in case you change your mind.

Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable). The reason for this is that the EML schema only allows characters that are apart of the unicode character set. 

NOTE: You can create your additional information in Microsoft Word and then copy over to `additional_info.txt` but first you will need to remove any non-unicode characters. To do this go to [this web service](http://utils.paranoiaworks.org/diacriticsremover/) and paste your additional information into the window. Click the button "Remove Diacritics" to remove these non-compliant characters, then copy the resultant text into `additional_info.txt`. You will want to give your additional information one last look over after performing this operation to ensure no information has been lost.


## Keywords
[back to top](#contents)

Open the tab delimited file `keywords.txt` in a spreadsheet editor and list the keywords that best describe your dataset. DO NOT edit this file in a text editor. [Consult the LTER controlled vocabulary](http://vocab.lternet.edu/vocab/vocab/index.php) for keywords and/or the associated [keywords distiller](http://vocab.lternet.edu/keywordDistiller/) to mine your abstract or methods for keywords. If you already have a list of keywords, [use the `validate_keywords` function](https://github.com/EDIorg/EMLassemblyline/blob/master/documentation/validate_keywords.md) to validate against a controlled vocabulary. In addition to keywords describing the data, you may want to include keywords that describe your lab, station, and project (e.g. OBFS, LTREB, etc.).

Definitions for columns of this file:

* **keyword** A keyword describing your dataset.
* **keywordThesaurus** A keywordThesaurus (i.e. a controlled vocabulary like the resource listed above) corresponding to the keyword listed in the keyword column. If the keyword is not from a thesaurus or controlled vocabulary, leave corresponding entry in the keywordThesaurus column blank.

## Personnel
[back to top](#contents)

Open the tab delimited file `personnel.txt` in a spreadsheet editor and enter information about the personnel associated with this dataset.

Definitions for columns of this file:

* **givenName** First name of person.
* **middleInitial** Middle initial of person.
* **surName** Last name of person.
* **organizationName** Name of organization the person is associated with.
* **electronicMailAddress** Email address of person.
* **userId** ORCID of person (not required). A valid entry for userId is the 16 digit ORCID number separated by dashes (i.e. XXXX-XXXX-XXXX-XXXX). An ORCID is like a social security number for scientists and links your dataset with your ORCID. [Create one here](https://orcid.org/).
* **role** Role of person with respect to this dataset. Valid entries for role are:
    + **creator** The creator is considered to be the author of the dataset, i.e. the person(s_ responsible for intellectual input into its creation (required; at least 1 creator must be listed for your dataset). 
    + **PI** Principal investigator associated with this dataset (not required). Often the PI should also be listed as the dataset creator.
    + **contact** Dataset contact (required; at least 1 contact must be listed for your dataset). The contact may be a person or a position at an organization. We recommend listing the contact as a person rather than a position. To list a position as a contact (e.g. Data Manager), Enter the position name in the `givenName` column and leave `middleInitial` and `surName` blank.
    + Any other entries into the 'role' column are acceptable and will be defined under the associated party element of this dataset with whatever value is entered under role.
    + If a person serves more than one role, duplicate this persons information in another row but with the additional role.
    + Similarly if a role is shared among many people, list the individuals with the shared role on separate lines.
* **projectTitle** Title of the project this dataset was created under (optional). Project titles are only listed on lines where the personnel role is PI. If an auxiliary project was involved in creating this dataset then add a new row below the row containing the primary project and list the project title and associated PI. Do this for each auxiliary project.
* **fundingAgency** Name of the entity funding the creation of this dataset (optional). Only include an entry in this column for rows where role PI.
* **fundingNumber** Number of the grant or award that supported creation of this dataset (optional). Only include an entry in this column for rows where role PI.


## Attributes
[back to top](#contents)
    
An `attributes_datatablename.txt` file has been created for each of your data tables. Edit each of these tab delimited files in a spreadsheet editor. DO NOT edit this file in a text editor. You will see this file has been partially populated with information detected by the `import_templates` function. You will have to double check values listed in all the columns except `attributeName`. 

Instructions for completing the attribute table are as follows:

* **attributeName** Enter attribute names (i.e. column names) as they appear in the data table and in the same order as listed in the data table.
* **attributeDefinition** Enter definitions for each attribute. Be specific, it can be lengthy.
* **class** Enter the attribute class. This is the type of value stored under the attribute. Valid options for class are:
    + **numeric** For numeric variables.
    + **categorical** For categorical variables.
    + **character** For variables containing text or symbols that are not categorical.
    + **Date** For date time variables.
    + If an attribute has class of `numeric` or `Date`, then all values of this attribute must be either numeric or date time. If any character strings are present in an otherwise `numeric` attribute, this attribute must be classified as `character`. Similarly if any values of a "Date" attribute do not match the date time format string (details below), then this attribute must be classified as `character`.

* **unit** If an attributes class is numeric, then you must provide units. If the attribute is numeric but does not have units, enter `dimensionless`. If the attribute class is categorical, character, or Date then leave the unit field blank. If the attribute is numeric and has units search the standard unit dictionary for the unit of interest and enter the unit `name` as it appears in the dictionary (unit names are case sensitive). Open the dictionary by running these lines of code in the RStudio console window:

```
# View and search the standard units dictionary
view_unit_dictionary()

```
* If you cannot find a unit in the dictionary, create one and add it to `custom_units.txt`. Open this tab delimited file in a spreadsheet editor. DO NOT edit this file in a text editor. If you have no custom units to report you may delete this file, but may also keep it around if you think it may be of future use. Valid custom units must be convertible to SI Units (i.e. International System of Units). If it cannot be converted to SI then list it in the attribute defintion and enter "dimensionless" in the unit field. To create a custom unit define the:
    + **id** This is equivalent to the unit name. 
    + **unitType** The type of unit being defined. Reference the dictionary for examples.
    + **parentSI** The SI equivalent of the id you have entered.
    + **multiplierToSI** This is the multiplier to convert from your custom unit to the SI unit equivalent.
    + **description** A description of the custom unit. Reference the dictionary for examples.
    
* **dateTimeFormatString** Enter the date time format string for each attribute of "Date" class. Remember, a class of "Date" specifies the attribute as a date, time, or datetime. Enter the format string in this field. If the attribute class is not "Date", leave this field blank. Below are rules for constructing format strings.
    + **year** Use `Y` to denote a year string (e.g. 2017 is represented as `YYYY`).
    + **month** Use `M` to denote a month string (e.g. 2017-05 is represented as `YYYY-MM`).
    + **day** Use `D` to denote a day string (e.g. 2017-05-09 is represented as `YYYY-MM-DD`).
    + **hour** Use `h` to denote a hour string (e.g. 2017-05-09 13 is represented as `YYYY-MM-DD hh`).
    + **minute** use `m` to denote a minute string (e.g. 2017-05-09 13:15 is represented as `YYYY-MM-DD hh:mm`).
    + **second** use `s` to denote a second string (e.g. 2017-05-09 13:15:00 is represented as `YYYY-MM-DD hh:mm:ss`).
    + **Time zone format strings:** use `+` or `-` along with a time string to denote time zone offsets (e.g. `2017-05-09 13:15:00+05:00` is represented as `YYYY-MM-DD hh:mm:ss+hh:mm`).
* **missingValueCode** If a code for 'no data' is used, specify it here (e.g. NA, -99999, etc.). Only one missingValueCode is allowed for a single attribute.
* **missingValueCodeExplanation** Define the missing value code here.
    

## Categorical variables
[back to top](#contents)

If your data tables contain any attributes with the categorical class, you will need to supply definitions for the categorical codes. Use the function `define_catvars` to do this. `define_catvars` searches through each attribute file looking for attributes with a categorical class. If found, the function extracts unique categorical codes for each attribute and writes them to a file for you to define.

```
# Define categorical variables when data and metadata templates are co-located at path
define_catvars(path = "/Users/csmith/Desktop/gleon_chloride")

# Define categorical variables when data and metadata templates are located in different directories
define_catvars(path = "/Users/csmith/Desktop/gleon_chloride",
               data.path = "/Users/csmith/Desktop/gleon_chloride/data")

```

A tab delimited __catvars_datatablename.txt__ will be created for each of your data tables containing categorical variables. Open these in a spreadsheet editor and add definitions for each code.

## Geographic coverage
[back to top](#contents)

If your dataset contains more than one sampling point or area, then you will want to add this information to your metadata. Often a data user will search for data within a geographic area.

#### Geographic points

Run the function `extract_geocoverage` to get the unique latitude, longitude, and site name combinations from your data and write to file. `extract_geocoverage` requires specific inputs that may require altering the latitude and longitude formate of your data. See documenation for details.

Arguments required by this function are:

1. **path** A path to the metadata directory.
2. **data.path** A path to the directory containing the data table with geographic information. Don't use this argument if the data table is located at the path argument listed above.
3. **data.file** Name of the input data table containing geographic coverage data.
4. **lat.col** Name of latitude column. Values of this column must be in decimal degrees. Latitudes south of the equator must be prefixed with a minus sign (i.e. dash, "-").
5. **lon.col** Name of longitude column. Values of this column must be in decimal degrees. Longitudes west of the prime meridian must be prefixed with a minus sign (i.e. dash, "-"). 
6. **site.col** Name of site column. This column lists site specific names to be associated with the geographic coordinates.

```
# View documentation for this function
?extract_geocoverage

# Run this function for your dataset
extract_geocoverage(path = "/Users/csmith/Desktop/gleon_chloride",
                    data.file = "lake_characteristics.csv",
                    lat.col = "lake_latitude",
                    lon.col = "lake_longitude",
                    site.col = "lake_name")
```

This function outputs a tab delimited file named `geographic_coverage.txt` to your metadata directory. You may edit this in a spreadsheet editor if you'd like, but if the data table this information has been extracted from is accurate, then there is no need for editing.

#### Geographic areas

The `import_templates` function created a tab delimited table (bounding_boxes.txt) in your working directory. Instructions for completing this file:

* **geographicDescription** Enter a brief description for each geographic area.
* **westBoundingCoordinate** Enter the western most geographic coordinate (in decimal degrees) of the area. Longitudes west of the prime meridian are prefixed with a minus sign (i.e. dash -). 
* **eastBoundingCoordinate** Enter the eastern most geographic coordinate (in decimal degrees) of the area. Longitudes west of the prime meridian are prefixed with a minus sign (i.e. dash -). 
* **northBoundingCoordinate** Enter the northern most geographic coordinate (in decimal degrees) of the area. Latitudes south of the equator are prefixed with a minus sign (i.e. dash -).
* **southBoundingCoordinate** Enter the northern most geographic coordinate (in decimal degrees) of the area. Latitudes south of the equator are prefixed with a minus sign (i.e. dash -).

## Make EML
[back to top](#contents)

Now you are ready to synthesize your completed metadata templates into EML. This step is relatively simple, but requires several arguments:

**NOTE: Make sure all your metadata templates and associated data files are closed. `make_eml` will fail if these files are open.**

1. **path** (character) A path to the directory containing the completed metadata templates. Data files can also be stored at this path.
2. **data.path** (character) A path to the directory containing the data entities described by the metadata templates. Default is set to path.
3. **eml.path** (character) A path to the directory where the EML will be writtn. Default is set to path.
4. **dataset.title** (character) The title for your data package. Be descriptive (more than 5 words). We recommend the following format: Project name: Broad description: Time span (e.g. "GLEON: Long term lake chloride concentrations from North America and Europe: 1940-2016").
5. **data.files** (character) A vector of character strings specifying the names of your data files (e.g. data.files = c("lake_chloride_concentrations.csv", "lake_characteristics.csv")).
6. **data.files.description** (character) A vector of character strings briefly describing the data files listed in the data.files argument and in the same order as listed in the data.files argument (e.g. data.files.description = c("Chloride concentration data.", "Climate, road density, and impervious surface data."))
7. **data.files.quote.character** (character) A vector of character strings defining the quote characters used in your data files and in the same order as listed in the data.files argument. This argument is required only if your data contain quotations.  If the quote character is a quotation, then enter "\\"". If the quote character is an apostrophe, then enter "\\'". Example: data.files.quote.character = c("\\"", "\\"").
8. **data.files.url** (character) The URL of where your data tables are stored on a publicly accessible server (i.e. does not require user ID or password). This argument is required only if your data are accessible from a publicly accesible URL. The EDI data repository software, PASTA+, will use this to upload your data into the repository. If you will be manually uploading your data tables, then don't use this argument. Example: data.files.url = "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride". 
9. **zip.dir** (character) A vector of character strings listing the .zip directories of your dataset.
10. **zip.dir.description** (character) A vector of character strings briefly describing the contents of any .zip directory present in the working directory.
11. **temporal.coverage** (character) A vector of character strings specifying the beginning and ending dates of your dataset. Use the format YYYY-MM-DD.
12. **geographic.coordinates** (character) A vector of character strings specifying the spatial bounding coordinates of your dataset in decimal degrees. This argument is not required if you are supplying bounding coordinates in the bounding_boxes.txt template file. The list must follow this order: North, East, South, West. Longitudes West of the prime meridian and latitudes South of the equator are prefixed with a minus sign (i.e. dash -). If you don't have an area, but rather a point. Repeat the latitude value for North and South, and repeat the longitude value for East and West (e.g. geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95)).
13. **geographic.description** (character) A description of the geographic coverage of your dataset. Don't use this argument if you are supplying geographic.coordinates in the bounding_boxes.txt template file. Example: "North America and Europe".
14. **maintenance.description** (character) A description of whether data collection for this dataset is "ongoing" or "completed".
15. **user.id** (character) A vector of character strings, specifying your user ID for the EDI data repository. The user.id controls editing access to your data package. If you do not have one, contact EDI (info@@environmentaldatainitiative.org) to obtain one. In the meantime do not use this argument when running `make_eml`.
16. **affiliation** (character) A vector of character strings, specifying the affiliation of your user ID. In a list, the associations must follow the same order of the corresponding values listed under user.id. This is the affiliation used when logging in to the EDI Data Portal and can be: "LTER" or "EDI". If you don't have a user.id then do not use this argument when running `make_eml`.
17. **package.id** (character) The ID of your data package. A missing package ID defaults to \emph{edi.101.1}. A package ID must contain the scope, package number, and revision number (e.g. 'edi.101.1').

```
# Make EML for data and metadata templates co-located at path
make_eml(path = "/Users/csmith/Desktop/gleon_chloride",
         dataset.title = "GLEON: Long term lake chloride concentrations from North America and Europe: 1940-2016",
         data.files = c("lake_chloride_concentrations",
                        "lake_characteristics"),
         data.files.description = c("Chloride concentration data.", 
                                    "Climate, road density, and impervious surface data."),
         data.files.quote.character = c("\"", "\""),
         temporal.coverage = c("1940-01-31", "2016-01-01"),
         geographic.description = "North America and Europe",
         geographic.coordinates = c("69.0", "28.53", "28.38", "-119.95"),
         maintenance.description = "completed", 
         user.id = "csmith",
         affiliation = "EDI",
         package.id = "edi.201.1")
         
# Make EML for data and metadata templates located in different paths
make_eml(path = "/Users/csmith/Desktop/gleon_chloride",
         data.path = "/Users/csmith/Desktop/gleon_chloride/data",
         dataset.title = "GLEON: Long term lake chloride concentrations from North America and Europe: 1940-2016",
         data.files = c("lake_chloride_concentrations",
                        "lake_characteristics"),
         data.files.description = c("Chloride concentration data.", 
                                    "Climate, road density, and impervious surface data."),
         data.files.quote.character = c("\"", "\""),
         temporal.coverage = c("1940-01-31", "2016-01-01"),
         geographic.description = "North America and Europe",
         geographic.coordinates = c("69.0", "28.53", "28.38", "-119.95"),
         maintenance.description = "completed", 
         user.id = "csmith",
         affiliation = "EDI",
         package.id = "edi.201.1")
         
# Make EML for data and metadata templates located in different paths, and EML to be exported to a separate path
make_eml(path = "/Users/csmith/Desktop/gleon_chloride",
         data.path = "/Users/csmith/Desktop/gleon_chloride/data",
         eml.path = "/Users/csmith/Desktop/gleon_chloride/eml",
         dataset.title = "GLEON: Long term lake chloride concentrations from North America and Europe: 1940-2016",
         data.files = c("lake_chloride_concentrations",
                        "lake_characteristics"),
         data.files.description = c("Chloride concentration data.", 
                                    "Climate, road density, and impervious surface data."),
         data.files.quote.character = c("\"", "\""),
         temporal.coverage = c("1940-01-31", "2016-01-01"),
         geographic.description = "North America and Europe",
         geographic.coordinates = c("69.0", "28.53", "28.38", "-119.95"),
         maintenance.description = "completed", 
         user.id = "csmith",
         affiliation = "EDI",
         package.id = "edi.201.1")

```

Your EML file will be written to your data directory with the name `packageID.xml`. If your EML is valid you will receive the message: `EML passed validation!`. If validation fails, open the EML file in an XML editor and look for the invalid section. Often a minor tweak to the EML can be made manually to bring it into compliance with the EML schema.


## Upload your data package
[back to top](#contents)

Your data and metadata form a package that may be uploaded to the [EDI data repository](https://portal.edirepository.org/nis/home.jsp). Contact EDI for login credentials (info@environmentaldatainitiative.org).

