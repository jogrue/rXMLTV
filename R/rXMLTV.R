require("XML")
require("dplyr")
#require("data.table")

# Internal functions for parsing XMLTV files -----------------------------------

# Checks if data object is from class xmltv.data.raw
.IsXmltvDataRaw <- function(x) {
  return(inherits(x, what = "xmltv.data.raw"))
}

# Checks if data object is from class xmltv.data
.IsXmltvData <- function(x) {
  return(inherits(x, what = "xmltv.data"))
}

# Assigns class xmltv.data.raw if not assigned already
.AssignXmltvDataRaw <- function(x) {
  if (.IsXmltvDataRaw(x)) {
    return(x)
  } else {
    class(x) <- append(class(x),"xmltv.data.raw")
    return(x)
  }
}

# Assigns class xmltv.data if not assigned already
.AssignXmltvData <- function(x) {
  if (.IsXmltvData(x)) {
    return(x)
  } else {
    class(x) <- append(class(x),"xmltv.data")
    return(x)
  }
}

# Parse numeric values to factors according to the codebook
.ParseToFactors <- function(xmltv.data) {
  return(xmltv.data)
}

# Parse a single programme node, returns a list containing the programmes data
.ParseProgramme <- function(xmltv.programme) {

  programme <- NULL
  programme$r1.generator <- NA
  programme$r2.generator.url <- NA

  # Programme's time
  programme$r3.start <- xmlGetAttr(xmltv.programme, name = "start",
                                   default = NA)
  programme$r4.stop <- xmlGetAttr(xmltv.programme, name = "stop", default = NA)
  programme$r5.date <- as.Date(substr(programme$r3.start,1,8),
                               format = "%Y%m%d")
  programme$r6.start.time <- as.POSIXct(programme$r3.start,
                                        format = "%Y%m%d%H%M%S %z")
  programme$r7.stop.time <- as.POSIXct(programme$r4.stop,
                                       format = "%Y%m%d%H%M%S %z")
  programme$r8.timezone <- substr(programme$r3.start,
                                  nchar(programme$r3.start) - 4,
                                  nchar(programme$r3.start))
  programme$r9.duration <- as.numeric(programme$r7.stop.time -
                                        programme$r6.start.time, units = "mins")

  # Programme's channel
  programme$r10.channel <- xmlGetAttr(xmltv.programme, name = "channel",
                                      default = NA)

  # Programme's title and subtitle
  titles <- xpathSApply(xmltv.programme, "title", xmlValue)
  titles.lang <- xpathSApply(xmltv.programme, "title",
                             fun = function(x) xmlGetAttr(x, name = "lang",
                                                          default = NA))
  subtitles <- xpathSApply(xmltv.programme, "sub-title", xmlValue)
  subtitles.lang <- xpathSApply(xmltv.programme, "sub-title",
                                fun = function(x) xmlGetAttr(x, name = "lang",
                                                             default = NA))
  # First title
  if (length(titles)>=1) {
    tmp <- titles[[1]]
    tmp.lang <- titles.lang[[1]]
    if (is.null(tmp.lang)) {
      tmp.lang <- ""
    }
  } else {
    tmp <- ""
    tmp.lang <- ""
  }
  programme$r11.title1 <- tmp
  programme$r12.title1.lang <- tmp.lang
  # Second title
  if (length(titles)>=2) {
    tmp <- titles[[2]]
    tmp.lang <- titles.lang[[2]]
    if (is.null(tmp.lang)) {
      tmp.lang <- ""
    }
  } else {
    tmp <- ""
    tmp.lang <- ""
  }
  programme$r13.title2 <- tmp
  programme$r14.title2.lang <- tmp.lang
  # First subtitle
  if (length(subtitles)>=1) {
    tmp <- subtitles[[1]]
    tmp.lang <- subtitles.lang[[1]]
    if (is.null(tmp.lang)) {
      tmp.lang <- ""
    }
  } else {
    tmp <- ""
    tmp.lang <- ""
  }
  programme$r15.subtitle1 <- tmp
  programme$r16.subtitle1.lang <- tmp.lang
  # Second subtitle
  if (length(subtitles)>=2) {
    tmp <- subtitles[[2]]
    tmp.lang <- subtitles.lang[[2]]
    if (is.null(tmp.lang)) {
      tmp.lang <- ""
    }
  } else {
    tmp <- ""
    tmp.lang <- ""
  }
  programme$r17.subtitle2 <- tmp
  programme$r18.subtitle2.lang <- tmp.lang
  tmp <- NULL
  tmp.lang <- NULL

  # Description
  programme$r19.descr <- xmlValue(xmltv.programme[["desc"]])
  if (is.na(programme$r19.descr)) {
    programme$r20.descr.lang <- NA
  } else {
    programme$r20.descr.lang <- xmlGetAttr(xmltv.programme[["desc"]],
                                           name = "lang", default = NA)
  }

  # Credits
  # Director
  directors <- xpathSApply(xmltv.programme, "credits//director", xmlValue)
  programme$r21.directors <- paste0(directors, collapse = "|")
  directors <- NULL
  # Producer
  producers <- xpathSApply(xmltv.programme, "credits//producer", xmlValue)
  programme$r22.producers <- paste0(producers, collapse = "|")
  producers <- NULL
  # Writer
  writers <- xpathSApply(xmltv.programme, "credits//writer", xmlValue)
  programme$r23.writers <- paste0(writers, collapse = "|")
  writers <- NULL
  # Presenter
  presenters <- xpathSApply(xmltv.programme, "credits//presenter", xmlValue)
  programme$r24.presenters <- paste0(presenters, collapse = "|")
  presenters <- NULL
  # Actor
  actors <- xpathSApply(xmltv.programme, "credits//actor", xmlValue)
  roles <- xpathSApply(xmltv.programme, "credits//actor", function(x)
    xmlGetAttr(x, name = "role", default = NA))
  programme$r25.actors <- paste0(actors, collapse = "|")
  programme$r26.roles <- paste0(roles, collapse = "|")
  actors <- NULL
  roles <- NULL

  # Programme's categories
  catlist <- xpathSApply(xmltv.programme,"category",xmlValue)
  catlist.lang <- xpathSApply(xmltv.programme, "category", function(x)
    xmlGetAttr(x, name = "lang", default = NA))
  programme$r27.categories <- paste0(catlist, collapse = "|")
  programme$r28.categories.lang <- paste0(catlist.lang, collapse = "|")
  catlist <- NULL
  catlist.lang <- NULL

  # Production date
  programme$r29.date <- as.numeric(xmlValue(xmltv.programme[["date"]]))

  # Production country
  programme$r30.country <- xmlValue(xmltv.programme[["country"]])

  # Rating (only the first rating)
  programme$r31.rating <- xmlValue(xmltv.programme[["star-rating"]][["value"]])
  if (is.na(programme$r31.rating)) {
    programme$r32.rating.system <- NA
  } else {
    programme$r32.rating.system <-
      xmlGetAttr(xmltv.programme[["star-rating"]], name = "system",
                 default = NA)
  }

  # Episode number (only the first listing)
  programme$r33.episode <- xmlValue(xmltv.programme[["episode-num"]])
  if (is.na(programme$r33.episode)) {
    programme$r34.episode.system <- NA
  } else {
    programme$r34.episode.system <- xmlGetAttr(xmltv.programme[["episode-num"]],
                                               name = "system", default = NA)
  }

  # URL
  programme$r35.url <- xmlValue(xmltv.programme[["url"]])

  # Aspect ratio
  programme$r36.aspect <- xmlValue(xmltv.programme[["video"]][["aspect"]])

  # Quality
  programme$r37.quality <- xmlValue(xmltv.programme[["video"]][["quality"]])

  return(programme)
}

# Parse a list of programme nodes, returns a raw XMLTV data.frame
.ParseProgrammes <- function(xmltv.programmes) {
  programmes <- bind_rows(lapply(xmltv.programmes, .ParseProgramme))
  .AssignXmltvDataRaw(programmes)
  return(programmes)
}

# Reads a mapping file. Mappings are used to map raw date according to the codebook.
.ReadMapping <- function(mapping.loc, is.dir = TRUE) {
  mapping <- NULL
  if (is.dir) {
    mapping$channels <- read.csv(paste0(mapping.loc,"channels.csv"), header = TRUE)
    mapping$further <- read.csv(paste0(mapping.loc,"further.csv"), header = TRUE)
    mapping$genre <- read.csv(paste0(mapping.loc,"genre.csv"), header = TRUE)
    mapping$language <- read.csv(paste0(mapping.loc,"language.csv"), header = TRUE)
    mapping$type_corrs <- read.csv(paste0(mapping.loc,"type_corrs.csv"), header = TRUE)
    mapping$type <- read.csv(paste0(mapping.loc,"type.csv"), header = TRUE)
  } else {
    mapping$channels <- read.csv(unz(mapping.loc, filename = "channels.csv"), header = TRUE)
    mapping$further <- read.csv(unz(mapping.loc, filename = "further.csv"), header = TRUE)
    mapping$genre <- read.csv(unz(mapping.loc, filename = "genre.csv"), header = TRUE)
    mapping$language <- read.csv(unz(mapping.loc, filename = "language.csv"), header = TRUE)
    mapping$type_corrs <- read.csv(unz(mapping.loc, filename = "type_corrs.csv"), header = TRUE)
    mapping$type <- read.csv(unz(mapping.loc, filename = "type.csv"), header = TRUE)
  }
  return(mapping)
}


#### External functions for parsing XMLTV files --------------------------------

# Reads an XMLTV file and returns its content in a raw XMLTV data.frame
ReadXmltvFile <- function(xmltv.file, verbose = FALSE) {
  if (verbose == TRUE) {
    print(xmltv.file)
    flush.console()
  }
  xmltv.root <- xmlRoot(xmlTreeParse(file = xmltv.file, useInternal = TRUE))
  xmltv.programmes <- getNodeSet(xmltv.root,"//programme")
  xmltv.data <- .ParseProgrammes(xmltv.programmes)
  if (length(xmltv.data)>0) {
    xmltv.data$r1.generator <-
      xmlGetAttr(xmltv.root, name = "generator-info-name", default = NA)
    xmltv.data$r2.generator.url <-
      xmlGetAttr(xmltv.root, name = "generator-info-url", default = NA)
  }
  .AssignXmltvDataRaw(xmltv.data)
  return(xmltv.data)
}

# Tries to load the XMLTV root, if it fails the erreneous file is returned
TestXmltvFile <- function(xmltv.file, verbose = FALSE) {
  if (verbose == TRUE) {
    print(xmltv.file)
    flush.console()
  }
  xmltv.root <-
    try(xmlRoot(xmlTreeParse(file = xmltv.file, useInternal = TRUE)))
  err.file <- NULL
  if (class(xmltv.root)[[1]] == "try-error") {
    err.file <- xmltv.file
  }
  return(err.file)
}

# Reads multiple XMLTV files in a directory and returns their content in a data.frame
ReadMultipleXmltvFiles <- function (xmltv.dir, regex.pattern = "*",
                                    verbose = FALSE) {
  filelist <- list.files(path = xmltv.dir, pattern = paste0("^", regex.pattern),
                         ignore.case = TRUE, recursive = FALSE,
                         include.dirs = FALSE, full.names = TRUE)
  xmltv.data <- bind_rows(lapply(filelist,
                                 function(x) ReadXmltvFile(x, verbose)))
  .AssignXmltvDataRaw(xmltv.data)
  return(xmltv.data)
}

# Tries multiple XMLTV files in a directory and returns a list of erreneous files
TestMultipleXmltvFiles <- function (xmltv.dir, regex.pattern = "*",
                                    verbose = FALSE) {
  filelist <- list.files(path = xmltv.dir, pattern = paste0("^", regex.pattern),
                         ignore.case = TRUE, recursive = FALSE,
                         include.dirs = FALSE, full.names = TRUE)
  err.files <- NULL
  err.files <- lapply(filelist, function(x) TestXmltvFile(x, verbose))
  err.files <- err.files[!sapply(err.files, is.null)]
  return(err.files)
}

# Parses a data.frame containing raw XMLTV data according to the codebook.
ParseXmltvData <- function (xmltv.data = NULL, mapping.dir = "",
                            mapping.zip = "", use.factors = TRUE,
                            include.rawdata = FALSE) {
  # Check function parameters
  mapping <- NULL
  if (mapping.dir != "") {
    mapping <- .ReadMapping(mapping.dir, is.dir = TRUE)
  }
  if (mapping.zip != "") {
    if (is.null(mapping)) {
      mapping <- .ReadMapping(mapping.zip, is.dir = FALSE)
    } else {
      warning("A directory and a zip file was provided, mapping directory is used")
    }
  }
  if (mapping.dir == "" & mapping.zip == "") {
    stop("No mapping directory or mapping zip file passed.")
  }
  if (is.null(xmltv.data)) {
    stop("No data.frame passed in xmltv.data.")
  }

  xmltv.parsed <- NULL
  # code

  # Return factors instead of numeric values if factors = TRUE
  if (use.factors == TRUE) {
    xmltv.data <- .ParseToFactors(xmltv.data)
  }

  # Drop raw data from XMLTV file if include.rawdata = FALSE
  if (include.rawdata == FALSE) {
    xmltv.data <- .DropRawData(xmltv.data)
  }

  return(return.data)
}

# Drop possibly copyrighted descriptions from raw data
DropDescriptionFromRaw <- function () {

}

# Drop possibly copyrighted descriptions from parsed data
DropDescriptionFromParsed <- function () {

}

# Returns a data.frame containing programmes whose type has not yet been defined in the type.csv mapping file
GetUndefined <- function(dataset) {
  return(dataset[dataset$c5d_type_lvl4==9999,c("a1a_grabber","a1b_grabberURL","a2_nameInGrabber","c1_title","c3_subtitle","c14_categories","c5d_type_lvl4")])
}

# Returns a data.frame containing programmes whose type could not be determined based on the categories in the XMLTV file
GetUndeterminable <- function(dataset) {
  return(dataset[dataset$c5d_type_lvl4==9000,c("a1a_grabber","a1b_grabberURL","a2_nameInGrabber","c1_title","c3_subtitle","c14_categories","c5d_type_lvl4")])
}

# Returns a data.frame containing a number of random programmes from certain channels for checks
GetRandomChannelData <- function(dataset, number, channel) {
  reduced_data = dataset[dataset$a3_channel==channel,]
  samplerows = sample(1:nrow(reduced_data), 100, replace=FALSE)
  reduced_data = reduced_data[samplerows,]
  return(reduced_data[,c("a1a_grabber","a1b_grabberURL","a2_nameInGrabber","c1_title","c3_subtitle","c14_categories","c5d_type_lvl4")])
}
