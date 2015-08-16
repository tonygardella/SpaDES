################################################################################
#' Parse and extract module metadata
#'
#' This function will return the metadata from either a module file or a
#' \code{simList} object. If a \code{simList} object is provided, then the
#' \code{path} is ignored.
#'
#' @param module Character vector. The names of modules from which metadata should
#' be returned.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is the current working directory.
#'
#' @param simList a \code{simList} object. If this is provided, module metadata
#' will be returned from modules contained within the \code{simList}, not the \code{path}
#'
#' @return A list of module metadata, matching the structure in
#'         \code{\link{defineModule}}.
#'
#' @export
#' @docType methods
#' @rdname moduleMetadata
#'
#' @seealso \code{\link{defineModule}}
#'
#' @author Alex Chubaty
#'
#' @examples
#' library(igraph)
#'  path <- system.file(package="SpaDES", "sampleModules")
#'  sampleModules <- dir(path)
#'  (x <- moduleMetadata(sampleModules, path))
#'
#'  listModules(path=path) %>%
#'    moduleMetadata(module=., path=path)
#'
setGeneric("moduleMetadata", function(module, path, simList) {
  standardGeneric("moduleMetadata")
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="character", simList="missing"),
  definition = function(module, path) {
    .Deprecated("metadata", package="SpaDES",
                "moduleMetadata will be deprecated. Please use metadata.",
                old = "moduleMetadata")
    metadata <- lapply(module, function(mod) {
      filename <- paste(path, "/", mod, "/", mod, ".R", sep="")
      stopifnot(file.exists(filename))

      parsedFile <- parse(filename)
      defineModuleItem <- grepl(pattern="defineModule", parsedFile)

      # pull out the list portion from "defineModule"
      x <- parsedFile[defineModuleItem] %>%
        as.character %>%
        gsub("[[:space:]]*\\n[[:space:]]*", " ", .) %>%
        sub("^defineModule[[:space:]]*\\([[:space:]]*", "", .) %>%
        sub("^sim[[:space:]]*,[[:space:]]*", "", .) %>%
        sub("\\)$", "", .) %>%
        gsub("[[:space:]]*=[[:space:]]*", " = ", .)

      # ensure variables in params are kept as strings
      x <- gsub("(globals\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl=TRUE) %>%
        gsub("(params\\(sim\\)\\$[^,]*)", "\"\\1\"", ., perl=TRUE)

      # check input types
      x <- gsub("extent\\(rep\\(NA, 4\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", x) %>%
        gsub("extent\\(c\\(NA, NA, NA, NA\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", .)

      # store metadata as list
      metadata <- eval(parse(text = x))
      return(metadata)
    })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="missing", simList="missing"),
  definition = function(module) {
    moduleMetadata(module, getwd())
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="character", path="missing", simList="simList"),
  definition = function(module, simList) {
    .Deprecated("metadata", package="SpaDES",
                "moduleMetadata will be deprecated. Please use metadata.",
                old = "moduleMetadata")
    metadata <- match(module, sapply(simList@depends@dependencies, function(x) x@name)) %>%
      lapply(., function(mod) {
        metadata <- slotNames(simList@depends@dependencies[[mod]]) %>%
          lapply(., function(x) slot(simList@depends@dependencies[[mod]],
                                 x))
        names(metadata) <- slotNames(simList@depends@dependencies[[mod]])
        metadata
      })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
  })

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(module="missing", path="missing", simList="simList"),
  definition = function(simList) {
    .Deprecated("metadata", package="SpaDES",
                "moduleMetadata will be deprecated. Please use metadata.",
                old = "moduleMetadata")
    modNames <- sapply(1:length(sapply(simList@depends@dependencies, function(x) x@name)),
           function(mod) {
      simList@depends@dependencies[[mod]]@name
    })
    moduleMetadata(modNames, simList=simList)
  })


################################################################################
#' Parse and extract module metadata
#'
#' This function will return the metadata from either a module file or a
#' \code{simList} object. If a \code{simList} object is provided, then the
#' \code{path} is ignored.
#'
#' @param module Character vector. The names of modules from which metadata should
#' be returned.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is the current working directory.
#'
#' @param simList a \code{simList} object. If this is provided, module metadata
#' will be returned from modules contained within the \code{simList}, not the \code{path}
#'
#' @return A list of module metadata, matching the structure in
#'         \code{\link{defineModule}}.
#'
#' @export
#' @docType methods
#' @rdname metadata
#'
#' @seealso \code{\link{defineModule}}
#'
#' @author Alex Chubaty
#'
#' @examples
#' library(igraph)
#'  path <- system.file(package="SpaDES", "sampleModules")
#'  sampleModules <- dir(path)
#'  (x <- metadata(sampleModules, path))
#'
#'  listModules(path=path) %>%
#'    metadata(module=., path=path)
#'
setGeneric("metadata", function(module, path, simList) {
  standardGeneric("metadata")
})

#' @export
#' @rdname metadata
setMethod(
  "metadata",
  signature = c(module="character", path="character", simList="missing"),
  definition = function(module, path) {

    metadata <- lapply(module, function(mod) {
      filename <- paste(path, "/", mod, "/", mod, ".R", sep="")
      stopifnot(file.exists(filename))

      parsedFile <- parse(filename)
      defineModuleItem <- grepl(pattern="defineModule", parsedFile)

      # pull out the list portion from "defineModule"
      x <- parsedFile[defineModuleItem] %>%
        as.character %>%
        gsub("[[:space:]]*\\n[[:space:]]*", " ", .) %>%
        sub("^defineModule[[:space:]]*\\([[:space:]]*", "", .) %>%
        sub("^sim[[:space:]]*,[[:space:]]*", "", .) %>%
        sub("\\)$", "", .) %>%
        gsub("[[:space:]]*=[[:space:]]*", " = ", .)

      # ensure variables in params are kept as strings
      x <- gsub("(globals\\(sim\\)\\$[^\\),]*)", "\"\\1\"", x, perl=TRUE) %>%
        gsub("(params\\(sim\\)\\$[^,]*)", "\"\\1\"", ., perl=TRUE)

      # check input types
      x <- gsub("extent\\(rep\\(NA, 4\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", x) %>%
        gsub("extent\\(c\\(NA, NA, NA, NA\\)\\)", "extent\\(rep\\(NA_real_, 4\\)\\)", .)

      # store metadata as list
      metadata <- eval(parse(text = x))
      return(metadata)
    })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
  })

#' @export
#' @rdname metadata
setMethod(
  "metadata",
  signature = c(module="character", path="missing", simList="missing"),
  definition = function(module) {
    metadata(module, getwd())
  })

#' @export
#' @rdname metadata
setMethod(
  "metadata",
  signature = c(module="character", path="missing", simList="simList"),
  definition = function(module, simList) {
    metadata <- match(module, sapply(simList@depends@dependencies, function(x) x@name)) %>%
      lapply(., function(mod) {
        metadata <- slotNames(simList@depends@dependencies[[mod]]) %>%
          lapply(., function(x) slot(simList@depends@dependencies[[mod]],
                                     x))
        names(metadata) <- slotNames(simList@depends@dependencies[[mod]])
        metadata
      })

    names(metadata) <- sapply(metadata, function(x) x$name)
    return(metadata)
  })

#' @export
#' @rdname metadata
setMethod(
  "metadata",
  signature = c(module="missing", path="missing", simList="simList"),
  definition = function(simList) {
    modNames <- sapply(1:length(sapply(simList@depends@dependencies, function(x) x@name)),
                       function(mod) {
                         simList@depends@dependencies[[mod]]@name
                       })
    metadata(modNames, simList=simList)
  })

################################################################################
#' Parse and extract specific elements of module metadata
#'
#' These functions will return specific elements of the metadata, either via
#' passing a \code{simList} object, or by passing the module metadata list
#' output from the \code{metadata} function.
#'
#' @param x Either a named list of module metadata (i.e., the result of a call to
#' \code{moduleMetadata}) or a \code{simList}.
#'
#' @return A list of various named elements in the metadata
#'
#' @rdname metadata-accessors
#' @name keywords
#' @aliases keywords
#' @export
#' @examples
#'  library(igraph)
#'  path <- system.file(package="SpaDES", "sampleModules")
#'  myMetadata <- listModules(path=path) %>%
#'    moduleMetadata(., path=path)
#'  keywords(myMetadata)
#'
#'  ## use a simList object
#'  times <- list(start=0, end=1, timeunit="second")
#'  parameters <- list(.globals=list(stackName="landscape"),
#'                     randomLandscapes=list(.plotInitialTime=NA))
#'  modules <- list("randomLandscapes", "caribouMovement")
#'  paths <- list(modulePath=system.file("sampleModules", package="SpaDES"),
#'                outputPath=tempdir())
#'  sim1 <- simInit(times=times, params=parameters, modules=modules, paths=paths)
#'  keywords(sim1)
setGeneric("keywords", function(x) {
  standardGeneric("keywords")
})

#' @export
#' @rdname metadata-accessors
setMethod(
  "keywords",
  signature = c(x="list"),
  definition = function(x) {
    keywords <- lapply(x, function(y) y$keywords)
    names(keywords) <- sapply(x, function(y) y$name)
    return(keywords)
})


#' @rdname metadata-accessors
#' @aliases keywords
#' @export
setMethod(
  "keywords",
  signature = c(x="simList"),
  definition = function(x) {
    keywords <- lapply(x@depends@dependencies, function(y) y@keywords)
    names(keywords) <- sapply(x@depends@dependencies, function(y) y@name)
    return(keywords)
  })

#' @rdname metadata-accessors
#' @name extents
#' @aliases extents
#' @export
#' @rdname metadata-accessors
#' @inheritParams keywords
#' @examples
#'  library(igraph)
#'  path <- system.file(package="SpaDES", "sampleModules")
#'  myMetadata <- listModules(path=path) %>%
#'    moduleMetadata(., path=path)
#'  extents(myMetadata)
setGeneric("extents", function(x) {
  standardGeneric("extents")
})

#' @export
#' @rdname metadata-accessors
setMethod(
  "extents",
  signature = c(x="list"),
  definition = function(x) {
    extents <- lapply(x, function(y) y$extent)
    names(extents) <- sapply(x, function(y) y$name)
    return(extents)
  })


#' @rdname metadata-accessors
#' @aliases extents
#' @export
setMethod(
  "extents",
  signature = c(x="simList"),
  definition = function(x) {
    extents <- lapply(x@depends@dependencies, function(y) y@spatialExtent)
    names(extents) <- sapply(x@depends@dependencies, function(y) y@name)
    return(extents)
  })

#' @rdname metadata-accessors
#' @name timeframes
#' @aliases timeframes
#' @export
#' @rdname metadata-accessors
#' @inheritParams keywords
#' @examples
#'  library(igraph)
#'  path <- system.file(package="SpaDES", "sampleModules")
#'  myMetadata <- listModules(path=path) %>%
#'    moduleMetadata(., path=path)
#'  timeframes(myMetadata)
setGeneric("timeframes", function(x) {
  standardGeneric("timeframes")
})

#' @export
#' @rdname metadata-accessors
setMethod(
  "timeframes",
  signature = c(x="list"),
  definition = function(x) {
    timeframes <- lapply(x, function(y) y$timeframe)
    names(timeframes) <- sapply(x, function(y) y$name)
    return(timeframes)
  })


#' @rdname metadata-accessors
#' @aliases timeframes
#' @export
setMethod(
  "timeframes",
  signature = c(x="simList"),
  definition = function(x) {
    timeframes <- lapply(x@depends@dependencies, function(y) y@timeframe)
    names(timeframes) <- sapply(x@depends@dependencies, function(y) y@name)
    return(timeframes)
  })
