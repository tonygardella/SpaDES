if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

################################################################################
#' Determine which modules in a list are unparsed
#'
#' Internal function, used during \code{\link{simInit}}.
#'
#' @param modules A character vector specifying the modules to parse.
#'
#' @return The ids of the unparsed list elements.
#'
#' @docType methods
#' @keywords internal
#' @rdname unparsed
#'
#' @author Alex Chubaty
#'
setGeneric(".unparsed",
           function(modules) {
             standardGeneric(".unparsed")
})

#' @rdname unparsed
setMethod(
  ".unparsed",
  signature(modules = "list"),
  definition = function(modules) {
    ids <- lapply(modules, function(x) {
      (attr(x, "parsed") == FALSE)
    }) %>% `==`(., TRUE) %>% which()
    return(ids)
})

################################################################################
#' @return \code{.parseModulePartial} extracts just the individual element
#' requested from the module. This can be useful if parsing the whole module
#' would cause an error.
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @export
#' @param filename The filename of the module to be parsed.
#' @inheritParams spades
#' @param defineModuleElement Character string indicating which of the list
#'                            elements in defineModule should be extracted
#' @docType methods
#' @rdname parseModule
#'
#' @author Eliot McIntire
#'
setGeneric(".parseModulePartial",
           function(sim, modules, filename, defineModuleElement) {
             standardGeneric(".parseModulePartial")
})

#' @rdname parseModule
setMethod(
  ".parseModulePartial",
  signature(
    sim = "missing",
    modules = "missing",
    filename = "character",
    defineModuleElement = "character"
  ),
  definition = function(filename, defineModuleElement) {
    parsedFile <- parse(filename)
    defineModuleItem <-
      grepl(pattern = "defineModule", parsedFile)
    pf <- parsedFile[defineModuleItem]

    namesParsedList <- names(parsedFile[defineModuleItem][[1]][[3]])

    element <- (namesParsedList == defineModuleElement)
    out <- pf[[1]][[3]][element][[1]]
    out <- tryCatch(
      eval(out),
      error = function(x) out
    )
    return(out)
})

#' @rdname parseModule
setMethod(
  ".parseModulePartial",
  signature(
    sim = "simList",
    modules = "list",
    filename = "missing",
    defineModuleElement = "character"
  ),
  definition = function(sim, modules, defineModuleElement) {
    out <- list()
    for (j in seq_along(modules)) {
      m <- modules[[j]][1]
      filename <-
        paste(sim@paths[['modulePath']], "/", m, "/", m, ".R", sep = "")
      out[[m]] <- .parseModulePartial(filename = filename,
                                      defineModuleElement = defineModuleElement)
    }
    return(out)
})

################################################################################
#' Parse and initialize a module
#'
#' Internal function, used during \code{\link{simInit}}.
#'
#' @param sim     A \code{simList} simulation object.
#'
#' @param modules A list of modules with a logical attribute "parsed".
#'
#' @param userSuppliedObjNames Character string (or \code{NULL}, the default)
#'                             indicating the names of objects that user has passed
#'                             into simInit via objects or inputs.
#'                             If all module inputObject dependencies are provided by user,
#'                             then the \code{.inputObjects} code will be skipped.
#'
#' @return A \code{simList} simulation object.
#'
#' @include module-dependencies-class.R
#' @include simList-class.R
#' @include environment.R
#' @docType methods
#' @keywords internal
#' @rdname parseModule
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setGeneric(".parseModule",
           function(sim, modules, userSuppliedObjNames = NULL) {
             standardGeneric(".parseModule")
})

#' @rdname parseModule
setMethod(
  ".parseModule",
  signature(sim = "simList", modules = "list"),
  definition = function(sim, modules, userSuppliedObjNames) {
    all_children <- list()
    children <- list()
    parent_ids <- integer()
    for (j in .unparsed(modules)) {
      m <- modules[[j]][1]
      filename <-
        paste(sim@paths[['modulePath']], "/", m, "/", m, ".R", sep = "")
      parsedFile <- parse(filename)
      defineModuleItem <-
        grepl(pattern = "defineModule", parsedFile)

      # evaluate the rest of the parsed file
      eval(parsedFile[!defineModuleItem], envir = sim@.envir)

      # parse any scripts in R subfolder
      RSubFolder <- file.path(dirname(filename), "R")
      RScript <- dir(RSubFolder)
      if (length(RScript) > 0) {
        for (Rfiles in RScript) {
          parsedFile1 <- parse(file.path(RSubFolder, Rfiles))
          eval(parsedFile1, envir = sim@.envir)
        }
      }

      # evaluate all but inputObjects and outputObjects part of 'defineModule'
      #  This allow user to use params(sim) in their inputObjects
      namesParsedList <-
        names(parsedFile[defineModuleItem][[1]][[3]])
      inObjs <- (namesParsedList == "inputObjects")
      outObjs <- (namesParsedList == "outputObjects")
      pf <- parsedFile[defineModuleItem]
      pf[[1]][[3]] <- pf[[1]][[3]][!(inObjs | outObjs)]
      sim <- suppressWarnings(eval(pf))

      # check that modulename == filename
      fname <- unlist(strsplit(basename(filename), "[.][r|R]$"))
      for (k in length(sim@depends@dependencies)) {
        if (sim@depends@dependencies[[k]]@name == m)
          i <- k
      }

      # assign default param values
      deps <- sim@depends@dependencies[[i]]@parameters
      sim@params[[m]] <- list()
      if (NROW(deps) > 0) {
        for (x in 1:NROW(deps)) {
          sim@params[[m]][[deps$paramName[x]]] <- deps$default[[x]]
        }
      }

      # do inputObjects and outputObjects
      pf <- parsedFile[defineModuleItem]
      if (any(inObjs)) {
        sim@depends@dependencies[[i]]@inputObjects <-
          eval(pf[[1]][[3]][inObjs][[1]])
        sim@depends@dependencies[[i]]@outputObjects <-
          eval(pf[[1]][[3]][outObjs][[1]])
      }

      # update parse status of the module
      attributes(modules[[j]]) <- list(parsed = TRUE)

      # add child modules to list of all child modules, to be parsed later
      children <-
        as.list(sim@depends@dependencies[[i]]@childModules) %>%
        lapply(., `attributes<-`, list(parsed = FALSE))
      all_children <- append_attr(all_children, children)

      # remove parent module from the list
      if (length(children)) {
        parent_ids <- c(parent_ids, j)
      }

      ## run .inputObjects() from each module file from each module, one at a time,
      ## and remove it from the simList so next module won't rerun it.

      # If user supplies the needed objects, then test whether all are supplied.
      # If they are all supplied, then skip the .inputObjects code
      if (!all(sim@depends@dependencies[[i]]@inputObjects$objectName %in% userSuppliedObjNames)) {
        if (!is.null(sim@.envir$.inputObjects)) {
          sim <- sim@.envir$.inputObjects(sim)
          rm(".inputObjects", envir = sim@.envir)
        }
      }
    }

    names(sim@depends@dependencies) <- unlist(modules)

    modules(sim) <- if (length(parent_ids)) {
      append_attr(modules, all_children)[-parent_ids]
    } else {
      append_attr(modules, all_children)
    } %>%
      unique()

    return(sim)
})
