% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-template.R
\docType{methods}
\name{newModule}
\alias{newModule}
\alias{newModule,character,character-method}
\alias{newModule,character,missing-method}
\title{Create new module from template.}
\usage{
newModule(name, path, ...)

\S4method{newModule}{character,character}(name, path, ...)

\S4method{newModule}{character,missing}(name, path, ...)
}
\arguments{
\item{name}{Character string specfying the name of the new module.}

\item{path}{Character string. Subdirectory in which to place the new module code file.
The default is the current working directory.}

\item{...}{Additonal arguments. Currently, only the following are supported:\cr\cr

             \code{open}. Logical. Should the new module file be opened after creation?
             Default \code{TRUE}.\cr\cr

             \code{unitTests}. Logical. Should the new module include unit test files?
             Default \code{TRUE}. Unit testing relies on the \code{testthat} package.\cr\cr

             \code{type}. Character string specifying one of \code{"child"} (default),
             or \code{"parent"}.\cr\cr

             \code{children}. Required when \code{type = "parent"}. A character vector
             specifying the names of child modules.}
}
\value{
Nothing is returned. The new module file is created at
\file{path/name.R}, as well as ancillary files for documentation, citation,
license, readme, and unit tests folder.
}
\description{
Autogenerate a skeleton for a new SpaDES module, a template for a
documentation file, a citation file, a license file, a README.txt file, and a
folder that contains unit tests information.
The \code{newModuleDocumentation} will not generate the module file, but will
create the other files.
}
\details{
All files will be created within a subfolder named \code{name} within the
\code{path}.
}
\note{
On Windows there is currently a bug in RStudio that prevents the editor
from opening when \code{file.edit} is called.
Similarly, in RStudio on OS X / macOS, there is an issue opening files where
they are opened in an overlayed window rather than a new tab.
\code{file.edit} does work if the user types it at the command prompt.
A message with the correct lines to copy and paste is provided.
}
\examples{
\dontrun{
  ## create a "myModule" module in the "modules" subdirectory.
  newModule("myModule", "modules")

  ## create a new parent module in the "modules" subdirectory.
  newModule("myParentModule", "modules", type = "parent", children = c("child1", "child2"))
}

}
\author{
Alex Chubaty and Eliot McIntire
}
\seealso{
Other module creation helpers: \code{\link{newModuleCode}},
  \code{\link{newModuleDocumentation}},
  \code{\link{newModuleTests}}
}

