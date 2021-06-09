#' Load packages into an aliased namespace.
#'
#' Register a new namespace with alias. Then, loads a package into
#' that namespace. Allows namespace management similar to python.
#'
#' @param package The name of the package to be loaded.
#' @param alias The name of the namespace to load the package into.
#' @return Nothing.
#' @export
import_as <- function(package, alias) {
    invisible(namespace::registerNamespace(alias, loadNamespace(package)))
}

