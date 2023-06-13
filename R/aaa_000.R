
#' @importFrom utils packageVersion
.onLoad <- function(libname, pkgname) {

        options("cache.debug" = FALSE)

	##Make sure all known package versions are associated with a cache version
	packageVersion_to_dFileVersion<-list(
		"0.89.0" = "0.89.0", 
		"0.90.0" = "0.89.0"
	)
	
	##Set global variable pkgname
	package_env <- asNamespace(pkgname)
	assign("pkgname",                        pkgname, envir = package_env)
  assign("packageVersion_to_dFileVersion", packageVersion_to_dFileVersion, envir = package_env)

	##Force the creation of a existing packageVersionToCacheVersion for the current version
	if ( length(packageVersion_to_dFileVersion[[as.character(packageVersion(pkgname))]])!=1 ) {
		stop(simpleError(paste0("Bug: no HandlerClass set in packageVersion_to_dFileVersion for current package version (",paste(packageVersion(pkgname)),").")))
	}
	
	##Force all defined handlerClasses to be referred to in the packageVersion_to_dFileVersion
	undefClass<-setdiff(.getHandlerClasses(),packageVersion_to_dFileVersion)
	if ( length(undefClass)) {
		stop(simpleError(paste0("Bug: HandlerClass(es) (",paste(undefClass,collapse=", "),") not referred to in packageVersion_to_dFileVersion.")))
	}
	
}

