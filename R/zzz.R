.getVersionFromDigestFile<- 
	function(dFile) {
		dContents = readRDS(dFile)
		if ( ! all("VERSION" %in% names(dContents))) stop(simpleError(message="No `VERSION` found in digestFile"))

		return(package_version(dContents[["VERSION"]]))
	}


.getHandlerClasses<-function(versions=NULL){
		targets <- if (is.null(version)) {"^roCache_\\d+\\.\\d+\\.\\d$"} else { paste0("^roCache_",paste0(paste0("(",versions,")"),collapse="|"),"$") }
		package_env <- asNamespace(pkgname)
		class_list <- gsub("^\\.__C__","",ls(envir = package_env, all.names = TRUE, pattern = "^\\.__C__"))
		grep(targets, class_list, value=TRUE)
	}


.format_bytes <- function(size_in_bytes) {
	if (size_in_bytes<0) stop(simpleError("Argument size_in_bytes must be >=0"))
  units <- c("B", "KB", "MB", "GB", "TB")
	unit_index <- floor(log(size_in_bytes+1, base = 1024))
  size_in_unit <- size_in_bytes / (1024^unit_index)
  paste0(formatC(size_in_unit, format = "f", digits = 1), " ", units[unit_index + 1])
}

##############################################################
#' Get a digest of a function body
#'
#' The digest of the function is excluding comment or whitespace structure.
#'
#' @param f the function to obtain the digest of
#'
#'
#' @return A character string representing the functions digest
#'
#' @examples
#'
#' aFunc1<-function(){
#'   return("aValue")
#' }
#'
#' aFunc2<-function(){
#' #Additional whitespce 
#'   return("aValue" ) ##This is a comment
#' }
#'
#' digestOfFunc(aFunc1)
#' digestOfFunc(aFunc2)
#' 
#' @importFrom digest digest
#' @importFrom utils capture.output
#' @export
digestOfFunc<-function(f){
  digest(capture.output(body(f)))
}
