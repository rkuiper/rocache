#' @import methods
setClass("roCache_versionBase",
	slots = list(
		dFile           = "character",
		cFolder         = "character",
		syncAllowed     = "logical",
	
		version         = "character",
		previousVersion = "character",
		RDSversion      = "integer"
	)
)


#Define the initialize method for the class
#' @import methods
setMethod("initialize", "roCache_versionBase", function(.Object, ..., dFile, cFolder, syncAllowed, version, previousVersion, RDSversion) {
  if (options("cache.debug")[[1]]==TRUE) 	print("Start init base")
  
	if ( !missing(dFile) )           .Object@dFile           = dFile
	if ( !missing(cFolder) )         .Object@cFolder         = cFolder
	if ( !missing(syncAllowed) )     .Object@syncAllowed     = syncAllowed
	if ( !missing(version) )         .Object@version         = version
	if ( !missing(previousVersion))  .Object@previousVersion = previousVersion
 	if ( !missing(RDSversion))       .Object@RDSversion      = RDSversion

	dFile_existsAsFolder   = dir.exists(dFile)
	cFolder_existsAsFolder = dir.exists(cFolder)

	dFile_existsAsFile   = file.exists(dFile)   & dFile_existsAsFolder==FALSE
	cFolder_existsAsFile = file.exists(cFolder) & cFolder_existsAsFolder==FALSE
	
	if (dFile == cFolder)           stop(simpleError("dFile and cFolder cannot refer to the same path."))
	if (dFile_existsAsFolder==TRUE) stop(simpleError("dFile cannot refer to an existing folder. Instead define it as a file with .rds extention."))
	if (cFolder_existsAsFile==TRUE) stop(simpleError("cFolder cannot refer to an existing file. Instead define it as a path to a folder."))
	if (options("cache.debug")[[1]]==TRUE) 	print("End init base")
  .Object
})


#' @import methods
setValidity("roCache_versionBase",
	function(object) {
		##At this point the cache must exist and contain the correct version
		##i.e. the handler should have incremented if applicable
		
		if (options("cache.debug")[[1]]==TRUE) 	print("Start validation base")
		
		errs<-vector()
		
		dFile               = get_dFile(object)
		cFolder             = get_cFolder(object)
		syncAllowed         = get_syncAllowed(object)
		version						  = get_version(object)
		prevVersion         = get_prevVersion(object)
		RDSversion          = get_RDSversion(object)

		

		dFile_existsAsFolder   = dir.exists(dFile)
		cFolder_existsAsFolder = dir.exists(cFolder)

		dFile_existsAsFile   = file.exists(dFile)   & dFile_existsAsFolder==FALSE
		cFolder_existsAsFile = file.exists(cFolder) & cFolder_existsAsFolder==FALSE

		if (length(dFile) != 1 )       errs<-c(errs, "dFile must point to exactly one file.")
		if (length(cFolder) != 1 )     errs<-c(errs, "cFolder must point to exactly one folder.")
		if (length(syncAllowed) != 1 ) errs<-c(errs, "syncAllowed must contain exactly one logical value.")
		if (length(version) != 1 )     errs<-c(errs, "version must contain exacly one version.")
		if (length(RDSversion) != 1 )  errs<-c(errs, "RDSversion must contain exactly one version.")
		if (length(prevVersion) !=1 )  errs<-c(errs, "prevVersion must contain exactly one version.")


		if (length(errs)>0) {
			if (options("cache.debug")[[1]]==TRUE) 	print("End validation base")
			return(paste0(errs,collapse="\n"))
		}

		if (is.na(dFile))       errs<-c(errs, "dFile cannot by NA.")
		if (is.na(cFolder))     errs<-c(errs, "cFolder cannot by NA.")
		if (is.na(syncAllowed)) errs<-c(errs, "syncAllowed cannot by NA.")
		if (is.na(version))     errs<-c(errs, "version cannot by NA.")
		if (is.na(RDSversion))  errs<-c(errs, "RDSversion cannot by NA.")
		if (is.na(prevVersion)) errs<-c(errs, "prevVersion cannot by NA.")

		if (length(errs)>0) {
			if (options("cache.debug")[[1]]==TRUE) 	print("End validation base")
			return(paste0(errs,collapse="\n"))
		}

		if (prevVersion>= version)      errs<-c(errs, "prevVersion must be smaller than version.")
		
				
  	if (dFile == cFolder)           errs<-c(errs,"dFile and cFolder cannot refer to the same path.")
	  if (dFile_existsAsFolder==TRUE) errs<-c(errs,"dFile cannot refer to an existing folder. Instead define it as a file with .rds extention.")
	  if (cFolder_existsAsFile==TRUE) errs<-c(errs,"cFolder cannot refer to an existing file. Instead define it as a path to a folder.")
	  
 	  if (dFile_existsAsFile   ==FALSE)   errs<-c(errs,"dFile could not be initialized.")
 	  if (cFolder_existsAsFolder ==FALSE) errs<-c(errs,"cFolder was not created.")
 	
 			
		if (length(errs)>0) {
			if (options("cache.debug")[[1]]==TRUE) 	print("End validation base")
			return(paste0(errs,collapse="\n"))
		}
	
		dFile_version = .getVersionFromDigestFile(dFile)
		if (dFile_version != version) {
			if (dFile_version == prevVersion) {
				errs<-c(errs,"Coding bug DCF32F92CCD. Somehow the handlerClass has not incremented the dFile version!.")
			} else {
				errs<-c(errs,paste0("Initialized an handerClass for version ", version, " with an incompatible dFile version ", dFile_version,"."))
			}
		}
		
		if (options("cache.debug")[[1]]==TRUE) 	print("End validation base")
		if (length(errs)>0) {return(paste0(errs,collapse="\n"))}
		return(TRUE)
})

#' @import methods
setMethod("show", "roCache_versionBase", function(object){
	cat ("Cache Handle:\n")
	cat (paste0("\tdFile       : ", get_dFile(object),"\n"))
	cat (paste0("\tcFolder     : ", get_cFolder(object),"\n"))
	listedIDs<-listCache(object)
	sizeInBytes<-lapply(listedIDs, function(id){
			file.size(readCache(object, id=id, returnType="filename"))
		}) |> unlist() |> sum()
	cat (paste0("\tn objects   : ",length(listedIDs)," (", .format_bytes(sizeInBytes),")\n"))
	cat (paste0("\tsyncAllowed : ", get_syncAllowed(object),"\n"))
})

#' @rdname get_dFile
#' @aliases get_dFile.roCache_versionBase
#' @import methods
#' @export
setMethod("get_dFile", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@dFile)
})

#' @rdname get_cFolder
#' @aliases get_cFolder.roCache_versionBase
#' @import methods
#' @export
setMethod("get_cFolder", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@cFolder)
})

#' @rdname get_syncAllowed
#' @aliases get_syncAllowed.roCache_versionBase
#' @import methods
#' @export
setMethod("get_syncAllowed", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@syncAllowed)
})

#' @rdname get_version
#' @aliases get_version.roCache_versionBase
#' @import methods
#' @export
setMethod("get_version", "roCache_versionBase", function(cacheHandle,...){
	return(package_version(cacheHandle@version))
})

#' @rdname get_prevVersion
#' @aliases get_prevVersion.roCache_versionBase
#' @import methods
#' @export
setMethod("get_prevVersion", "roCache_versionBase", function(cacheHandle,...){
	return(package_version(cacheHandle@previousVersion))
})
	
#' @rdname get_RDSversion
#' @aliases get_RDSversion.roCache_versionBase
#' @import methods
#' @export
setMethod("get_RDSversion", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@RDSversion)
})
