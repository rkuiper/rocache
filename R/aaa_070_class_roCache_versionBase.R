#' @import methods
setClass("roCache_versionBase",
	slots = list(
		dFile           = "character",
		cFolder         = "character",
		overwrite       = "logical",
	
		version         = "character",
		previousVersion = "character",
		rdsVersion      = "integer"
	)
)


#Define the initialize method for the class
#' @import methods
setMethod("initialize", "roCache_versionBase", function(.Object, ..., dFile, cFolder, overwrite, version, previousVersion, rdsVersion) {
  if (options("cache.debug")[[1]]==TRUE) 	print("Start init base")
  
	if ( !missing(dFile) )           .Object@dFile           = dFile
	if ( !missing(cFolder) )         .Object@cFolder         = cFolder
	if ( !missing(overwrite) )       .Object@overwrite       = overwrite
	if ( !missing(version) )         .Object@version         = version
	if ( !missing(previousVersion))  .Object@previousVersion = previousVersion
 	if ( !missing(rdsVersion))       .Object@rdsVersion      = rdsVersion

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
		
		dFile               = dFile(object)
		cFolder             = cFolder(object)
		overwrite           = overwrite(object)
		version						  = version(object)
		prevVersion         = prevVersion(object)
		rdsVersion          = rdsVersion(object)

		

		dFile_existsAsFolder   = dir.exists(dFile)
		cFolder_existsAsFolder = dir.exists(cFolder)

		dFile_existsAsFile   = file.exists(dFile)   & dFile_existsAsFolder==FALSE
		cFolder_existsAsFile = file.exists(cFolder) & cFolder_existsAsFolder==FALSE

		if (length(dFile) != 1 )       errs<-c(errs, "dFile must point to exactly one file.")
		if (length(cFolder) != 1 )     errs<-c(errs, "cFolder must point to exactly one folder.")
		if (length(overwrite) != 1 )   errs<-c(errs, "overwrite must contain exactly one logical value.")
		if (length(version) != 1 )     errs<-c(errs, "version must contain exacly one version.")
		if (length(rdsVersion) != 1 )  errs<-c(errs, "rdsVersion must contain exactly one version.")
		if (length(prevVersion) !=1 )  errs<-c(errs, "prevVersion must contain exactly one version.")


		if (length(errs)>0) {
			if (options("cache.debug")[[1]]==TRUE) 	print("End validation base")
			return(paste0(errs,collapse="\n"))
		}

		if (is.na(dFile))       errs<-c(errs, "dFile cannot be NA.")
		if (is.na(cFolder))     errs<-c(errs, "cFolder cannot be NA.")
		if (is.na(overwrite))   errs<-c(errs, "overwrite cannot be NA.")
		if (is.na(version))     errs<-c(errs, "version cannot be NA.")
		if (is.na(rdsVersion))  errs<-c(errs, "rdsVersion cannot be NA.")
		if (is.na(prevVersion)) errs<-c(errs, "prevVersion cannot be NA.")

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
	cat (paste0("\tdFile       : ", dFile(object),"\n"))
	cat (paste0("\tcFolder     : ", cFolder(object),"\n"))

	listedIDs<-listCache(object)
	sizeInBytes<-0

	if ( length(listedIDs)>0) {
		sizeInBytes<-lapply(listedIDs, function(id){
				file.size(readCache(object, id=id, returnType="filename"))
			}) |> unlist() |> sum()
	}

	cat (paste0("\tn objects   : ",length(listedIDs)," (", .format_bytes(sizeInBytes),")\n"))
	cat (paste0("\toverwrite : ", overwrite(object),"\n"))
})

#' @rdname dFile
#' @import methods
#' @export
setMethod("dFile", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@dFile)
})

#' @rdname cFolder
#' @import methods
#' @export
setMethod("cFolder", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@cFolder)
})

#' @rdname overwrite
#' @import methods
#' @export
setMethod("overwrite", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@overwrite)
})

#' @rdname overwrite
#' @import methods
#' @export
setReplaceMethod("overwrite", c("roCache_versionBase","logical"), function(cacheHandle, value){
	cacheHandle@overwrite<-value
	cacheHandle
})

	
#' @rdname version
#' @import methods
#' @export
setMethod("version", "roCache_versionBase", function(cacheHandle,...){
	return(package_version(cacheHandle@version))
})

#' @rdname prevVersion
#' @import methods
#' @export
setMethod("prevVersion", "roCache_versionBase", function(cacheHandle,...){
	return(package_version(cacheHandle@previousVersion))
})
	
#' @rdname rdsVersion
#' @import methods
#' @export
setMethod("rdsVersion", "roCache_versionBase", function(cacheHandle,...){
	return(cacheHandle@rdsVersion)
})
