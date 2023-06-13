#' initCache
#'
#' Initialize an cache handler. 
#'
#' A cache handler has an digest file (dFile) and a cache folder (cFolder). 
#' All meta data (i.e. input/output digests) are stored in the dFile with an
#' entry per filename ID. The actual data is stored in the cFolder, named by 
#' by the merger of id tag and global input digest of the stored object.
#'
#' @param dFile path to dFile. Will be created if not existing. 
#' @param cFolder path to cFolder. Will be created if not existing. 
#' @param overwrite Overwrite cache upon change (default: FALSE)
#' @param syncAllowed Allow synchronization of cFolder (defult: FALSE).
#'
#' @inherit storeCache return examples seealso
#' @export
initCache<-function( dFile = "./dFile.rds", cFolder = "./cache", overwrite = FALSE, syncAllowed = FALSE) {
	
	##Save initial state:

	dFile_existsAsFolder   = dir.exists(dFile)
	cFolder_existsAsFolder = dir.exists(cFolder)

	dFile_existsAsFile   = file.exists(dFile)   & dFile_existsAsFolder==FALSE
	cFolder_existsAsFile = file.exists(cFolder) & cFolder_existsAsFolder==FALSE
	
	tmp_dFile_backup = tempfile()
	if (dFile_existsAsFile == TRUE) {
		file.copy(from = dFile, to = tmp_dFile_backup)
	}
	dFileModified = FALSE
	
	##Define latest dFile version
	latestKnown_dFileVersion = packageVersion_to_dFileVersion[[as.character(packageVersion(pkgname))]]

	tryCatch(	expr = {
		errs = vector()
		
		##Get actual dFile version
		if (dFile_existsAsFile==TRUE) {
			dFileVersion = .getVersionFromDigestFile(dFile)

			#Check if the dFile version exceeds latestKnown_dFileVersion
			if (package_version(dFileVersion) > package_version(latestKnown_dFileVersion)) {
				stop(simpleError(paste0("Version of cache is more recent than latest version known by package. Please upgrade package.")))
			}
			
			#If dFile version < latestKnown_dFileVersion:
			if (package_version(dFileVersion) < package_version(latestKnown_dFileVersion) ) {
				#Attempt to increment cache to most recent version
				unlist(packageVersion_to_dFileVersion)
				
				#First construct path from current latestKnown_dFileVersion to actual dFileVersion
				versionPath = sort(package_version(unique(unlist(packageVersion_to_dFileVersion))))
				startAt<-which(versionPath==dFileVersion)
				if (length(startAt)!=1) stop(simpleError(paste0("Version in dFile (",dFileVersion,") is not recognized as a known version!" )))
				startAt<-startAt+1
				#Init the handerClass of (version +1), to trigger version increment.
				for (idx in seq(startAt, length(versionPath) )){
					cacheHandler<- new( .getHandlerClasses(versionPath[idx]), dFile = dFile, cFolder = cFolder, overwrite = overwrite )
				}
				
				##Get actual dFile version	after increment
				dFileVersion = .getVersionFromDigestFile(dFile)
				if (package_version(dFileVersion) < package_version(latestKnown_dFileVersion) ) {
					stop(simpleError("Coding bug A2134FDFC233. Somehow the handlerClass has not incremented the dFile version!."))
				}

			}
		}

		cacheHandler<- new( .getHandlerClasses(latestKnown_dFileVersion), dFile = dFile, cFolder = cFolder, overwrite = overwrite )

				
		##Make sure that there are no cFolder cache files without dFile entry
	 	syncMsgs <- synchronizeCache( cacheHandler, what = "cFolder", dryRun = I(syncAllowed==FALSE) ) 
		if ( length(syncMsgs) > 0 ) { ##If there is something tobe(if dryRun) / was (if sync allowed) synchronized
			if ( syncAllowed == TRUE) { dFileModified = TRUE; message(syncMsgs); }
			if ( syncAllowed == FALSE) {
				stop(simpleError(paste0(c(syncMsgs,"\n", "Please make sure that the provided dFile and cFolder are the correct ones. If so, then consider setting syncAllowed = TRUE, which will remove the files in cFolder that have no associated entry in dFile."),collapse="")))
			}
		}
		return(cacheHandler)
	}, error = function(e) {
		##Recover initial state:
		if ( dFile_existsAsFile == TRUE && dFileModified == TRUE ) {
			file.copy( from = tmp_dFile_backup, to = dFile, overwrite=TRUE )
		} 
		if ( dFile_existsAsFile == FALSE && dFile_existsAsFolder == FALSE ) {
			unlink(dFile)
		}
		
		if ( cFolder_existsAsFile == FALSE && cFolder_existsAsFolder == FALSE ) {
			unlink(cFolder, recursive = TRUE)
		}
						
		error_message <- conditionMessage(e)
		stop(simpleError(error_message))
	}, finally = {
		unlink("tmp_dFile_backup") #Always remove the inital dFile backup
	}) ##End tryCatch
	
}

