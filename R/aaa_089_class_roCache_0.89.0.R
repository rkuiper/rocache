#' @import methods
setClass("roCache_0.89.0",
		contains = c("roCache_versionBase") 
)

#' @import methods
setValidity("roCache_0.89.0",
	function(object) {
		if (options("cache.debug")[[1]]==TRUE) 	print("Start validation 0.89.0")
		errs<-vector()
		expectedVersion = gsub("^roCache_([0-9\\.]+)$","\\1", class(object))
		if (!all(get_version(object) == expectedVersion)) errs<-c(errs,paste("Incorrectly set 'version': ",object@version)) #By definition 
		if (!all(get_prevVersion(object) == c("0.0.0"))) errs<-c(errs,"Incorrectly set 'previousVersion'") #By definition 
		if (!all(get_RDSversion(object) == c(3L)))    errs<-c(errs,"Incorrectly set 'RDSversion'") #By definition 

		if (options("cache.debug")[[1]]==TRUE) 	print("End validation 0.89.0")		
		
		if (length(errs)>0) {return(paste0(errs,collapse="\n"))}
		return(TRUE)
})


#Define the initialize method for the class
#' @import methods
setMethod("initialize", "roCache_0.89.0", function( .Object,  dFile, cFolder, syncAllowed = FALSE ) {
	if (options("cache.debug")[[1]]==TRUE) 	print("Start init 0.89.0")
	
	.Object<-do.call("callNextMethod",	args=c( ##Call parent class
		.Object     = .Object, 
		dFile       = dFile,
		cFolder     = cFolder,
		syncAllowed = syncAllowed,
		version     = "0.89.0",  
		RDSversion  = 3L, 
		previousVersion = "0.0.0")) ##Place holder version (non existent)

	dFile_existsAsFolder   = dir.exists(dFile)
	cFolder_existsAsFolder = dir.exists(cFolder)

	dFile_existsAsFile   = file.exists(dFile)   & dFile_existsAsFolder==FALSE
	cFolder_existsAsFile = file.exists(cFolder) & cFolder_existsAsFolder==FALSE
	
	if (dFile_existsAsFile == FALSE)     .createEmpty_dFile(.Object); 
	if (cFolder_existsAsFolder == FALSE) .createEmpty_cFolder(.Object)

	validObject(.Object)
	
	if (options("cache.debug")[[1]]==TRUE) 	print("End init 0.89.0")
  .Object
})






#########################################################
## .createEmpty_dFile
##
#' @import methods
setMethod(".createEmpty_dFile", "roCache_0.89.0", function(cacheHandle, ...) {
	if (options("cache.debug")[[1]]==TRUE) 	print("Start .createEmpty_dFile")

	dFile      = get_dFile(cacheHandle)
	version    = get_version(cacheHandle)
	RDSversion = get_RDSversion(cacheHandle)

	dFileExists   <- file.exists( dFile  ) 
	dFileIsFolder <- dir.exists(  dFile  )

	if (dFileIsFolder) stop(simpleError(message("dFile points to a folder")))
	if (dFileExists)   stop(simpleError(message("dFile already exists")))

	empty_dFile<-list(
		"VERSION" = version, 
		"FILEIDS" = list()
	)

  saveRDS(
  	object   = empty_dFile, 
  	file     = dFile, 
  	ascii    = TRUE, 
  	compress = FALSE, 
  	version  = RDSversion)
	if (options("cache.debug")[[1]]==TRUE) 	print("End .createEmpty_dFile")

	return(TRUE)

})

#########################################################
## .createEmpty_cFolder
##
#' @import methods
setMethod(".createEmpty_cFolder", "roCache_0.89.0", function(cacheHandle, ...) {
	if (options("cache.debug")[[1]]==TRUE) 	print("Start .createEmpty_cFolder")

	cFolder = get_cFolder(cacheHandle) 
	cFolderExists   <- dir.exists( cFolder ) 
	if (cFolderExists)   stop(simpleError(message("cFolder already exists")))
	dir.create( cFolder,	showWarnings=TRUE, recursive=TRUE )
	if (options("cache.debug")[[1]]==TRUE) 	print("End .createEmpty_cFolder")
	return(TRUE)
})


#########################################################
## .incrementVersion
##
#' @import methods
setMethod(".incrementVersion",  "roCache_0.89.0",
	function(	cacheHandle, ... ) {
		#No previous versions exists!
		stop(simpleError(paste0(".incrementVersion of cacheHandle ", get_version(cacheHandle),". However, no earlier versions exist!")))
	}
)

#########################################################
## readCache
##
#' @rdname readCache
#' @aliases readCache.roCache_0.89.0
#' @import methods
#' @export
setMethod( "readCache", "roCache_0.89.0", 
	function( cacheHandle, id, returnType = c("contents", "outputDigest", "inputDigests", "filename"), ...)
{
	
	if (missing(returnType)) returnType = c("contents")

 	
	returnType<-match.arg(returnType, c("contents", "outputDigest", "inputDigests", "filename") )

	dFile       = get_dFile(cacheHandle)
	cFolder     = get_cFolder(cacheHandle)
	
	if (length(id) !=1 ) stop(simpleError("Please provide a single value to argument 'id'."))

	
	if (is.na(id) ) stop(simpleError("'id' cannot be NA."))
	

	flag = 0
	F_NOEXPECTEDENTRY_INPUT = 2
	F_NOEXPECTEDENTRY_OUTPUT = 4
	F_NOCACHEFILE= 8
	F_MULTIPLECACHEFILES= 16
			
	#First obtain relevant file in cFolder
	f<-list.files(cFolder, pattern=paste0("^",id,"_[a-z0-9]+\\.rds"), full.names = TRUE,ignore.case = TRUE);
	if (length(f)==0) { flag = flag + F_NOCACHEFILE }
	if (length(f)>1)  { flag = flag + F_MULTIPLECACHEFILES }
	
	##Read dFile
	contents_dFile<-readRDS(dFile)
	if (is.null(contents_dFile[["FILEIDS"]][[id]][["inputDigests"]])) {
	  flag = flag + F_NOEXPECTEDENTRY_INPUT
	}
	if (is.null(contents_dFile[["FILEIDS"]][[id]][["outputDigest"]])) {
	  flag = flag + F_NOEXPECTEDENTRY_OUTPUT
	}
	
	##Assess flag value
	if (flag !=0 ) {
	  if ( flag %in% c( #Some normal conditions
	    F_NOEXPECTEDENTRY_INPUT + F_NOEXPECTEDENTRY_OUTPUT + F_NOCACHEFILE, #cache file not exists, and no entry for this fileID in dFile
	    F_NOCACHEFILE) #cache file not exists, but entry for this fileID in dFile does exist (e.g. after removing the cache file)
	    ) {
	    	stop(simpleError(paste0("found no cache files for ", id)))
	  } else  {
	       
    	if (F_NOEXPECTEDENTRY_INPUT + F_NOEXPECTEDENTRY_OUTPUT ) {
    		warning("Ran synchronizeCache on the cFolder because a cache file without associated dFile entry was found for fileID: ",id,".")
    		synchronizeCache(cacheHandle, what = "cFolder")
    	} else{
	    	##Always report error if structure of cache files has been violated
		    errMessages="Violation of cache structure! This could have been caused by interuption during writing cache. The following elements were found to be wrong:"
		    if (bitwAnd(flag, F_NOEXPECTEDENTRY_INPUT)){
		      errMessages<-c(errMessages,paste0("No input cache entry for ", id," was found in ", dFile,"."))
		    }
		    if (bitwAnd(flag, F_NOEXPECTEDENTRY_OUTPUT)){
		      errMessages<-c(errMessages,paste0("No output cache entry for ", id," was found in ", dFile,"."))
		    }
			  
			  if (bitwAnd(flag, F_MULTIPLECACHEFILES)){
			    errMessages<-c(errMessages,paste0("Multiple cache files for ", id," were found in ", dFile,"."))
			  }
		    stop(simpleError(paste0(errMessages,collapse="\n\t")))
			}	    
	  }
	}
	
	if (returnType=="filename"){
	  return(f)
	}
	
	if ( returnType=="contents" ){
	  localEnv<-readRDS(f)
	  
	  if (!is.environment(localEnv)) { 
	    stop(simpleError(paste0("Not a cache file: ", f)))
	  } else if (!all(c("object")%in%ls(localEnv))) {
	    stop(simpleError(paste0("Not a cache file: ", f)))
	  }
	  return( get("object",localEnv) )
	} else if ( returnType=="inputDigests" ){
	  return(contents_dFile[["FILEIDS"]][[id]][["inputDigests"]])
	} else if ( returnType=="outputDigest" ){
	  return(contents_dFile[["FILEIDS"]][[id]][["outputDigest"]])
	} else {
	  stop(simpleError("Run-time error: FA3E7A3D34.")) #Should not be possible
	}
})


#########################################################
## listCache
##

#' @rdname listCache
#' @aliases listCache.roCache_0.89.0
#' @import methods
#' @export
setMethod( "listCache", "roCache_0.89.0", 
	function( cacheHandle, ... ) {
		dFile = get_dFile(cacheHandle)
		return( names(readRDS(dFile)[["FILEIDS"]]) )
})



#########################################################
## removeCache
##

#' @rdname removeCache
#' @aliases removeCache.roCache_0.89.0
#' @import methods
#' @export
setMethod( "removeCache", "roCache_0.89.0", 
	function( cacheHandle, ids, ... ) {
	
	dotList<-list(...)


	dFile               = get_dFile(cacheHandle)
	cFolder             = get_cFolder(cacheHandle)
	RDSversion          = get_RDSversion(cacheHandle)
	dFileEntryMustExist <- if (is.null(dotList[["dFileEntryMustExist"]])) { TRUE } else { dotList[["dFileEntryMustExist"]] }

	##Note 0 or more ids should be allowed
	
	##Read dFile
	contents_dFile<-readRDS(dFile)
	
	##get the cache file in cFolder (some/all might be absent)
  f<-list.files(cFolder, pattern=paste0("^(.+)_[a-z0-9]+\\.rds"), full.names = TRUE,ignore.case = TRUE);
	f_base<-basename(f)
	cacheFiles_cFolder<- gsub("(^.+)_[a-z0-9]+\\.rds", "\\1",f_base)
	f<- f[cacheFiles_cFolder%in%ids] 
	
	if (dFileEntryMustExist==TRUE) {
		if ( !all(ids %in% names(contents_dFile[["FILEIDS"]]) ) ) {
			delta<-setdiff(ids , names(contents_dFile[["FILEIDS"]]) )
			stop(simpleError(paste0("No entries in ",dFile," where found for the following file(s) requested to be removed: ",paste(delta,collapse = ","))))
		} 
	}
		
	contents_dFile[["FILEIDS"]] <- contents_dFile[["FILEIDS"]][which(!names(contents_dFile[["FILEIDS"]])%in%ids)]

	#Remove requested file(s) from cFolder
	unlink(f)
		
	#And save the dFile
  saveRDS(object = contents_dFile, file = dFile, ascii = TRUE, compress = FALSE, version=RDSversion)
  
  return(TRUE)
})

#########################################################
## synchronizeCache
##

#' @rdname synchronizeCache
#' @aliases synchronizeCache.roCache_0.89.0

#' @import methods
#' @export
setMethod( "synchronizeCache", "roCache_0.89.0", 
	function( cacheHandle , what = c("cFolder","both","dFile"), ... ) 
{

	dFile               = get_dFile(cacheHandle)
	cFolder             = get_cFolder(cacheHandle)
	RDSversion          = get_RDSversion(cacheHandle)
	syncAllowed         = get_syncAllowed(cacheHandle)

	what <- match.arg(what)

	##Read dFile
	contents_dFile<-readRDS(dFile)
	dFile_entries<- names(contents_dFile[["FILEIDS"]])
	
	##list cache files in cFolder
  f<-list.files(cFolder, pattern=paste0("^.+_[a-z0-9]+\\.rds"), full.names = TRUE,ignore.case = TRUE);
  f_base<-basename(f)
	cacheFiles_cFolder<- gsub("(^.+)_[a-z0-9]+\\.rds", "\\1",f_base)
	
	
	##Which cFolder files have no associated dFile entry:
	delta1<-setdiff(cacheFiles_cFolder, dFile_entries)
	##Which dFile entries have no associated cFolder file
	delta2<-setdiff(dFile_entries, cacheFiles_cFolder)


	msg = vector()
		
	if (what %in% c("cFolder","both")) {
		if (length(delta1)>0 ){
			#Remove files in cFolder without entry in dFile
			if (syncAllowed==TRUE) {
				removeCache( cacheHandle, ids = delta1, dFileEntryMustExist = FALSE )
				msg<-c(msg,"Sync: Removed files from cFolder:\n", paste0("\t",delta1,"\n"))
			}	else {
				msg<-c(msg,"Found files in cFolder that have no associated dFile entry:\n", paste0("\t",delta1,"\n"))
			}

		}
	
	} else if (what %in%c("dFile","both") ) {
		if ( length(delta2)>0 ) {
			if (syncAllowed==TRUE) {
				removeCache( cacheHandle, ids = delta2 )
				msg<-c(msg,"Sync: Removed entries from dFile:\n", paste0("\t",delta2,"\n"))
			} else {
				msg<-c(msg,"entries in dFile without associated dile in cFolder:\n", paste0("\t",delta2,"\n"))
			}
		}
	} else {
		stop(simpleError("Coding bug #A8EDF209812 Show not be possible."))
	}

	return(msg)
})




#########################################################
## storeCache
##


#' @rdname storeCache
#' @aliases storeCache.roCache_0.89.0
#' @export
#' @import methods
setMethod( "storeCache", "roCache_0.89.0", 
	function( cacheHandle, ..., id, funcHandle = defaultFuncHandle,  overwrite = FALSE, returnType = c("outputDigest","contents","inputDigests","filename") ) {

	dFile               = get_dFile(cacheHandle)
	cFolder             = get_cFolder(cacheHandle)
	RDSversion          = get_RDSversion(cacheHandle)

	if (missing(returnType)) returnType = c("outputDigest")
	if (missing(overwrite))   overwrite = FALSE
	if (missing(funcHandle)) funcHandle = defaultFuncHandle

  returnType<-match.arg(returnType, c("outputDigest","contents","inputDigests","filename"))
  	
	if (length(id)         !=1 ) stop("Please provide a single value to argument 'id'.")
	if (length(funcHandle) !=1 ) stop("Please provide a single value to argument 'funcHandle'.")
	if (length(overwrite)  !=1 ) stop("Please provide a single value to argument 'overwrite'.")
	
	if (is.na(id))                stop("'id' cannot be NA.")
	if (is.na(overwrite))         stop("'overwrite' cannot be NA.")
	if (!is.function(funcHandle)) stop("'funcHandle' must be a function reference.")
	

  ##Request the digest via funcHandle(tmpFileName=NULL, getDigests=TRUE, ...)
  ##Checks if the id already exists. iff so, do nothing
  #calls the funcHandle(tmpfilename, getDigests=FALSE, ....) which processes and saves the tmpfile
  ##if succeeded, remove any previous digests for id
	curInputDigests <- 
		tryCatch( 
			expr = {  funcHandle( getDigests=TRUE, ... ) },
			error = function(e) {
				error_message <- paste0("While calling the function handle with getDigests=TRUE: ", conditionMessage(e))
				stop(simpleError(error_message))
			})
					

  if ( !is.list(curInputDigests) ) {
    stop(simpleError("The storage funcHandle does not return a named digest list."))
  } else if (length(curInputDigests)==0){
    stop(simpleError("The storage funcHandle returns an empty digest list."))
  } else if (is.null(names(curInputDigests)) || (length(unique(names(curInputDigests)))!= length(curInputDigests))) {
    stop(simpleError("The storage funcHandle returns an unnamed digest list, or names are not unique."))
  } else if ("funcHandle" %in% names(curInputDigests))  {
    stop(simpleError("The storage funcHandle returns a digest list with a reserved item name 'funcHandle'."))
  } 
  
  curInputDigests[["funcHandle"]] = digestOfFunc(funcHandle)
  
  ##Read dFile
	contents_dFile<-readRDS(dFile)
      
  #If this is the first time, record it in the contents_dFile
  if (is.null(contents_dFile[["FILEIDS"]][[id]]))   contents_dFile[["FILEIDS"]][[id]]<-list("inputDigests" = curInputDigests)
  
      
  curDigest = digest(unlist(curInputDigests, use.names = FALSE))
  expDigest = digest(unlist(contents_dFile[["FILEIDS"]][[id]][["inputDigests"]],use.names = FALSE))
  
  if ( (expDigest != curDigest) && (overwrite == FALSE) ) {
    #Only store if curDigest == expectedDigest
  
    errMessages=paste0("The observed inputDigest (",curDigest,") does not match the earlier created expected inputDigest (",expDigest,").")
  
    prevInputDigests<-contents_dFile[["FILEIDS"]][[id]][["inputDigests"]]
    
    if (length(prevInputDigests)>0) {
      
      inters <- intersect( names(curInputDigests),  names(prevInputDigests))
      diff1  <- setdiff(   names(curInputDigests),  names(prevInputDigests))
      diff2  <- setdiff(   names(prevInputDigests), names(curInputDigests))

      if ( length(diff1) > 0 ) {
        errMessages<-c(errMessages, paste0("Found the following ", length(diff1)," new key(s) not present in earlier version: ", paste(diff1, collapse=", ")))
      }
      if ( length(diff2) > 0 ) {
        errMessages<-c(errMessages, paste0("Found the following ", length(diff2)," key(s) is earlier version not present in current version: ", paste(diff2, collapse=", ")))
      }
      
      if ( length(inters) > 0 ) {
      	cur = data.frame(key = names(curInputDigests),value = unlist(curInputDigests))[inters,,drop=FALSE]
	      prev = data.frame(key = names(prevInputDigests),value = unlist(prevInputDigests))[inters,,drop=FALSE]

				discrepant<-inters[which(cur[["value"]]!=prev[["value"]])]
				
        if ( length(discrepant) > 0 ){
          errMessages<-c(errMessages, paste0("Found different digest between current and previous version, for the following ", length(discrepant)," key(s) : ", paste(discrepant, collapse=", ")))
        }
      }
    } 
    errMessages = c( errMessages, "Consider setting overwrite=TRUE, to overwrite the current cache.")
    stop(simpleError(paste0(errMessages,collapse="\n")))
  }
  
  ##Set the inputDigests in the digest file
  contents_dFile[["FILEIDS"]][[id]][["inputDigests"]]<-curInputDigests
  

  ##make the id into an actual filePath:
  cur_tmpFile = paste0(cFolder,"/",id,"_",curDigest,".rds")
  ##And a file to write to if an error occurs in the output file
  err_tmpFile = paste0(cFolder,"/_",id,"_digestError.rds")

  ##Collect any potential previously stored files
	prevFile<-vector()

  tryCatch( ##Does not have to exist
  	expr = {
			prevFile<-readCache(cacheHandle, id  = id, returnType = "filename")
		}, error = function(e) {
			if (! grepl("found no cache files for",conditionMessage(e))) {
				stop(simpleError(e))
			}
		}
	)

  if (length(prevFile)==0 || cur_tmpFile!=prevFile) { ##I.e. if first run or inputDigest changed
		objectToStore =	tryCatch(
			expr = {  funcHandle(getDigests=FALSE, ... ) }, 
			error = function(e) {
			
				error_message <- paste0("While calling the function handle with getDigests=FALSE: ", conditionMessage(e))
				stop(simpleError(error_message))
			}
		)

    localEnv = new.env()
    localEnv[["object"]]  = objectToStore
    
    obsOutputDigest<-digest(objectToStore)
    if (is.null(contents_dFile[["FILEIDS"]][[id]][["outputDigest"]])){ #If first time
      contents_dFile[["FILEIDS"]][[id]][["outputDigest"]]<-obsOutputDigest
    }
    if ( ( contents_dFile[["FILEIDS"]][[id]][["outputDigest"]] != obsOutputDigest ) && (overwrite == FALSE) ) {
      #Only store if output digest has not changed
      errMessages=paste0("The observed outputDigest (",obsOutputDigest,") does not match the earlier created expected outputDigest (",contents_dFile[["FILEIDS"]][[id]][["outputDigest"]] ,"). The output is written to a temp file for inspection: ", err_tmpFile)
      errMessages = c( errMessages, "Consider setting overwrite=TRUE, to overwrite the current cache.")
      saveRDS(object = localEnv, file = err_tmpFile, version=RDSversion)
      stop(simpleError(errMessages))
    }
    
    ##Set the outputDigest in the digest file
    contents_dFile[["FILEIDS"]][[id]][["outputDigest"]]<-obsOutputDigest
         
    saveRDS(object = localEnv,       file = cur_tmpFile, version=RDSversion)
    saveRDS(object = contents_dFile, file = dFile, ascii = TRUE, compress = FALSE, version=RDSversion)
    unlink(prevFile)
  }

  return(readCache(
  	cacheHandle,
  	id          = id,
  	returnType  = returnType
  	))

})


