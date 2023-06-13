#############################################################################
### Some getters
###

#' getter: get_dFile
#'
#' A getter to obtain the dFile from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_dFile",
	def = function( cacheHandle, ...){
			standardGeneric("get_dFile")
	})
	
#' getter: get_cFolder
#'
#' A getter to obtain the cFolder from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_cFolder",
	def = function( cacheHandle, ...){
			standardGeneric("get_cFolder")
	})

#' getter: get_syncAllowed
#'
#' A getter to obtain 'syncAllowed' status from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_syncAllowed",
	def = function( cacheHandle, ...){
			standardGeneric("get_syncAllowed")
	})

#' getter: get_version
#'
#' A getter to obtain the version from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_version",
	def = function( cacheHandle, ...){
			standardGeneric("get_version")
	})
	
#' getter: get_prevVersion
#'
#' A getter to obtain the prevVersion from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_prevVersion",
	def = function( cacheHandle, ...){
			standardGeneric("get_prevVersion")
	})
		
#' getter: get_RDSversion
#'
#' A getter to obtain the RDSversion from the cacheHandle.
#'
#' @param cacheHandle The handle of interest.
#' @param ... not used
#' @import methods
setGeneric(
	name = "get_RDSversion",
	def = function( cacheHandle, ...){
			standardGeneric("get_RDSversion")
	})
	
	
#############################################################################
### Some functional methods	
###

#' @import methods
setGeneric(
	name = ".createEmpty_dFile",
	def  =  function(	cacheHandle, ... ) {
		##Do not call validObject because dFile will most likely not exist!
		standardGeneric(".createEmpty_dFile")
	})
			
#' @import methods
setGeneric(
	name = ".createEmpty_cFolder",
	def  =  function(	cacheHandle, ... ) {
		##Do not call validObject because cFolder will most likely not exist!
		standardGeneric(".createEmpty_cFolder")
	})
	
#' @import methods		
setGeneric(
	name = ".incrementVersion",
	def  =  function(	cacheHandle, ... ) {
		validObject(cacheHandle)
		standardGeneric(".incrementVersion")
	})


	
#' storeCache
#'
#' Stores data (as returned by funcHandle) in cache. If the fileID was already 
#' written previously, then by default, the cache will not be changed. This will be
#' the case, even if the fileID is not present in cFolder, but only has an dFile
#' entry. Cache will only be overwritten if setting overwrite = TRUE.  
#' Then a change in the input parameters, will trigger a rerun of the funcHandle,
#' and subsequent overwriting of existing cache. 
#'
#' A function handle must be a function with a first argument being 'getDigest' 
#' receiving either TRUE, or FALSE. If TRUE, the function must return a named 
#' list containing digests values per named item. Usually each item reflects the
#' contents of an input parameter. If FALSE, the function can
#' can process what is was intended for, and return the result, which will be 
#' stored in cache. (see \code{\link{defaultFuncHandle}})
#' 
#'
#' @param cacheHandle The handle of interest.
#' @param id name tag.
#' @param funcHandle a reference to a function. See details.
#' @param overwrite Are changes to existing cache allowed (TRUE/FALSE)?
#' @param returnType One of 'outputDigest', 'contents', 'inputDigests', 'filename'.
#' @param ... additional arguments for the funcHandle.
#'
#' @seealso \code{\link{initCache}}, \code{\link{storeCache}}, \code{\link{listCache}}, \code{\link{readCache}}, \code{\link{removeCache}} , \code{\link{synchronizeCache}},\code{\link{defaultFuncHandle}} 
#' @examples
#' #Define a function handle, usable in conjuction with \code{\link{storeCache}}.
#' myFuncHdl<- function( getDigests, arg1, arg2) {
#'   #Make sure to load required libraries inside the function
#'   library(digest) 
#'   if (getDigests==TRUE) {
#'     #Create the input digests
#'     return( list(
#'       "arg1" = digest(arg1),
#'       "arg2" = digest(arg2)
#'    ))
#'   }
#'   #Perform the intended processing
#'   result = arg1*arg2
#'   Sys.sleep(4)  ##Artificially sleep to mimic 'long' running function
#'   return(result) ##And return what should be cached
#' }
#'
#' #Create a temporary digest file and cache folder
#' dFile   = tempfile() 
#' cFolder = tempfile() 
#'
#' #Initialize cache at a non standard location
#' cacheHdl = initCache(dFile = dFile, cFolder = cFolder)
#'
#' #Store some data in cache which is generated within the funcHandle defined above. 
#' #Store new file with ID1, and return outputDigest (default)
#' storeCache(cacheHdl, 
#'   id = "ID1", 
#'   funcHandle = myFuncHdl, arg1 = 89, arg2=10)
#' #Store new file with ID2, and return contents
#' storeCache(cacheHdl, 
#'   id = "ID2", 
#'   funcHandle = myFuncHdl, arg1 = 2, arg2=4,  
#'   returnType="contents")
#' #Rerun cache file with ID2, without changing input arguments. Because input arguments have not 
#' ## changed, the funcHandle will not be rerun, and thus stored cache is not affected.  
#' storeCache(cacheHdl, 
#'   id = "ID2", 
#'   funcHandle = myFuncHdl, arg1 = 2, arg2=4)
#' \dontrun{
#' #Rerun cache file with ID2, but now with a change in input arguments. Because by default 
#' ## overwrite=FALSE, the required overwrite is rejected. 
#' storeCache(cacheHdl, 
#'   id = "ID2", 
#'   funcHandle = myFuncHdl, arg1 = 20, arg2=4)
#' }
#' #Instead set overwrite=TRUE
#' storeCache(cacheHdl, 
#'   id = "ID2", 
#'   funcHandle = myFuncHdl, arg1 = 20, arg2=4, overwrite=TRUE)
#'
#' #List fileIDs currently stored in cache
#' listCache(cacheHdl)
#'
#' #Read the contents of a file stored in cache
#' readCache(cacheHdl, id="ID1", returnType = "contents")
#'
#' #Remove a specific fileID
#' removeCache(cacheHdl, ids="ID1")
#' 
#' #Synchronize cFolder with dFile. Usually they are already synchronized
#' synchronizeCache(cacheHdl, what = "cFolder")
#' #However to illustrate synchronization, we can forcefully remove a file in cFolder
#' unlink(dir(get_cFolder(cacheHdl), pattern='ID2', full.names=TRUE))
#' #And show a dry-run
#' synchronizeCache(cacheHdl, what = "dFile")
#' #Or reinitialize the cache with syncAllowed.
#' cacheHdl = initCache(dFile = dFile, cFolder = cFolder, syncAllowed=TRUE)
#' #And perform a true run:
#' synchronizeCache(cacheHdl, what = "dFile")
#' 
#' #Clean up example
#' unlink(get_dFile(cacheHdl))
#' unlink(get_cFolder(cacheHdl), recursive=TRUE)
#'
#' @import methods
setGeneric(
	name = "storeCache",
	def = function(cacheHandle, ..., id, funcHandle, overwrite, returnType){
		validObject(cacheHandle)
		standardGeneric("storeCache")
	})
	

#' removeCache
#'
#' Removes data from cache. 
#' 
#' @param cacheHandle The handle of interest.
#' @param ids name tag to remove. 
#' @param ... not used.
#'
#' @inherit storeCache return examples seealso
#' @import methods
setGeneric(
	name = "removeCache",
	def = function( cacheHandle, ids, ...){
		validObject(cacheHandle)
		standardGeneric("removeCache")
	})


#' synchronizeCache
#'
#' Synchronizes dFile and cFolder.
#'
#' Synchonization, erases any cached id, which is not associated with an entry. 
#' For what = 'cFolder' (default), any file without an associated entry in dFile will be removed
#' For what = 'dFile', any entry without an associated file in cFolder will be removed
#' For what = 'both', both cFolder and dFile will be synchronized. Note that if 
#' syncAllowed=FALSE during initialization of the cache, only a 'dry-run' will be
#' performed, without actually removing anything.  
#' 
#' @param cacheHandle The handle of interest.
#' @param what one of 'cFolder','both','dFile'.
#' @param ... not used.
#'
#' @inherit storeCache return examples seealso
#' @import methods
setGeneric(
	name = "synchronizeCache",
	def = function( cacheHandle, what, ... ){
		validObject(cacheHandle)
		standardGeneric("synchronizeCache")
	})



#' readCache
#'
#' Reads stored cache file.
#'
#' 
#' @param cacheHandle The handle of interest.
#' @param id The ID of the stored file. (see \code{\link{listCache}}.
#' @param returnType one of 'contents', 'outputDigest', 'inputDigests', 'filename'.
#' @param ... not used.
#'
#' @inherit storeCache return examples seealso
#' @import methods
setGeneric(
	name = "readCache",
	def  =  function(	cacheHandle, id, returnType, ...){  
		validObject(cacheHandle) 
		standardGeneric("readCache")
	})
	

#' listCache
#'
#' List filename IDs currently stored in cache (i.e. with dFile entry).
#'
#' 
#' @param cacheHandle The handle of interest.
#' @param ... not used.
#'
#' @inherit storeCache return examples seealso
#' @import methods
setGeneric(
	name = "listCache",
	def  =  function(	cacheHandle, ... ){   
		validObject(cacheHandle)
		standardGeneric("listCache")
	})


