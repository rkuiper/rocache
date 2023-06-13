
#' defaultFuncHandle
#' 
#' The default function handle used for \code{\link{storeCache}}.
#' The func handle has one argument names object, reflecting the 
#' object to store. 
#'
#' @param getDigests (TRUE/FALSE), return the list or input digest, or return object.
#' @param object The object to return (and store in cache).
#'
#' @examples
#' \dontrun{
#' storeCache(cacheHdl, id = "ID1", object="ABCD")
#'}
#' @importFrom digest digest
#' @export
defaultFuncHandle<-function( getDigests=FALSE, object){
  internal_sort_list<-function(x){
    if (is.list(x)){
      if (!is.null(names(x))){
        x<-x[order(names(x))]
      }
      for (itemIdx in seq_along(x)){
        x[[itemIdx]]<-internal_sort_list(x[[itemIdx]])
      }
    }
    return(x)
  }
  
  ##Caches any R object
  if (getDigests==TRUE){
    d<-list("object" = digest(internal_sort_list(object)))
    return(d)
  }
  
  return(object)
}
