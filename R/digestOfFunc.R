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


