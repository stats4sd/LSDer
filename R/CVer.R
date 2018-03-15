#' Calculating % coefficient of variation from mixed model
#' @param model An object of class \code{merMod}
#' @keywords CV lmer
#' @export
#' @examples

CVer<-function(model){
  if(class(model)=="merMod"|class(model)=="merModLmerTest"){
   
    GM<-mean(model@frame[,1])
    MS<-sigma(model)**2
return(100*sqrt(MS)/GM)
  }
  else(stop("model not of class merMod or merModLmerTest"))
}