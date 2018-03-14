#' Calculating % coefficient of variation from mixed model
#' @param model An object of class \code{merMod}
#' @keywords CV lmer
#' @export
#' @examples

CVer<-function(model){
  if(class(model)=="merMod"|class(model)=="merModLmerTest"){
    if(!term%in%colnames(model@frame))
    {stop(paste("No variable called",term,"found in model",deparse(substitute(model))))}
   
    GM<-mean(model@frame[,1])
    MS<-sigma(model)**2
return(100*sqrt(MS)/GM)
  }
  else(stop("model not of class merMod or merModLmerTest"))
}