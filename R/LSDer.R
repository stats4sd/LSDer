#' Calculating Fishers LSD value from mixed model
#' @param model An object of class \code{merMod}
#' @param term string indicating variable to calculate LSDs for
#' @param comps vector of length 2 indicating which groups to compare
#' @param level confidence level for LSD
#' @keywords LSD lmer
#' @export
#' @examples

LSDer<-function(model,term,comps=NULL,level=0.95){
  if(class(model)=="merMod"|class(model)=="merModLmerTest"|class(model)=="lmerModLmerTest"){
    if(!term%in%colnames(model@frame))
    {stop(paste("No variable called",term,"found in model",deparse(substitute(model))))}
    if(is.null(comps)){
    t1<-(table(model@frame[term]))
    ns<-cbind(median(t1),median(t1))
    rownames(ns)<-NULL
    message<-paste("LSD for comparison between two groups with",ns[1],"observations in each group")
    }
    else{
      if(!comps[1]%in%as.character(model@frame[term][,1])){
        stop(paste("No group called",comps[1],"found in variable",term))
      }
      if(!comps[2]%in%as.character(model@frame[term][,1])){
        stop(paste("No group called",comps[2],"found in variable",term))
      }
      ns<-t(matrix((table(model@frame[term]))[comps]))
      names(ns)<-NULL
      message<-paste("LSD for comparison between",comps[1],"and",comps[2])
    }
    
    MS<-sigma(model)**2
    DF<-(anova(model))[term,"DenDF"]
    NREP1<-ns[,1]
    NREP2<-ns[,2]
    LSD<-qt(1-((1-level)/2),DF)*sqrt(MS*(1/NREP1+1/NREP2))
    
    cat(message,sep = "\n")
    return(LSD)
    }
  else(stop("model not of class merMod or merModLmerTest"))
}