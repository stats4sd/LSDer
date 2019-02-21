#' Calculating Tukey's HSD (Honestly Significant Difference) value from a mixed effects regression model fitted using \code{lme4}
#' @param model An object of class \code{merMod}
#' @param term string indicating variable to calculate HSDs for
#' @param comps (optional) vector of length 2 indicating which groups to compare
#' @param level confidence level for HSD
#' @keywords Tukey HSD lmer
#' @export
#' @examples

HSDer<-function(model,term,comps=NULL,level=0.95){
  if(class(model)=="merMod"|class(model)=="merModLmerTest"|class(model)=="lmerModLmerTest"){
    if(!term%in%colnames(model@frame))
    {stop(paste("No variable called",term,"found in model",deparse(substitute(model))))}
    t1<-(table(model@frame[term]))
    if(is.null(comps)){
    ns<-cbind(median(t1),median(t1))
    rownames(ns)<-NULL
    message<-paste("HSD for comparison between two groups with",ns[1],"observations in each group")
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
      message<-paste("Tukey HSD for comparison between",comps[1],"and",comps[2])
    }
    
    MS<-sigma(model)**2
    DF<-(anova(model))[term,"DenDF"]
    NREP1<-ns[,1]
    NREP2<-ns[,2]
    nt<-length(t1)
    HSD<-qtukey(level,nt,DF)*sqrt(MS*(0.5*(1/NREP1+1/NREP2)))
    
    cat(message,sep = "\n")
    return(HSD)
    }
  else(stop("model not of class merMod or merModLmerTest"))
}



