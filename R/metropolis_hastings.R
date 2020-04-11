## library("EMC")

`rp` <- function(n, H, startp=NULL, fcm=NULL, fcv=NULL,SMALL=1e-6, l=loglik,...){

  myfunc <- function(ignore){
    logTarDensFunc <- function(draw,...) {
      s <- size(H)
      UI <- rbind(diag(nrow = s - 1), -1, fcm)
      CI <- c(rep(SMALL, s - 1), -1 + SMALL, fcv)
      if(any(UI %*% draw < CI)){
        return(-Inf)
      } else {
         return(l(draw,H))
      }
    }
    
    proposalSD <- rep(0.01,size(H)-1)
    propNewFunc <- function(block, currentDraw, ...) {
      proposalDraw <- currentDraw
      proposalDraw[block] <- rnorm(1, currentDraw[block], proposalSD[block])
      proposalDraw
    }
    list(logTarDensFunc = logTarDensFunc, propNewFunc = propNewFunc)
  }

  if(is.null(startp)){startp <- indep(maxp(H))}
  out <-
    with(myfunc(),
         EMC::randomWalkMetropolis(nIters         = n,
                                   startingVal    = startp,
                                   logTarDensFunc = logTarDensFunc,
                                   propNewFunc    = propNewFunc,
                                   verboseLevel   = 0))
    
  out <- out$draws
  out <- cbind(out,1-rowSums(out))
  if(!identical(pnames(H),NA)){
      colnames(out) <- pnames(H)
  }
  return(out)
}
