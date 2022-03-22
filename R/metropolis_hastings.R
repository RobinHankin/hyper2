`rp` <- function(n, H, startp=NULL, fcm=NULL, fcv=NULL,SMALL=1e-6, l=loglik,...){

  mypi <- function(p){
      s <- size(H)
      UI <- rbind(diag(nrow = s - 1), -1, fcm)
      CI <- c(rep(SMALL, s - 1), -1 + SMALL, fcv)
      if(any(UI %*% p < CI)){
        return(0)
      } else {
         return(l(p,H,log=FALSE))
      }
    }
    if(is.null(startp)){startp <- indep(equalp(H))}

    calibrator::MH(n,start=startp,sigma=diag(0.01,nrow=size(H)-1),pi=mypi)
}
