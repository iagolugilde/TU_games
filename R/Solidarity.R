#' @title Solidarity value
#' @description The solidarity value of a TU game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return  Computes the solidarity value of a TU game given by vector \eqn{v}.
#' @details Given a TU game \eqn{(N,v)}, we define the average marginal contribution vectors for each coalition \eqn{S} in \eqn{N} by,
#' \deqn{A(S)=\sum_{k\in S}\frac{1}{|S|}(v(S)-v(S\backslash \{k\}))}{A(S)=\sum(v(S)-v(S\ {k})),k in S}
#'
#' The Solidarity value, \eqn{\phi(v)}, is calculated by averaging the average vectors of marginal contributions associated with all the coalitions.
#'
#' \deqn{\phi(v)=\frac{(n-|S|)!(|S|-1)!}{n!}A(S)}{\phi(v)=[(n-|S|)!(|S|-1)!A(S)]/n!}
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Solidarity(v)
#' @references Nowark,AS. & Radzik,T. (1994). A Solidarity Value for n-Person Transferable Utility Games. International Journal of Game Theory, vol. 23; 43-48
#' @seealso \link{AverageMarginal}, \link{PlotSolution}, \link{Shapley}
#' @export

Solidarity=function(v,name=FALSE){
  if(name==TRUE){
    solution="Solidarity"
    return(solution)
  }
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  AM=AverageMarginal(v)
  base=Basebin(n)
  factor=1/factorial(n)
  S=rep(0,n)
  for(i in 1:n){
    factor2=factorial(rowSums(base[which(base[,i]==1),])-1)
    factor3=factorial(n-rowSums(base[which(base[,i]==1),]))
    S[i]=factor*sum(AM[which(base[,i]==1)]*factor2*factor3)
  }

  return(S)}
