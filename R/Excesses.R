#' @title Excesses
#' @description Computes the excesses of an allocation.
#' @param v Characteristic function in binary order.
#' @param x Vector.
#' @return excesses. The vector of excesses.
#' @return coalition. The coalitions where the excesses are attained.
#' @details Given a TU game \eqn{(N,v)} and an allocation \eqn{x}, the excess of coalition \eqn{S} in \eqn{N} with respect to \eqn{x}
#' is defined as \eqn{e(S,x)=v(S)-x(S)} where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' x=c(2.5,5,4.5)
#' Excesses(v,x)
#' @seealso \link{Nucleolus}, \link{PreNucleolus}
#' @export
Excesses=function(v,x){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC

  if(length(x)!=n)
    stop('The point does not have the same dimension as the core',call.=F)
    base=Basebin(n)
    e=-(v-t(base%*%x))[1:nC-1];
    coalitions=NULL
    for(ii in 1:(nC-1)){
      coalitions=cbind(coalitions,paste(Getplayers(ii)$S, collapse = " "))
    }
    excesos=sort(e)
    I=sort(e,index.return=T)$ix
    excesos=-excesos;
    coalicion=coalitions[I];



  return(list(excesses=excesos,coalition=coalicion))}
