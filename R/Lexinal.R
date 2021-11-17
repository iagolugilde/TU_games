#' @title Lexinal vectors
#' @description This function returns the lexinal of a TU game \eqn{(N,v)}.
#' @param v Characteristic function in binary order.
#' @return Matrix with the lexinal vectors.
#' @details Given a TU game \eqn{(N,v)} and let \eqn{\pi} an order of the players of \eqn{N} (\eqn{\Pi(N)}), the lexinal
#' \eqn{\lambda^{\pi}(v)\in R^n}{\lambda in Rn} is defined as the lexicographic maximum on \eqn{C(N,v)} with respect to \eqn{\pi}.
#' \deqn{\lambda_{\pi(k)}^{\pi}(v)=max\{x_{\pi(k)}:x\in C(N,v),x_{\pi(1)}=\lambda_{\pi(l)}^{\pi}(v)\;\forall l\in\{1,\ldots,k-1\}\}}{}
#' \eqn{\forall k\in\{1,2,\ldots,n\}.}{}
#'
#' The lexinal is recursively defined such that every player gets the maximum he can
#' obtain inside the core under the restriction that the players before him in the corresponding
#' order obtain their restricted maxima.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Lexinal(v)
#' @seealso \link{Alexia}, \link{PlotSolution}, \link{Shapley}
#' @references Tijs,S., Borm,P., Lohmann,E., & Quant,M. (2011). An average lexicographic value for cooperative games. Eur JOper Res 213:210–220
#' @export

Lexinal=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  vertices=CoreVertices(v)
  if(dim(vertices)[1]==0)
    stop('The core is an empty set',call.=F)
  f=factorial(n)
  L=matrix(0,f,n);
  Perms=perms(c(1:n))
for (k in 1:f){
  vert=round(vertices)
  P=Permutation(n,k)
  for (ii in 1:(n-1)){
    jj=which(P==ii)
    vert=vert[which(vert[,jj]==max(vert[,jj])),]
    if(length(vert)==n){break}
  }
  if(length(vert)>n){vert=unique(vert)}
  L[k,]=vert}
return(L)}
