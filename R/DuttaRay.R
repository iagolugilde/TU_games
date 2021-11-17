#' @title Dutta-Ray value
#' @description The Dutta-Ray value of a TU game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return  Computes the Dutta-Ray value of a TU game given by vector \eqn{v}.
#' @details Given a TU game \eqn{(N,v)}, the Dutta-Ray solution, \eqn{DT(v)}, is the  solution which selects a core allocation which maximizes
#' the Lorenz ordering.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' DuttaRay(v)
#' @references Dutta, B., & Ray, D. (1989). A concept of egalitarianism under participation constraints. Econometrica: Journal of the Econometric Society, 615-635.
#' @seealso \link{CumulativeAwardsCurve}, \link{DeviationIndex}, \link{LorenzDominance}, \link{PlotSolution}
#' @export

DuttaRay=function(v,name=FALSE){
  if(name==TRUE){
    solution="Dutta Ray"
    return(solution)
  }
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
DR=rep(0,n)
base=Basebin(n)
vN=v[m]
while(sum(DR)!=vN){
  e=v/rowSums(base)
  S=max(which(e==max(e)))
  DR=DR+base[S,]*e[S]
  if(sum(DR)<vN){
  S1=base[S,]
  B=t(t(base)+S1)
  B[B==2]=1
  for(i in 1:n){
    B[,i][B[,i]==1]=i
  }
  v1=rep(0,m)
  for(j in 1:m){
    v1[j]=v[Coalitionnumber(B[j,][B[j,]!=0],n)]
  }
  v=v1-v[S]
}}
return(DR)}
