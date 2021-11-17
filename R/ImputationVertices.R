#' @title imputation vertices
#' @description It gives the imputation vertices of a TU game.
#' @details Given a TU game \eqn{(N,v)}, the imputation set is defined by \eqn{I(N,v)=\{x=(x_1, \ldots , x_n) : x_i \ge v(i)\; \forall i \in N, \sum_{i \in N} x_i=v(N)\}}{I(N,v)={x=(x1, \ldots , xn) : xi \ge v(i) for all i in N, \sum xi=v(N), i in N}}.
#' It is the set of all individually rational and efficient allocations.
#' @param v Characteristic function in binary order.
#' @return The vertices of the imputation set.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ImputationVertices(v)
#' @seealso \link{CoreCoverVertices}, \link{CoreVertices}, \link{EpsilonCoreVertices}, \link{ImputationSet}, \link{LeastCoreVertices}, \link{WeberVertices}
#' @export

ImputationVertices=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  E=EssentialGame(v)$E
  D=EssentialGame(v)$D
  P=Getplayers(nC)$P
  base=v[P]
 # if(E==0){
#    vertices=c()
#  }else
  if(D==1){
    vertices=base
  }else{
    vertices=matrix(0,n,n)
    for(ii in 1:n){
      vertices[ii,1:n]=base
      vertices[ii,ii]=v[nC]-sum(v[P[-ii]])
    }
    vertices=unique(vertices)
  }
return(vertices)}
