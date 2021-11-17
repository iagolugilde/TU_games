#' @title Least Core vertices
#' @description This function returns the vertices of the least core of the TU game.
#' @param v Characteristic function in binary order.
#' @return The vertices of the least core.
#' @details Given \eqn{\epsilon\in R^n}{\epsilon in R^n}, the \eqn{\epsilon-}core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
#'  such that \eqn{x(S)\ge v(S)-\epsilon} for all coalition \eqn{S} in \eqn{N}, where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#'  \deqn{C_{\epsilon}(N,v)=\{x\in I(N,v):x(S)\ge v(S)-\epsilon\; \forall S\in N,\epsilon\in R^n\}}{C\epsilon(N,v)={x in I(N,v):x(S)\ge v(S)-\epsilon for all S in N, \epsilon in R^n}}
#'
#'  The Least core, \eqn{LC(N,v)}, is the intersection of all the non-empty \eqn{\epsilon-}cores.
#'
#'  \deqn{LC(N,v)=\cap_{\epsilon\in R^n:C_{\epsilon}(N,v)\not=\emptyset}C_{\epsilon}(N,v)}{}
#'
#'  The dimension of the least core is \eqn{|N|-2}.
#' @examples
#' v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
#' LeastCoreVertices(v)
#' @references Maschler, M., Peleg, B., & Shapley, L. S. (1979). Geometric properties of the kernel, nucleolus, and related solution concepts. Mathematics of operations research, 4(4), 303-338.
#' @seealso \link{CoreCoverVertices}, \link{CoreVertices}, \link{EpsilonCoreVertices}, \link{ImputationVertices}, \link{LeastCoreSet}, \link{WeberVertices}
#' @export

LeastCoreVertices<-function(v){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  vlex=Bin2lex(v)
  C=cbind(-Baselex(n)[1:(m-1),],rep(-1,m-1))
  b=as.matrix(-vlex[1:(m-1)])
  Ceq=c(rep(1,n),0)
  beq=vlex[m]
  cc=c(rep(0,n),1)
  lb=rep(-Inf,n+1)
  ub=rep(Inf,n+1)
  #Ceq=as.matrix(Ceq)
  restricciones=c(rep("<=",dim(b)[1]),rep("==",length(beq)))
  l=dim(C)[1]
  C=rbind(C,Ceq)
  beq=as.matrix(beq)
  b=rbind(b,beq)
  a=Rglpk_solve_LP(
    cc, C, restricciones, b,
    bounds = list( lower = list( ind=seq(1L:(n+1)), val=lb ),
                   upper = list( ind=seq(1L:(n+1)), val=ub ) ),
    max=F,  )
  epsilon=-a$optimum
  vertices=EpsilonCoreVertices(v,epsilon)
  return(list(vertices=vertices,epsilon=epsilon))
}
