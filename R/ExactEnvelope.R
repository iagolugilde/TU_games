#' @title Exact envelope
#' @description It returns the exact envelope (exactification) of a balanced TU game.
#' @param v Characteristic function in binary order.
#' @return Let \eqn{v} the characteristic function of a balanced TU game,
#' it returns the characteristic function of its exact envelope. If \eqn{v} has an empty core then v=c().
#' @details Given a TU game \eqn{(N,v)} its exact envelope \eqn{(N,v^E)}{(N,vE)} is defined by \eqn{v^E(S)=min\{x(S):x \in C(N,v)\}}{vE(S)=min{x(S):x in C(N,v)}}, for
#' every coalition \eqn{S} of the set of players \eqn{N}. Then, \eqn{v^E}{vE} is the only
#' exact game whose core coincides with the core of \eqn{v}.
#' @importFrom Rglpk Rglpk_solve_LP
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ExactEnvelope(v)
#' @seealso \link{ExactFacesGames}, \link{FacesGames}
#' @export
ExactEnvelope=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  exact=c()
  base=Basebin(n)
  lb=rep(-Inf,n)
  ub=rep(Inf,n)
  A=-base[-nC,]
  b=-v[-nC]
  b=as.matrix(b)
  Aeq=rep(1,n); beq=v[nC];
  restricciones=c(rep("<=",length(b)),rep("==",length(beq)))
  A=rbind(A,Aeq)
  beq=as.matrix(beq)
  b=rbind(b,beq)

  for(ii in 1:nC){
    f=base[ii,]
    a=Rglpk_solve_LP(
      f, A, restricciones, b,
      bounds = list( lower = list( ind=seq(1L:(n)), val=lb ),
                     upper = list( ind=seq(1L:(n)), val=ub ) ),
      max=F,  )
    c=a$optimum
    exact=c(exact,c)
    }
return(exact)}
