#' @title Faces Games
#' @description Returns the faces games of a TU game.
#' @param v Characteristic function in binary order.
#' @return Faces. Matrix of \eqn{(2^n-2)\times(2^n-1)}{(2^n-2)x(2^n-1)} length.
#' The i-row of Faces is the characteristic function of the exact face game
#' corresponding to the i-th coalition \eqn{T} (according to the binary ordering). If the corresponding face of the core is empty, then it will be a row of NaN.
#' @return intermediate. Matrix of \eqn{(2^n-2)\times(2^n-1)}{(2^n-2)x(2^n-1)} length.
#' @details Given a TU game \eqn{(N,v)} we define, for each coalition \eqn{T} in \eqn{N} its face game \eqn{(N,v_T)}{(N,vT)}, by
#' \eqn{v_T(S)=v((S\cap T) \cup (N\backslash T))-v(N\backslash T)+v(S\cap(N\backslash T)}{vT(S)=v((S^T) v (N\ T))-v(N\ T)+v(S^(N\ T)}, for every coalition \eqn{S} of the set of players \eqn{N}.
#' @importFrom Rglpk Rglpk_solve_LP
#' @importFrom pracma ones
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' FacesGames(v)
#' @references González-Díaz, J., & Sánchez-Rodríguez, E. (2008). Cores of convex and strictly convex games. Games and Economic Behavior, 62(1), 100-105.
#' @references Mirás-Calvo, M. A., Quinteiro-Sandomingo, C., & Sánchez-Rodríguez, E. (2020). The boundary of the core of a balanced game: face games. International Journal of Game Theory, 49(2), 579-599.
#' @references Fiestras-Janeiro, M. G., Sánchez-Rodríguez, E., & Schuster, M. (2016). A precedence constraint value revisited. Top, 24(1), 156-179.
#' @seealso \link{ExactEnvelope}, \link{ExactFacesGames}, \link{FacesShapley}, \link{FacesSolution}
#' @export
FacesGames=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  caras_exactas=v[nC]*ones(nC-1,nC)
  v_intermedio =t(v*t(ones(nC-1,nC)));
  base = Basebin(n);
  indicador_orde=c();
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
  for(tt in 1:(nC-1)){
    f=base[nC-tt,];
    a=Rglpk_solve_LP(
      f, A, restricciones, b,
      bounds = list( lower = list( ind=seq(1L:(n)), val=lb ),
                     upper = list( ind=seq(1L:(n)), val=ub ) ),
      max=F,  )
    fT=v[nC]-a$optimum
    v_intermedio[tt,tt] = fT;
    v_intermedio[tt,nC-tt]=a$optimum;
    for(ii in 1:(nC-1)){
      caras_exactas[tt,]=ExactEnvelope(v_intermedio[tt,])
    }
  }
return(list(Faces=caras_exactas,intermediate=v_intermedio))}
