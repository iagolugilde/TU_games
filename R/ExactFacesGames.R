#' @title Exact faces games
#' @description Returns the exact faces games of a balanced TU game.
#' @param v Characteristic function in binary order.
#' @return Faces. Matrix of \eqn{(2^n-2)\times(2^n-1)}{(2^n-2)x(2^n-1)} length.
#' The i-row of Faces is the characteristic function of the exact face game
#' corresponding to the i-th coalition \eqn{T} (according to the binary ordering). If the corresponding face of the core is empty, then it will be a row of NaN.
#' @return intermediate. Matrix of \eqn{(2^n-2)\times(2^n-1)}{(2^n-2)x(2^n-1)} length.
#' @details Given a TU game \eqn{(N,v)} we define, for each coalition \eqn{T} in \eqn{N} its face game \eqn{(N,v_{F_T})}{(N,vFT)} as the exact envelope of the game \eqn{v_T}{vT}
#' define by:  \eqn{v_T(S)=v(S)}{vT(S)=v(S)} if \eqn{S} is not in \eqn{T}, and \eqn{v_T(T)=v(N)-v(N\backslash T)}{vT(T)=v(N)-v(N\ T)} otherwise.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ExactFacesGames(v)
#' @references González-Díaz, J., & Sánchez-Rodríguez, E. (2008). Cores of convex and strictly convex games. Games and Economic Behavior, 62(1), 100-105.
#' @references Mirás-Calvo, M. A., Quinteiro-Sandomingo, C., & Sánchez-Rodríguez, E. (2020). The boundary of the core of a balanced game: face games. International Journal of Game Theory, 49(2), 579-599.
#' @references Fiestras-Janeiro, M. G., Sánchez-Rodríguez, E., & Schuster, M. (2016). A precedence constraint value revisited. Top, 24(1), 156-179.
#' @seealso \link{ExactEnvelope}, \link{FacesGames}
#' @export
ExactFacesGames=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  vv_intermedio = matrix(0,nC-1,nC);
  caras_exactas = matrix(0,nC-1,nC);
  vv=ExactEnvelope(v)
  for(ii in 1:(nC-1)){
    vv_intermedio[ii,] = vv;
    vv_intermedio[ii,ii] = vv[nC]-vv[nC-ii];
    envelope = ExactEnvelope(vv_intermedio[ii,]);
    caras_exactas[ii,]=envelope;
  }
return(list(Faces=caras_exactas,intermediate=vv_intermedio))}
