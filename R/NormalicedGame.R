#' @title Normaliced Game
#' @description It provides the 0-normalization and the 0-1 normalization of a TU game.
#' @param v Characteristic function in binary order.
#' @return  norm0 is the 0-normalization of the TU game given by
#' vector v.
#' @return  norm01 is the 0-1 normalization of the TU game given by
#' vector v. If the 0-1 normalization does not exist, then it only returns norm0.
#' @details Given a TU game \eqn{(N,v)} its 0-normalization, \eqn{(N,v_0)}{(N,v0)}, is the game given by \eqn{v_0(S)=v(S)-d_i\sum v(i)}{v0(S)=v(S)-di\sum v(i)} for \eqn{S} in \eqn{N}. The 0-normalization
#' can be viewed as a change in the values of the individual coalitions
#' without changing the scale.
#'
#' The 0-1 normalization of  \eqn{(N,v)}, \eqn{(N,u)}, is the game given by
#' \eqn{u(S)=kv(S)+d_i\sum \alpha_i}{u(S)=kv(S)+di\sum \alpha i} where \eqn{k=1/(v(N)-\sum v(i))} and \eqn{\alpha_i=-kv(i)}{\alpha i=-kv(i)}, for all \eqn{i} in \eqn{N}. Therefore, the 0-1
#' normalization exists if and only if \eqn{v(N)-d_i\sum v(i)\not=0}{v(N)-di\sum v(i) is not equal to zero}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' NormalicedGame(v)
#' @seealso \link{ZeroMonotonicGame}
#' @export

NormalicedGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  v0=c()
  for (ii in 1:nC){
    P=Getplayers(ii)$P
    v0[ii]=v[ii]-sum(v[P])
  }
  N=Getplayers(nC)$P
  vN=sum(v[N])
  v1=c()
  if(vN==0){
    #print('The 0-1 normalization does not exist')

  return(list(norm0=v0))}
  k=1/vN
  for(ii in 1:nC){
    P=Getplayers(ii)$P
    v1[ii]=k*(v[ii]-sum(v[P]));
  }
  return(list(norm0=v0,norm01=v1))}
