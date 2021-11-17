#' @title Face-weighted Shapley value
#' @description This function calculates the face-weighted Shapley value.
#' @param v Characteristic function in binary order.
#' @param lambda Vector of weights in binary order.
#' @return It returns the face-weigthed Shapley values of a TU game given by
#' vector \eqn{v} and weights \eqn{\lambda}
#' @details Given an vector of weigths \eqn{\lambda\in R^{2^n-1}}{\lambda of dimension 2^n-1} such that \eqn{\sum_{i=1}^{2^n-1}\lambda_{i}=1}{\sum \lambda=1} and let \eqn{(N,v)} a cooperative game.
#' For each coalition \eqn{T} in \eqn{N}, exits a face game \eqn{v_T(S)=v((S\cap T) \cup (N\backslash T))-v(N\backslash T)+v(S\cap(N\backslash T)}{vT(S)=v((S^T) v (N\ T))-v(N\ T)+v(S^(N\ T)}. If we solve each face game with the Shapley value
#' and then we do the ponderate sum of the results with the vector of weights we obtain the face-weighted Shapley value.
#' We obtain the same results if we do the ponderate sum of the faces with the vector of weights and then we solve this game with the Shapley value.
#' @examples
#' v=c(0,0,0,0,0,0,1,0,0,1,3,4,6,8,10)
#' lambda=c(0.3,0.1,0,0.6,0,0,0,0,0,0,0,0,0,0,0)
#' FacesShapley(v,lambda)
#' @seealso \link{FacesGames}, \link{FacesSolution}, \link{Shapley}
#' @references Mirás Calvo, M. A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2021). The face-weighted Shapley values. Work in process.
#' @export

FacesShapley=function(v,lambda){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  if(sum(lambda)!=1)
    stop('The weigths do not sum 1',call.=F)

  if(length(lambda)!=m)
    stop('There must be a weight for each coalition',call.=F)

  Faces=rbind(FacesGames(v)$F,v)
  Sh=Shapley(t(t(Faces)%*%lambda))
  return(Sh)}
