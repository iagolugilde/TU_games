#' @title Face-weighted value.
#' @description This function calculates the face-weighted value for any solution.
#' @param v Characteristic function in binary order.
#' @param lambda vector of weights in binary order.
#' @param solution A solution: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau.
#' @return It returns the possible ways to compute the face-weigthed values of a TU game for any solution given by
#' vector \eqn{v} and weights \eqn{\lambda}.
#' @details Given an vector of weigths \eqn{\lambda\in R^{2^n-1}}{\lambda of dimension 2^n-1} such that \eqn{\sum_{i=1}^{2^n-1}\lambda_{i}=1}{\sum \lambda=1} and let \eqn{(N,v)} a cooperative game.
#' For each coalition \eqn{T} in \eqn{N} exits a face game \eqn{v_T(S)=v((S\cap T) \cup (N\backslash T))-v(N\backslash T)+v(S\cap(N\backslash T)}{vT(S)=v((S^T) v (N\ T))-v(N\ T)+v(S^(N\ T)}. If we solve each face game with any solution
#' and then we do the ponderate sum of the results with the vector of weights we obtain the face-weighted solution value.
#' Another option is do the ponderate sum of the faces with the vector of weights and then we solve this game with any solution. The results are the same only
#' for the Shapley value. This function returns both results.
#' @examples
#' v=c(0,0,0,0,0,0,1,0,0,1,3,4,6,8,10)
#' lambda=c(0.3,0.1,0,0.6,0,0,0,0,0,0,0,0,0,0,0)
#' FacesSolution(v,lambda,Nucleolus)
#' @seealso \link{Alexia}, \link{Corecenter}, \link{DuttaRay}, \link{FacesGames}, \link{FacesShapley}, \link{Nucleolus}, \link{PerCapitaNucleolus}, \link{PreNucleolus}, \link{Shapley}, \link{Solidarity}, \link{Tau}
#' @references Mirás Calvo, M. A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2021). The face-weighted Shapley values. Work in process.
#' @export

FacesSolution=function(v,lambda,solution){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  if(sum(lambda)!=1)
    stop('The weigths do not sum 1',call.=F)

  if(length(lambda)!=m)
    stop('There must be a weight for each coalition',call.=F)

  Faces=rbind(FacesGames(v)$F,v)
  awards1=solution(t(t(Faces)%*%lambda))
  Sol_faces=matrix(0,m,n)
  for(i in 1:m){
    Sol_faces[i,]=lambda[i]*solution(Faces[i,])
  }
  awards2=colSums(Sol_faces)
  return(list(faces_weight=awards1,awards_weight=awards2))}
