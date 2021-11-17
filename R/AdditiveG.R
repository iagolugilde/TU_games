#' @title Additive Game
#' @description Given a vector, A, it returns the characteristic function of the additive TU game of the vector A in the binary order.
#' @param A Vector with length n.
#' @return The characteristic function of the additive game given by the vector A.
#' @details Let A a n-dimensional vector, the characteristic function of the additive TU game is, for each coalition \eqn{S} in \eqn{N}, \eqn{v(S)=\sum_{i\in S}A(i)}{the sum of the elements of the vector that are in S}.
#' @examples
#' A=c(0,1,1,2)
#' AdditiveG(A)
#' @seealso \link{SuperAdditiveGame}
#' @export
AdditiveG=function(A){
  v=c();

  n=length(A)
  for (S in 1:2^n-1){
  Sp=as.logical((as.numeric(intToBits(S)[1:n])));
  v[S]=sum(A[Sp]);
}
return(v)}
