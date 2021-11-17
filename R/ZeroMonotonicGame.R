#' @title Zero Monotonic Game
#' @description Checks if a TU game is 0-monotonic
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector \eqn{v} is 0-monotonic it returns 1, otherwise it returns 0.
#' @details Given a TU game \eqn{(N,v)} it is 0-monotonic if its 0-normalization is monotonic.
#'
#' A game is monotonic if \eqn{v(S) \le v(T)} for all coalitions \eqn{S, T} such that \eqn{S} is in \eqn{T}.
#'A game is 0-monotonic if, and only if the Weber set is inside the imputation set.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ZeroMonotonicGame(v)
#' @seealso \link{ImputationSet}, \link{MonotonicGame}, \link{NormalicedGame}, \link{WeberSet}
#' @export
ZeroMonotonicGame=function(v){
  Characteristicdata(v)
    NO=NormalicedGame(v)[[1]]
    M=MonotonicGame(NO)


  return(M)}
