#' @title Weber vertices
#' @description This function returns the vertices of the weber set of the TU game.
#' @param v Characteristic function in binary order.
#' @return The vertices of the weber set.
#' @details  iven a TU game \eqn{(N,v)} The Weber set, \eqn{W(N,v)}, is the convex hull of all the marginal vectors. Let \eqn{\pi} an order of the players of \eqn{N}, the distribution of the marginal contributions associated with the order \eqn{\pi} is defined as,
#' \eqn{m_i^{\pi}=v(Pre^{\pi}(i)\cap i)-v(Pre^{\pi}(i))}{the value of the characteristic function for the coalition of the players who precede the player i in the order \pi and the union of the player i minus the value of the characteristic function for the coalition of the players who precede the player i}
#' for all \eqn{i} in \eqn{N} \eqn{and}{} \eqn{Pre^{\pi}(i)=\{j:\pi(j)<\pi(i)\}}{}.
#'
#'The core  is always a subset of the Weber set and \eqn{C(N,v) = W(N,v)} if and only if the game \eqn{(N,v)} is convex.
#' @importFrom rcdd makeV
#' @importFrom rcdd scdd
#' @importFrom rcdd redundant
#' @importFrom grDevices chull
#' @examples
#' v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
#' WeberVertices(v)
#' @references Weber, R. J. (1988). Probabilistic values for games. The Shapley Value. Essays in Honor of Lloyd S. Shapley, 101-119.
#' @seealso \link{CoreCoverVertices}, \link{CoreVertices}, \link{EpsilonCoreVertices}, \link{ImputationVertices}, \link{LeastCoreVertices}, \link{Marginal}, \link{WeberSet}
#' @export
#'

WeberVertices<-function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  OutcomeMatrix = matrix(ncol = n,nrow = 0)
#  if(NonnegativeGame(v)==0){
#    message("Game is not nonnegative. Vertices of Weber Set are only computed for nonnegative games.")
#  }
 # else if (MonotonicGame(v)$mon==0){
#    print("Game is not monotonic. Vertices of Weber Set are only computed for monotonic games.")
#  }
#  else
#  {
    #getMarginalContributions gives the marginal vectors
    ResultList = Marginal(v)
    ResultMatrix = ResultList

    if(n == 3)
    {
      sequencevector = chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop = FALSE]
    }
    else
    {
      #Delete redundant points
      vRep = makeV(points = ResultMatrix)
      if (nrow(vRep) >= 2)
      {
        vRep = redundant(vRep, representation = "V")
      }

      VectorCounter = length(vRep[[1]]) / (n + 2)
      OutcomeVector = vRep[[1]][(VectorCounter * 2 + 1):(length(vRep[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, n)
      OutcomeMatrix = ResultMatrix
    }
 # }
  return (OutcomeMatrix)
}
