#' @title Core vertices
#' @description This function returns the vertices of the core of the TU game.
#' @param v Characteristic function in binary order.
#' @return The vertices of the core.
#' @details The core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
#'  such that \eqn{x(S)\ge v(S)} for all coalition \eqn{S} in \eqn{N}, where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#'  \deqn{C(N,v)=\{x\in I(N,v):x(S)\ge v(S)\; \forall S\in N\}}{C(N,v)={x in I(N,v):x(S)\ge v(S) for all S in N}}
#'
#' The core allocations provide the agents with an incentive to maintain the grand coalition and it can be empty.
#' @importFrom rcdd makeH
#' @importFrom rcdd scdd
#' @importFrom rcdd redundant
#' @importFrom grDevices chull
#' @examples
#' v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
#' CoreVertices(v)
#' @references Edgeworth, F. Y. (1881). Mathematical psychics: An essay on the application of mathematics to the moral sciences (No. 10). CK Paul.
#' @references Gillies, D. B. (1953). Some theorems on n-person games. Princeton University.
#' @seealso \link{BalancedGame}, \link{CoreCoverVertices}, \link{CoreSet}, \link{EpsilonCoreVertices}, \link{ImputationVertices}, \link{LeastCoreVertices}, \link{WeberVertices}
#' @export

CoreVertices<-function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n

    v=Bin2lex(v)

    vectorA1 = c()

    #Calculate the matrixA1 for the function makeH
    vectorRechnung = c(rep(0,(n - 1)),c(1))
    vectorA1 = c(1, rep(0,(n - 1)))
    b = TRUE


    #Build the matrices and vectors for the function makeH
    matrixA1 = -Baselex(n=n)[-length(v),-(n+1)]
    matrixA2 = -matrix(rep(1,n),1,n)
    vectorB1 = -v[1:(length(v)-1)]
    vectorB2 = -v[length(v)]

    # Utilize the R-Package rcdd
    hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
    vRepresentation = scdd(hRepresentation)
    if (nrow(vRepresentation$output) >= 2)
    {
      vRepresentation = redundant(vRepresentation$output, representation = "V")
    }

    #Transform the V-Representation into a matrix
    VectorCounter = length(vRepresentation[[1]]) / (n + 2)
    OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
    ResultMatrix = matrix(OutcomeVector, VectorCounter, n)

    if(n == 3){
      sequencevector = chull(ResultMatrix)
      vertices = ResultMatrix[sequencevector,,drop = FALSE]
    }
    else{
      vertices = ResultMatrix
    }


  return(vertices)
}
