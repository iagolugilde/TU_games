#' @title Core cover vertices
#' @description This function returns the vertices of the core cover of the TU game.
#' @param v Characteristic function in the binary order.
#' @return The vertices of the core cover.
#' @details Let \eqn{(N,v)} a TU game and let \eqn{m(N,v)} the vector of minimal rights and \eqn{M(N,v)} the vector of the utopia payoffs, the core-cover is the set of all the imputations that assure each player his minimum
#' rights and no more than his utopia payment.
#' \deqn{CCover(N,v)=\{x\in R^n: m(N,v)\le x\le M(N,v)\}}{CC(N,v)={x: m(N,v)\le x\le M(N,v)}}
#' @importFrom rcdd makeH
#' @importFrom rcdd scdd
#' @importFrom grDevices chull
#' @examples
#' v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
#' CoreCoverVertices(v)
#' @references Tijs, S. H. (1981). Nash equilibria for noncooperative n-person games in normal form. Siam Review, 23(2), 225-237.
#' @seealso \link{CoreCoverSet}, \link{CoreVertices}, \link{EpsilonCoreVertices}, \link{ImputationVertices}, \link{LeastCoreVertices}, \link{UtopiaPayoffs}, \link{WeberVertices}
#' @export

CoreCoverVertices<-function(v){
  Characteristicdata(v)

  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  if(EssentialGame(v)$Essential == 1){
    M <- UtopiaPayoffs(v)$M
    m <- UtopiaPayoffs(v)$m

    matrixA1_help1 = diag(rep(-1,n))
    matrixA1_help2 = diag(rep(1,n))
    matrixA1 <- rbind(matrixA1_help1, matrixA1_help2)
    matrixA2 = matrix(rep(-1,n),nrow = 1)
    vectorB1 <- c(-m,M)
    vectorB2 = -v[length(v)]

    # Utilize the R-Package rcdd
    hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
    vRepresentation = scdd(hRepresentation)

    #Transform the V-Representation into a matrix
    VectorCounter = length(vRepresentation[[1]]) / (n + 2)
    OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
    ResultMatrix = matrix(OutcomeVector, VectorCounter, n)

    if(n == 3)
    {
      sequencevector =chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop=FALSE]
    }
    else
    {
      OutcomeMatrix = ResultMatrix
    }
  }
  else
  {
    OutcomeMatrix = matrix(ncol = n,nrow = 0)
  }
  return(OutcomeMatrix)
}
