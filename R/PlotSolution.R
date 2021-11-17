#' @title Plot of an awards vector
#' @description This function plots an awards vector of a TU game.
#' @param v Characteristic function in binary order.
#' @param awards An awards vector.
#' @param set A logical value. By default, set=TRUE.
#' @param solution A solution: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau.
#' @param col The color.
#' @return If set = TRUE, the function creates a new figure plotting both the core for the game and the given awards vector.
#' Otherwise, it just adds to the current picture the point representing the solution. The function only plots one awards vector at a time.
#'
#' The awards vector can be introduced directly as a vector. Alternatively, we can provide a rule and then the awards vector to be plotted is the one selected by the rule for the claims problem.
#' Therefore, if Rule = NULL it plots the given awards vector.
#' Otherwise, it plots the awards vector selected by the given rule for the claims problem.
#' In order to plot two (or more) awards vectors, draw the first one with the option set = TRUE and add the others, one by one, with the option set = FALSE.
#' @details Given a TU game \eqn{(N,v)}, a solution is a function that assigns a division of the total uttility, \eqn{v(N)}, between all the players of the game.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' WeberSet(v)
#' PlotSolution(v,solution=Corecenter,set=FALSE)
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom rgl text3d
#' @importFrom rgl points3d
#' @seealso \link{Alexia}, \link{Corecenter}, \link{CoreCoverSet}, \link{CoreSet}, \link{DuttaRay}, \link{EpsilonCoreSet}, \link{HarsanyiSet}, \link{ImputationSet}, \link{LeastCoreSet}, \link{Nucleolus}, \link{PerCapitaNucleolus}, \link{PreNucleolus}, \link{Shapley}, \link{Solidarity}, \link{Tau}, \link{WeberSet}
#' @export
PlotSolution = function(v, solution=NULL, awards=NULL, set=TRUE, col = "blue") {
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n

  # MORE than 4 players
  if (n>4)
    stop('There are more than 4 players. \n',call.=F)

  #####
  ##### If given a rule, then awards = Rule(E,d)
  if (is.null(solution)==FALSE){
    awards=solution(v)
  }
  if(length(awards)!=n)
    stop('The awards vector and the game do not have the same dimension',call.=F)
  #
  #
  # CASE n=2
  if (n == 2) {

    if (set==TRUE){CoreSet(v)}
    points(awards[1], awards[2], col = col, pch = 8)
  }
  #
  #
  # CASE n=3
  else if (n == 3) {
    # We need to compute the matrix to project the awards vector in our equilateral triangle
    m =UtopiaPayoffs(v)$m
    Delta=v[7]-v[1]-v[2]-v[4];
    # the real imputation set (in R^3)
    imputation=matrix(c(v[7]-v[2]-v[4],v[2],v[4],v[1],v[7]-v[1]-v[4],v[4],v[1],v[2],v[7]-v[1]-v[2],v[7]-v[2]-v[4],v[2],v[4]),ncol=3,byrow=T)
    imputation=unique(imputation)
    # The equilateral triangle (in R^2)
    equilatero=matrix(c(0,0,Delta,0,Delta/2,sqrt(3)/2*Delta),ncol=2,byrow=T);

    # The projection matrix
    P=t(equilatero)%*%solve(t(imputation));
    # The projected coordinates of the vector of awards
    awards = t(P %*% t(t(awards)))
    # Plot the set of awards (if necessary)
    if (set==TRUE){CoreSet(v)}
    points(awards, col = col, pch = 8)
  }
  #
  #
  # CASE n=4
  else if (n == 4) {
    if (set==TRUE){CoreSet(v)}
    text3d(awards[1],awards[2],awards[3],"*",col=col,font=4)
  }
}
