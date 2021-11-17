#' @title Cumulative awards curve
#' @description This function returns the cumulative awards curve for a TU game.
#' @param v Characteristic function in binary order.
#' @param Solutions The solutions: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau.
#' @param Solution Principal solution: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau. By default, Solution=Corecenter.
#' @param col The colors. If col = NULL then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The color legend is shown if legend = TRUE.
#' @return The graphical representation of the cumulative awards curves of a puntual solution (or several solutions) for a TU game.
#' @details  Given a TU game \eqn{(N,v)}, the cumulative awards curve of a solution \eqn{S'} with respect of a solution \eqn{S} for the game is the polygonal path connecting the \eqn{n+1} points
#' \deqn{(0,0), (\frac{S'_1(N,v)}{v(N)},\frac{S_1(N,v)}{v(N)}),\dots,(\frac{\sum_{i=1}^{n-1}S'_i(N,v)}{v(N)},\frac{\sum_{i=1}^{n-1}S_i(N,v)}{v(N)}),(1,1).}{%
#' (0,0) , (S'1(N,v)/v(N),S1(N,v)/v(N)) , ((S'1(N,v)+S'2(N,v))/v(N) , ((S1(N,v)+S2(N,v))/v(N) ,\dots , (1,1).}
#'
#' The cumulative claims-awards curve fully captures the Lorenz ranking of solutions:
#' if a solution \eqn{S} Lorenz-dominates a rule \eqn{S'} then, for each TU game, the cumulative awards  curve of \eqn{S}
#' lies above the cumulative curve of \eqn{S'}.
#' @examples
#' v=c(0,0,0,1,1,1,20)
#' Solution=Corecenter
#' Solutions=c(Shapley,Nucleolus)
#' CumulativeAwardsCurve(v,Solutions,Solution)
#' @references  Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association, 9(70), 209-219.
#' @references Mirás Calvo, M. A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2020). Deviation from proportionality and Lorenz-domination for claims problems. ECOBAS.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics grid
#' @importFrom graphics mtext
#' @seealso \link{Alexia}, \link{Corecenter}, \link{DeviationIndex}, \link{DuttaRay}, \link{LorenzDominance}, \link{Nucleolus}, \link{PerCapitaNucleolus}, \link{PreNucleolus}, \link{Shapley}, \link{Solidarity}, \link{Tau}
#' @export

CumulativeAwardsCurve = function(v,Solutions, Solution=Corecenter, col = NULL, legend = TRUE) {
  Characteristicdata(v)
  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }

  n = Characteristicdata(v)$n
  m = Characteristicdata(v)$nC


  if (v[m] ==0)
    stop('The total utility must be positive, v(N)>0.',call.=F)
  R=Solution(0,name=TRUE)

  ###################

  ### THE IDENTITY LINE
  plot(
    c(0, 1), c(0, 1),
    type = "l",
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = c("Percentage of awards Solution 1"),
    ylab = c("Percentage of awards rest of solutions"),
    main = paste(R," curve",sep="")
  )
  subtitle=paste("v=(",toString(v),")",sep="")
  mtext(subtitle,side=3,line=0.5,cex=0.7)
  grid()
  ###### claims CURVE (DATA) ######
  numberrules = length(Solutions)
  percentAwards = matrix(0, numberrules, n)
  percentAwards2= matrix(0, numberrules, n)
  # THE CURVES
  r=Solution(v)
  do = sort(r)
  if (sum(do == r) < n){
    message('The result is computed for the rearranged players.\n')
  }
  r=do
  for (ii in 1:numberrules) {
    # THE CUMULATIVE PERCENTAGES
    rule = sort(Solutions[[ii]](v))
    percentAwards[ii, ] = cumsum(rule)/v[m]
    #r=Solution(v)
    percentAwards2[ii, ] = cumsum(r)/v[m]
    # THE POLIGONAL CURVE
    lines(
      c(0,percentAwards2[ii,]),
      c(0, percentAwards[ii,]),
      lwd = 2,
      type = "o",
      col = col[ii]
    )
  }
  name = rep(0, numberrules)
  for (kk in 1:numberrules) {
    name[kk] = Solutions[[kk]](0,TRUE )
  }
  if (legend==TRUE) {
    legend(
      x = "topleft",
      legend = c(name),
      col=col[1:numberrules],
      lty=1,
      lwd=2,
      seg.len=0.5,
      cex=0.8,
      y.intersp=0.6,
      bty = "n"
    )
  }
}
