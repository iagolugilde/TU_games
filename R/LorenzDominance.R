#' @title Lorenz-dominance relation
#' @description This function checks whether or not the awards assigned by two solutions to a TU game are Lorenz-comparable.
#' @param v Characteristic function in binary order.
#' @param Solutions Two solutions: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau. By default, Solution=Corecenter.
#' @param Info A logical value.
#' @return If Info = FALSE, the Lorenz-dominance relation between the awards vectors selected by both solutions.
#' If both awards vectors are equal then cod = 2. If the awards vectors are not Lorenz-comparable then cod = 0.
#' If the awards vector  selected by the first solution Lorenz-dominates the awards vector  selected by the second rule then cod = 1; otherwise cod = -1.
#' If Info = TRUE, it also gives the corresponding cumulative sums.
#'
#' @details Given a TU game \eqn{(N,v)}, a vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the TU game \eqn{(N,v)}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=v(N)}{x1+\dots+xn=v(N)} the sum of its coordinates is equal to \eqn{v(N)}.
#'
#'  In order to compare a pair of awards vectors \eqn{x,y\in C(N,v)}{x,y in C(N,v)} with the Lorenz criterion,
#' first one has to rearrange the coordinates of each allocation in a non-decreasing order. Then we say that \eqn{x} Lorenz-dominates \eqn{y} (or, that \eqn{y} is Lorenz-dominated by \eqn{x})
#' if all the cumulative sums of the rearranged coordinates are greater with \eqn{x} than with \eqn{y}. That is,
#' \eqn{x} Lorenz-dominates \eqn{y} if for each \eqn{k=1,\dots,n-1} we have that
#' \deqn{\sum_{j=1}^{k}x_j \geq \sum_{j=1}^{k}y_j}{x1+\dots+xk \ge y1+\dots+yk.}
#'
#' Let \eqn{S} and \eqn{S'} be two solutions. We say that \eqn{S} Lorenz-dominates \eqn{S'} if \eqn{S(N,v)} Lorenz-dominates \eqn{S'(N,v)} for all \eqn{(N,v)}.
#'
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Solutions=c(Shapley,Corecenter)
#' LorenzDominance(v,Solutions)
#' @references  Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association, 9(70), 209-219.
#' @seealso \link{Alexia}, \link{Corecenter}, \link{CumulativeAwardsCurve} \link{DeviationIndex}, \link{DuttaRay}, \link{Nucleolus}, \link{PerCapitaNucleolus}, \link{PreNucleolus}, \link{Shapley}, \link{Solidarity}, \link{Tau}
#' @export

LorenzDominance = function(v,Solutions, Info = FALSE) {
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n

  if(length(Solutions)>2)
    warning('We check if the first two solutions are Lorenz-comparable. \n',call.=F,immediate.=T)

  #####################

  Solution1char = Solutions[[1]](0, name=TRUE)
  Solution2char = Solutions[[2]](0, name=TRUE)
  solution1 = Solutions[[1]](v)
  solution2 = Solutions[[2]](v)
  do = sort(solution1)
  if (sum(do == solution1) < n){
    message('The result is computed for the rearranged players.\n')
  }
  solution1=sort(solution1)
  solution2=sort(solution2)
  sum1 = cumsum(solution1)
  sum2 = cumsum(solution2)
  dom1 = sum(round(sum1,3) >= round(sum2,3))
  dom2 = sum(round(sum2,3) >= round(sum1,3))

  if (dom1 == n & dom2 < n) {
    cod=1
    if (Info == TRUE){
      message('The awards vector selected by the ',
              Solution1char,
              ' value Lorenz dominates the awards vector selected by the ',
              Solution2char,
              ' value.\n')
    }
  } else if (dom2 == n & dom1 < n) {
    cod=-1
    if (Info == TRUE){
      message('The awards vector selected by the ',
              Solution1char,
              ' value is Lorenz-dominated by the awards vector selected by the ',
              Solution2char,
              ' value. \n')

    }
  } else if (dom1 == n & dom2 == n) {
    cod=2
    if (Info == TRUE){
      message('The awards vectors selected by the ',
              Solution1char,
              ' and the ',
              Solution2char,
              ' values are equal.\n')

    }
  }else{
    cod=0
    if (Info == TRUE){
      message('The awards vectors selected by the',
              Solution1char,
              'and the',
              Solution2char,
              'values are not Lorenz-comparable.\n')

    }
  }
  if (Info == TRUE){
    message('The cumulative sums for both awards vectors are:\n')
    message(paste(
      format(Solution1char, width = 17, justify = c("right")),
      format(Solution2char, width = 17, justify = c("right")),
      '\n'
    ))
    for (i in 1:n) {
      message(paste(
        format(
          round(sum1[i], 3),
          nsmall = 3,
          width = 17,
          justify = c("right")
        ),
        format(
          round(sum2[i], 3),
          nsmall = 3,
          width = 17,
          justify = c("right")
        ),
        '\n'
      ))
    }
  }
  return(list(cod=cod))
}
