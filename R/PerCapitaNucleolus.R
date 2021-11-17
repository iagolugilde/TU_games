#' @title Per Capita Nucleolus
#' @description It provides the per capita nucleolus of a TU game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return  It returns the per capita nucleolus of a TU game given by vector \eqn{v}.
#' @details  Let \eqn{(N,v)} be a TU-game such that \eqn{I(v)} is not empty. Given an allocation \eqn{x} the complaint, or excess per capita, of coalition \eqn{S}
#' is defined by \eqn{e(x,S)=\frac{v(S)-x(S)}{|S|}}{e(x,S)=(v(S)-x(S))/|S|} where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#' Next, \eqn{e(x)} denote the vector of excesses per capita for all the coalitions with its elements arranged in decreasing order. The
#' per capita nucleolus is then the imputation \eqn{\eta} in \eqn{I(v)} such that
#' \eqn{e(\eta) \le e(x)}, for all \eqn{x} in \eqn{I(v)}.
#' The per capita nucleolus is the allocation of the imputation set that minimizes the
#' maximum per capita dissatisfaction.
#' @examples
#' v=c(0,0,1,0,1,1,2)
#' PerCapitaNucleolus(v)
#' @references Schmeidler, D. (1969). The nucleolus of a characteristic function game. SIAM Journal on applied mathematics, 17(6), 1163-1170.
#' @references Walmeier, E. (1983): “Der f-Nucleolus und ein dynamisches Verhandlungsmodell als Losungskonzepte fur Kooperative n-Personenspiele, Skripten zur Mathematischen Statistik 5,” Ph.D. thesis, Westfalische WilhelmsUniversitat, Munste
#' @seealso \link{Excesses}, \link{Nucleolus}, \link{PlotSolution}, \link{PreNucleolus}
#' @export
PerCapitaNucleolus=function(v,name=FALSE){
  if(name==TRUE){
    solution="Per Capita Nucleolus"
    return(solution)
  }
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  v=Bin2lex(v)
  control=0
  etapa=1
  base=Baselex(n);suma=rowSums(base)
  C=cbind(-Baselex(n)[1:(m-1),],rep(-1,m-1))*suma[1:(m-1)]
  b=as.matrix(-v[1:(m-1)])
  Ceq=c(rep(1,n),0)
  beq=v[m]
  y=Nucleolussimplex(v,C,b,Ceq,beq)
  reparto=y[[1]]
  el=y[[2]]
  saturadas=y[[3]]

  while (control==0 & etapa<(m-1)){
    etapa=etapa+1;
    a=Nucleolusaux(v,C,b,Ceq,beq,saturadas,el);
    CC=a[[1]]
    bb=t(t(a[[2]]))
    CCeq=a[[3]]
    bbeq=a[[4]]
    indep=a[[5]]
    if (indep>=n){
      control=1;nucleolus=reparto;
    }else{
      y=Nucleolussimplex(v,CC,bb,CCeq,bbeq)
      reparto=y[[1]]
      el=y[[2]]
      saturadas=y[[3]]
      C=CC;b=bb;Ceq=CCeq;beq=bbeq;}
  }
  if(etapa==(m-1)){
    nucleolus=reparto
  }


  return(nucleolus)}
