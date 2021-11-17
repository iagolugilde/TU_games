#' @title Deviation index
#' @description This function returns the general deviation index and the general signed deviation index of a rule for a claims problem.
#' @param v Characteristic function in binary order.
#' @param R A solution: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau.
#' @param S A solution: Alexia, Corecenter, DuttaRay, Nucleolus, PerCapitaNucleolus, PreNucleolus, Shapley, Solidarity and Tau.
#' @return The  deviation index and the signed deviation index of a solution for a TU game.
#' @details Given a TU game \eqn{(N,v)}, the cumulative awards curve of a solution \eqn{S} with respect of a solution \eqn{R} for the game is the polygonal path connecting the \eqn{n+1} points
#' \deqn{(0,0), (\frac{S_1(N,v)}{v(N)},\frac{R_1(N,v)}{v(N)}),\dots,(\frac{\sum_{i=1}^{n-1}S_i(N,v)}{v(N)},\frac{\sum_{i=1}^{n-1}R_i(N,v)}{v(N)}),(1,1).}{%
#' (0,0) , (S1(N,v)/v(N),R1(N,v)/v(N)) , ((S1(N,v)+S2(N,v))/v(N) , ((R1(N,v)+R2(N,v))/v(N) ,\dots , (1,1).}
#' The signed deviation index of the solution \eqn{S} with respect to the rule \eqn{R} for the TU game \eqn{(N,v)}, denoted by \eqn{I(S(N,v),R(N,v))}, is
#' the ratio of the area that lies between the identity line and the cumulative curve over the total area under the identity line.
#'
#' Let \eqn{R_0(N,v)=0}{R0(N,v)=0} and \eqn{S_0(N,v)=0}{S0(N,v)=0}. For each \eqn{k=1,\dots,n} define,
#' \eqn{X_k=\frac{1}{v(N)} \sum_{j=0}^{k} R_j}{Xk=(R0+\dots+Rk)/v(N)} and
#' \eqn{Y_k=\frac{1}{v(N)} \sum_{j=0}^{k} S_j}{Xk=(S0+\dots+Sk)/v(N)}. Then,
#' \deqn{I(S(N,v),R(N,v))=1-\sum_{k=1}^{n}(X_{k}-X_{k-1})(Y_{k}+Y_{k-1}).}{I(R(N,v),S(N,v))=1-\sum (Xk-X(k-1))(Yk+Y(k-1)) where the sum goes from k=1 to n.}
#'  In general  \eqn{-1 \le I(S(N,v),R(N,v)) \le 1}.
#'
#' The deviation index of the solution \eqn{R} with respect to the solution \eqn{S} for the game \eqn{(N,v)}, denoted by \eqn{I^{+}(R(N,v),S(N,v))}{I+(R(N,v),S(N,v))}, is
#' the ratio of the area between the line of the cumulative sum of solution \eqn{R} and the cumulative curve over the area under the line \eqn{x=y}.
#' In general,  \eqn{0 \le I^{+}(S(N,v),R(N,v)) \le 1}{0 \le I+(S(N,v),R(N,v)) \le 1}.
#'
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' S=Corecenter
#' R=Shapley
#' DeviationIndex(v,S,R)
#' @references  Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality, 10(3), 421-443.
#' @references Mirás Calvo, M. A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2020). Deviation from proportionality and Lorenz-domination for claims problems. ECOBAS.
#' @export
DeviationIndex = function(v,S,R) {
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  m=Characteristicdata(v)$nC
  if (v[m] == 0) {
    #claims index for the rule is zero
    stop('The total utility must be positive, v(N)>0.',call.=F)
  }else {
    x=R(v)
    do = sort(x)
    if (sum(do == x) < n){
      message('The result is computed for the rearranged players.\n')
    }
    x=do
    #claims index for the rule and for the claims
    y = sort(S(v))
    crule1=c(0,cumsum(x)/v[m])
    crule2=c(0,cumsum(y)/v[m])
    if(sum(crule1>=crule2)==(n+1)){
      index = (1-1/v[m]*(sum(x*y)/v[m]+2/v[m]* sum(y*(v[m]-cumsum(x)))))
    }else if(sum(crule1<=crule2)==(n+1)){
      index = -(1-1/v[m]*(sum(x*y)/v[m]+2/v[m]* sum(y*(v[m]-cumsum(x)))))
    }else{
      alpha=rep(0,n+1)
      alpha[crule1>crule2]=-1
      alpha[crule1<crule2]=1
      index=0
      for(i in 2:(n+1)){
        if(alpha[i-1]>=0 & alpha[i]>=0){
          index=index+(crule2[i]-crule2[i-1])*(crule2[i-1]-crule1[i-1]+crule2[i]-crule1[i])
        }else if(alpha[i-1]<=0 & alpha[i]<=0){
          index=index+(crule2[i]-crule2[i-1])*(crule1[i-1]-crule2[i-1]+crule1[i]-crule2[i])
        }else if(alpha[i-1]==1 & alpha[i]==-1){
          z=(crule2[i]*crule1[i-1]-crule2[i-1]*crule1[i])/((crule2[i]-crule2[i-1])-(crule1[i]-crule1[i-1]))
          index=index+(z-crule2[i-1])*(crule2[i-1]-crule1[i-1])+(crule2[i]-z)*(crule1[i]-crule2[i])
        }else if(alpha[i-1]==-1 & alpha[i]==1){
          z=(crule2[i]*crule1[i-1]-crule2[i-1]*crule1[i])/((crule2[i]-crule2[i-1])-(crule1[i]-crule1[i-1]))
          index=index+(z-crule2[i-1])*(crule1[i-1]-crule2[i-1])+(crule2[i]-z)*(crule2[i]-crule1[i])
        }
      }
    }
    index_signed = 1-(sum(x*y)+2*sum(y*(v[m]-cumsum(x))))/v[m]^2
    return(list(index=index,index_signed=index_signed))
  }

}
