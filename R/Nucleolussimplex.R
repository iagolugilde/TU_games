#' @title Nucleolus simplex
#' @description  Linear programming algorithm for computing the nucleolus.
#' @param v The objective function.
#' @param C,b The matrix \eqn{C} and vector \eqn{b} are, respectively, the coefficients of the linear inequality constraints and the corresponding right-hand side
#' vector: \eqn{C\cdot x\le b}{C*x\le b}.
#' @param Ceq,beq The matrix \eqn{Ceq} and vector \eqn{beq} are, respectively, the coefficients of the linear equality constraints and the corresponding right-hand side
#' vector: \eqn{Ceq\cdot x=beq}{Ceq*x=beq}.
#' @return allocation. All the components of the optimal solution, except the last one, which gives the allocation.
#' @return  el. The last component of the optimal solution, which gives the excess.
#' @return saturated. The binding inequality constraints at the optimum.
#' @importFrom Rglpk Rglpk_solve_LP
#' @references Schmeidler, D. (1969). The nucleolus of a characteristic function game. SIAM Journal on applied mathematics, 17(6), 1163-1170.
#' @seealso \link{Nucleolus}, \link{Nucleolusaux}
#' @export

Nucleolussimplex=function(v,C,b,Ceq,beq){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  cc=c(rep(0,n),1)
lb=c(v[1:n],-Inf)
ub=rep(Inf,n+1)
#Ceq=as.matrix(Ceq)
restricciones=c(rep("<=",dim(b)[1]),rep("==",length(beq)))
l=dim(C)[1]
C=rbind(C,Ceq)
beq=as.matrix(beq)
b=rbind(b,beq)
a=Rglpk_solve_LP(
  cc, C, restricciones, b,
  bounds = list( lower = list( ind=seq(1L:(n+1)), val=lb ),
                 upper = list( ind=seq(1L:(n+1)), val=ub ) ),
  max=F,  )

solucion=a$solution
multiplicadores=-a$auxiliary$dual[1:l]
saturadas=which(abs(multiplicadores)>10^(-4));
reparto=solucion[1:n]
el=solucion[n+1]



  return(list(allocation=reparto,el=el,saturated=saturadas))}
