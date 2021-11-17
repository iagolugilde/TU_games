#' @title Nucleolus auxiliar
#' @description  Linear programming problems used in nucleolus computation.
#' @param v Characteristic function in binary order.
#' @param C,b The matrix \eqn{C} and vector \eqn{b} are, respectively, the coefficients of the linear inequality constraints and the corresponding right-hand side
#' vector: \eqn{C\cdot x\le b}{C*x\le b}.
#' @param Ceq,beq The matrix \eqn{Ceq} and vector \eqn{beq} are, respectively, the coefficients of the linear equality constraints and the corresponding right-hand side
#' vector: \eqn{Ceq\cdot x=beq}{Ceq*x=beq}.
#' @param  saturated The reference of the binding inequality constraints.
#' @param el The value of the corresponding optimum vector of excesses component.
#' @return CC, bb. The matrix \eqn{CC} and vector \eqn{bb} are, respectively, the coefficients of the new linear inequality constraints and the corresponding right-hand
#' side vector: \eqn{C\cdot x\le b}{C*x\le b}.
#' @return  CCeq, bbeq. The matrix \eqn{Ceq} and vector \eqn{bbeq} are, respectively, the coefficients of the new linear equality constraints and the corresponding
#' right-hand side vector: \eqn{CCeq\cdot x= bbeq}{CCeq*x= bbeq}.
#' @return indep. The number of independent equality constraints in the new linear programming problem.
#' @importFrom pracma rref
#' @references Schmeidler, D. (1969). The nucleolus of a characteristic function game. SIAM Journal on applied mathematics, 17(6), 1163-1170.
#' @seealso \link{Nucleolus}, \link{Nucleolussimplex}
#' @export

Nucleolusaux=function(v,C,b,Ceq,beq,saturated,el){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  CC=C;bb=b;
  CC=CC[-saturated,];
  bb=bb[-saturated,];
  if(length(saturated)==1){
    CCeq=rbind(Ceq,c(C[saturated,1:n],0))
  }else{
  CCeq=rbind(Ceq,cbind(C[saturated,1:n],0))}
  AUX=b[saturated]+el;
  bbeq=c(beq,AUX)
  CCEQ=cbind(CCeq,bbeq)

  toler=10^(-6);
  ecuaciones=dim(CCEQ)[1];
  indep=qr(CCEQ,toler)$rank

  if (ecuaciones-indep>0){
  base=rref(t(CCEQ));
  filasindep=c(1:qr(base,toler)$rank)
CCeqindep=CCeq[filasindep,];
bbeqindep=bbeq[filasindep];
CCeq=CCeqindep;bbeq=bbeqindep;
}


  return(list(CC=CC,bb=bb,CCeq=CCeq,bbeq=bbeq,indep=indep))}
