#' @title Imputation set
#' @description Draws the imputation set of an essential non-degenerate TU game for 2, 3 or 4 players.
#' @param v Characteristic function in binary order:
#'
#' For 2 players the characteristic function must be introduced with vector \eqn{v=(v_{1},v_{2},v_{12})}{v=(v{1},v{2},v{12})}
#'
#' For 3 players the characteristic function must be introduced with vector
#'  \eqn{v=(v_{1},v_{2},v_{12},v_{3},v_{13},v_{23},v_{123})}{v=(v{1},v{2},v{12},v{3},v{13},v{23},v{123})}.
#'
#'  For 4 players the characteristic function must be introduced with vector
#'
#' \eqn{v=(v_{1},v_{2},v_{12},v_{3},v_{13},v_{23},v_{123},v_{4},v_{14},v_{24},v_{124},v_{34},v_{134},v_{234},v_{1234})}{v=(v{1},v{2},v{12},v{3},v{13},v{23},v{123},v{4},v{14},v{24},v{124},v{34},v{134},v{234},v{1234})}.
#' @param col Color.
#' @return The graph of the imputation set.
#' @details Given a TU game \eqn{(N,v)}, the imputation set is defined by \eqn{I(N,v)=\{x=(x_1, \ldots , x_n) : x_i \ge v(i)\; \forall i \in N, \sum_{i\in N} x_i=v(N)\}}{I(N,v)={x=(x1, \ldots , xn) : xi \ge v(i) for all i in N, \sum xi=v(N), i in N}}.
#' It is the set of all individually rational and efficient allocations.
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics text
#' @importFrom graphics polygon
#' @importFrom rgl plot3d
#' @importFrom rgl view3d
#' @importFrom rgl par3d
#' @importFrom pracma ones
#' @importFrom graphics grid
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ImputationSet(v,"gray")
#' @seealso \link{CoreCoverSet}, \link{CoreSet}, \link{EpsilonCoreSet}, \link{HarsanyiSet}, \link{ImputationVertices}, \link{LeastCoreSet}, \link{WeberSet}
#' @export

ImputationSet=function(v,col=NULL){
   Characteristicdata(v)
   n=Characteristicdata(v)$n
   if(n>4)
      stop('The imputation set can only be represented if n<=4',call.=F)
   if(n==2){
      if(v[3]==(v[1]+v[2])){
         vi=c(v[1],v[2])
         return(paste('The core is a single-point: (',toString(vi),')',sep=""))
      }
      if(v[3]<(v[1]+v[2])){
         return(paste('The core is empty',sep=""))
      }
      m=UtopiaPayoffs(v)$m
      # The length of the set of awards
      Delta=v[3]-sum(m)
      V=matrix(c(m[1],v[3]-m[1],v[3]-m[2],m[2]),ncol=2,byrow=T)
      #The extreme points of the set of awards (no repeated points)
      V=unique(V)
      #If the set of awards is a line segment
      #The axis are determined by the minimal rights
      plot(m[1],
                         m[2],
                         type="n",
                         xlim=c(m[1],v[3]-m[2]),
                         ylim=c(m[2],v[3]-m[1]),
                         xlab=expression("x"[1]),
                         ylab=expression("x"[2]),
                         main=c("Core set")
      )
         subtitle=paste("v=(",toString(v),")",sep="")
         mtext(subtitle,side=3,line=0.5,cex=0.7)
         grid()
      lines(c(m[1],v[3]-m[2]),c(m[2],m[2]))
      lines(c(m[1],m[1]),c(v[3]-m[1],m[2]))
      # Default color for the set fo awards vector: RED
      if (is.null(col)){col="red"}
      lines(t(V[,1]),t(V[,2]),col=col)
   }else if(n==3){
      imputation=matrix(c(v[7]-v[2]-v[4],v[2],v[4],v[1],v[7]-v[1]-v[4],v[4],v[1],v[2],v[7]-v[1]-v[2],v[7]-v[2]-v[4],v[2],v[4]),ncol=3,byrow=T)
      imputation=unique(imputation)
      Delta=v[7]-v[1]-v[2]-v[4];
      #The corresponding extreme points of the equilateral triangle in our window
      equilatero=matrix(c(0,0,Delta,0,Delta/2,sqrt(3)/2*Delta),ncol=2,byrow=T);
      P=t(equilatero)%*%solve(t(imputation));
      plot(0,
           0,
           xlim=c(min((equilatero[,1]))-0.3,max((equilatero[,1]))+0.3),
           ylim=c(min((equilatero[,2])),max((equilatero[,2]))*1.1),
           type="n",
           main="Imputation set",
           axes=F,
           xlab=paste("v=(",toString(v),")",sep=""),
           ylab="",
           asp=1)
      text(equilatero[1,1],equilatero[1,2]+0.2, paste("(",toString(imputation[1,]),")"))
      text(equilatero[2,1],equilatero[2,2]+0.2, paste("(",toString(imputation[2,]),")"))
      text(equilatero[3,1],equilatero[3,2]-0.2,paste("(",toString(imputation[3,]),")"))
      if (is.null(col)){col="white"}
   polygon(t((equilatero[,1])),t((equilatero[,2])),col=col)

 }else if(n==4){
   #v=c(v[1],v[2],v[4],v[8],v[3],v[5],v[9],v[6],v[10],v[12],v[7],v[11],v[13],v[14],v[15])
    vertices2=matrix(c(v[1], v[2], v[4],v[1], v[2], v[15]-v[1]-v[2]-v[8],
                       v[15]-v[2]-v[4]-v[8], v[2], v[4],
                       v[1], v[15]-v[1]-v[4]-v[8], v[4],
                       v[1], v[2], v[4],
                       v[15]-v[2]-v[4]-v[8], v[2], v[4],
                       v[1], v[15]-v[1]-v[4]-v[8], v[4],
                       v[1], v[2], v[15]-v[1]-v[2]-v[8]),ncol=3,byrow = T);
   par3d(windowRect = c(100,100,900,900),zoom=2)
   um=matrix(c(-0.862371,0.5039079,-0.04891979,0,-0.1309225,-0.1286247,0.98301315,0,0.4890557,0.8541268,0.17689507,0,0,0,0,1),ncol=4,byrow=T)
   view3d(userMatrix=um)
   if (is.null(col)){col="black"}
   plot3d(vertices2,type="l",
          xlab="",
          ylab="",
          zlab = "",
          col=col,
          lwd=1,
          main=paste("Imputation set for ","v=(",toString(v),")",sep=""),
          box=F)
   }

}
