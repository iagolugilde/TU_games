#' @title \eqn{\epsilon-}core set
#' @description Draws the \eqn{\epsilon-}core of a 2, 3 or 4 person compromise admissible TU game.
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
#' @param epsilon The parameter \eqn{\epsilon\in R^n}{\epsilon in R^n}.
#' @param set A logical value. By default, set=TRUE.
#' @param col Color.
#' @return  If set=TRUE, it displays the \eqn{\epsilon-}core of the game defined by \eqn{v} inside the imputation set.
#' If set=FALSE, it displays the \eqn{\epsilon-}core inside the last graph.
#' @details Given \eqn{\epsilon\in R^n}{\epsilon in R^n}, the \eqn{\epsilon-}core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
#'  such that \eqn{x(S)\ge v(S)-\epsilon} for all coalition \eqn{S} in \eqn{N}, where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#'  \deqn{C_{\epsilon}(N,v)=\{x\in I(N,v):x(S)\ge v(S)-\epsilon\; \forall S\in N,\epsilon\in R^n\}}{C\epsilon(N,v)={x in I(N,v):x(S)\ge v(S)-\epsilon for all S in N, \epsilon in R^n}}
#'
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics text
#' @importFrom graphics polygon
#' @importFrom rgl plot3d
#' @importFrom rgl view3d
#' @importFrom rgl rgl.triangles
#' @importFrom rgl points3d
#' @importFrom rgl grid3d
#' @importFrom rgl par3d
#' @importFrom rgl lines3d
#' @importFrom geometry convhulln
#' @importFrom graphics grid
#' @examples
#' v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
#' epsilon=0.1
#' EpsilonCoreSet(v,epsilon)
#' @references Gillies, D. B. (1953). Some theorems on n-person games. Princeton University.
#' @references Shapley, L. S., & Shubik, M. (1966). Quasi-cores in a monetary economy with nonconvex preferences. Econometrica: Journal of the Econometric Society, 805-827.
#' @seealso  \link{CoreCoverSet}, \link{CoreSet}, \link{EpsilonCoreVertices}, \link{HarsanyiSet}, \link{ImputationSet}, \link{LeastCoreSet}, \link{WeberSet}
#' @export

EpsilonCoreSet=function(v,epsilon=0,set=TRUE,col=NULL){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC

  if(n>4)
    stop('The epsilon core set can only be represented if n<=4',call.=F)
  nC=Characteristicdata(v)$nC
  if(n==2){
    if(v[3]==(v[1]+v[2])){
      vi=c(v[1],v[2])
      return(paste('The core set is a single-point: (',toString(vi),')','-->If epsilon>0, the epsilon core is an empty set',sep=""))
    }
    if(v[3]<(v[1]+v[2])){
      return(paste('The core set is empty-->The epsilon core set is empty',sep=""))
    }
    m=UtopiaPayoffs(v)$m
    # The length of the set of awards
    Delta=v[3]-sum(m)
    satura=(v[3]-m[2]-m[1])/2
    if(epsilon>satura)
      stop('The epsilon is too big, the epsilon core is empty',call.=F)
    V=matrix(c(m[1]+epsilon,v[3]-m[1]-epsilon,v[3]-m[2]-epsilon,m[2]+epsilon),ncol=2,byrow=T)
    #The extreme points of the set of awards (no repeated points)
    V=unique(V)
    #If the set of awards is a line segment
    #The axis are determined by the minimal rights
    if(set==TRUE){plot(m[1],
                       m[2],
                       type="n",
                       xlim=c(m[1],v[3]-m[2]),
                       ylim=c(m[2],v[3]-m[1]),
                       xlab=expression("x"[1]),
                       ylab=expression("x"[2]),
                       main=c("Epsilon core set")
    )
      subtitle=paste("Epsilon core set for ","v=(",toString(v),")"," and ","epsilon=",toString(epsilon),sep="")
      mtext(subtitle,side=3,line=0.5,cex=0.7)
      grid()}
    lines(c(m[1],v[3]-m[2]),c(m[2],m[2]))
    lines(c(m[1],m[1]),c(v[3]-m[1],m[2]))
    # Default color for the set fo awards vector: RED
    if (is.null(col)){col="red"}
    if(dim(V)[1]==1){points(V,col=col)}else{
    lines(t(V[,1]),t(V[,2]),col=col)}
  }else if(n==3){

    m=UtopiaPayoffs(v)$m
    V=EpsilonCoreVertices(v,epsilon)
    imputation=matrix(c(v[7]-v[2]-v[4],v[2],v[4],v[1],v[7]-v[1]-v[4],v[4],v[1],v[2],v[7]-v[1]-v[2],v[7]-v[2]-v[4],v[2],v[4]),ncol=3,byrow=T)
    imputation=unique(imputation)
    Delta=v[7]-v[1]-v[2]-v[4];
    #The corresponding extreme points of the equilateral triangle in our window
    equilatero=matrix(c(0,0,Delta,0,Delta/2,sqrt(3)/2*Delta),ncol=2,byrow=T);
    P=t(equilatero)%*%solve(t(imputation));
    vertP=t(P%*%t(V));

    if(set==TRUE){ plot(0,
                        0,
                        xlim=c(min(c(equilatero[,1],vertP[,1]))-0.3,max(c(equilatero[,1],vertP[,1]))+0.3),
                        ylim=c(min(c(equilatero[,2],vertP[,2])),max(c(equilatero[,2],vertP[,2]))*1.1),
                        type="n",
                        main="Epsilon core set",
                        axes=F,
                        xlab=paste("v=(",toString(v),")",sep=""),
                        ylab="",
                        asp=1)
      text(equilatero[1,1],equilatero[1,2]+0.2, paste("(",toString(imputation[1,]),")"))
      text(equilatero[2,1],equilatero[2,2]+0.2, paste("(",toString(imputation[2,]),")"))
      text(equilatero[3,1],equilatero[3,2]-0.2,paste("(",toString(imputation[3,]),")"))
      polygon(t((equilatero[,1])),t((equilatero[,2])))}
    if(dim(V)[1]==0)
      stop('The epsilon is too big, the epsilon core is empty',call.=F)
    if(dim(V)[1]==1){
      if (is.null(col)){col="red"}
      points((vertP),col=col)
    }else if(dim(V)[1]==2){
      # Default color for the set fo awards vector: RED
      if (is.null(col)){col="red"}
      lines(t((vertP[,1])),t((vertP[,2])),col=col,lwd=3)
    }else{
      # Default color for the set fo awards vector: BEIGE
      if (is.null(col)){col="beige"}
      vertP=Polygonorder((vertP))$C
      polygon(vertP,col=col)
    }
  }else if(n==4){

    Vert=CoreVertices(v)
    vertices2=matrix(c(v[1], v[2], v[4],v[1], v[2], v[15]-v[1]-v[2]-v[8],
                       v[15]-v[2]-v[4]-v[8], v[2], v[4],
                       v[1], v[15]-v[1]-v[4]-v[8], v[4],
                       v[1], v[2], v[4],
                       v[15]-v[2]-v[4]-v[8], v[2], v[4],
                       v[1], v[15]-v[1]-v[4]-v[8], v[4],
                       v[1], v[2], v[15]-v[1]-v[2]-v[8]),ncol=3,byrow = T);


    if(set==TRUE){ par3d(windowRect = c(100,100,900,900),zoom=2)
      um=matrix(c(-0.862371,0.5039079,-0.04891979,0,-0.1309225,-0.1286247,0.98301315,0,0.4890557,0.8541268,0.17689507,0,0,0,0,1),ncol=4,byrow=T)
      view3d(userMatrix=um)
      plot3d(vertices2,type="l",
             xlab="",
             ylab="",
             xlim=c(-0.1,v[nC]),
             ylim=c(-0.1,v[nC]),
             zlim=c(-0.1,v[nC]),
             zlab = "",
             col="black",
             lwd=1,
             main=paste("Epsilon core set for ","v=(",toString(v),")"," and ","epsilon=",toString(epsilon),sep=""),
             box=F)}
    Vert=EpsilonCoreVertices(v,epsilon)
    if(dim(Vert)[1]==0)
      stop('The epsilon is too big, the epsilon core is empty',call.=F)
    if(dim(Vert)[1]==1){
      if (is.null(col)){col="red"}
      points3d(Vert[,1],Vert[,2],Vert[,3],col=col,lwd=5)
      return(grid3d(c("x-+", "y-+", "z-+")))
    }
    rango=qr(Vert)$rank
    Vert1=round(Vert)
    for(i in 1:dim(Vert)[2]){
      a=0
      for(j in 1:(dim(Vert1)[1]-1)){
        if(Vert1[j,i]==Vert1[j+1,i]){a=a+1}
      }
      if(a==(dim(Vert)[1]-1)){rango=rango-1}
    }
    V=Vert[,1:3]
    V=unique(V);
    # The set of awards is a line segment
    if(dim(V)[1]==2){
      # Default color for the set fo awards vector: RED
      if (is.null(col)){col="red"}
      lines3d(t(V[,1]),t(V[,2]),t(V[,3]),col=col,lwd=5)
      grid3d(c("x-+", "y-+", "z-+"))
      # The set of awards has dimension 2 or 3
    }else if(rango<4){
      V=Polygonorder(V)$C
      V=rbind(V,V[1,])
      if(is.null(col)){col="red"}
      lines3d(V,col=col,lwd=5)
      grid3d(c("x-+", "y-+", "z-+"))

    }else{
      points3d(t(V[,1]),t(V[,2]),t(V[,3]),col="black")
      if(is.null(col)){col="red"}
      ts.surf1 <- t(convhulln(V))
      convex1 <-  rgl.triangles(V[ts.surf1,1],V[ts.surf1,2],V[ts.surf1,3],col=col,alpha=0.2)
      grid3d(c("x-+", "y-+", "z-+"))
    }
  }
}
