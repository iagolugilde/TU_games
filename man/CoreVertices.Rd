% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoreVertices.R
\name{CoreVertices}
\alias{CoreVertices}
\title{Core vertices}
\usage{
CoreVertices(v)
}
\arguments{
\item{v}{Characteristic function in binary order.}
}
\value{
The vertices of the core.
}
\description{
This function returns the vertices of the core of the TU game.
}
\details{
The core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
 such that \eqn{x(S)\ge v(S)} for all coalition \eqn{S} in \eqn{N}, where,
 \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.

 \deqn{C(N,v)=\{x\in I(N,v):x(S)\ge v(S)\; \forall S\in N\}}{C(N,v)={x in I(N,v):x(S)\ge v(S) for all S in N}}

The core allocations provide the agents with an incentive to maintain the grand coalition and it can be empty.
}
\examples{
v=c(0,0,0,0,0,0,0,0,1,4,1,3,6,8,10)
CoreVertices(v)
}
\references{
Edgeworth, F. Y. (1881). Mathematical psychics: An essay on the application of mathematics to the moral sciences (No. 10). CK Paul.

Gillies, D. B. (1953). Some theorems on n-person games. Princeton University.
}
\seealso{
\link{BalancedGame}, \link{CoreCoverVertices}, \link{CoreSet}, \link{EpsilonCoreVertices}, \link{ImputationVertices}, \link{LeastCoreVertices}, \link{WeberVertices}
}
