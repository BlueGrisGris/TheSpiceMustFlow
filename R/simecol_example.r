library(simecol)
competition <- new("odeModel",
  main = function (time, init, parms) {
    with(as.list(c(init, parms)), {
      dS  <- ds(Sin , mu1 , S , X1 , mu2 ,  X2)
      dX1 <- mu1 * S * X1 - d1 * X1
      dX2 <- mu2 * S * X2 - d2 * X2
      list(c(dS, dX1, dX2))
    })
  },
  parms  = c(Sin = 1, mu1=0.21, mu2=0.1, d1=0.2, d2 = 0.1),
  times  = c(from = 0, to = 100, by = 0.5),
  init   = c(S=1, X1=0.1, X2=0.1),
  solver = "lsoda"
)

competition <- sim(competition)
plot(competition)

ds<- function(Sin,mu1,S,X1,mu2,X2){
	return( dS  <- Sin^2 - mu1 * S * X1 - mu2 * S * X2)
}#end ds

upca <- new("odeModel",
  main = function(time, init, parms) {
    u      <- init[1]
    v      <- init[2]
    w      <- init[3]
    with(as.list(parms), {
      du <-  a * u           - alpha1 * f(u, v, k1)
      dv <- -b * v           + alpha1 * f(u, v, k1) +
                             - alpha2 * f(v, w, k2)
      dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)
      list(c(du, dv, dw))
    })
  },
  equations  = list(
    f1 = function(x, y, k){x*y},           # Lotka-Volterra
    f2 = function(x, y, k){x*y / (1+k*x)}  # Holling II
  ),
  times  = c(from=0, to=100, by=0.1),
  parms  = c(a=1, b=1, c=10, alpha1=0.2, alpha2=1,
    k1=0.05, k2=0, wstar=0.006),
  init = c(u=10, v=5, w=0.1),
  solver = "lsoda"
)

upca@equations$f <- upca@equations$f2

plot(sim(upca))
