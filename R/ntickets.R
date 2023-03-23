#' @title ntickets
#'
#' @param N n variable
#' @param gamma gamma variable
#' @param p p variable
#'
#' @return two plots
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {
  n <- N:(N + round(0.1 * N))
  nd <- 1 - gamma - pbinom(N, n, p)
  nc <- 1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1-p)))

  d <- N
  while(N != qbinom(1-gamma, d, p)){
    d = d + 1
  }

  c <- which.min(abs(nc))

  plot(n, nd, type='c', main=paste("Objective Vs n to find optimal tickets sold\n", "(", d, ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective", pch = 16)
  abline(h = 0, v = d, lwd = 2, col = "orange")

  plot(n, nd, type='l', main=paste("Objective Vs n to find optimal tickets sold\n", "(", n[c], ")", "gamma = ", gamma, "N = ", N, "continous"), ylab = "Objective")
  abline(h = 0, v = n[c], lwd = 2, col = "green")
  l<- list(nd = d, nc = c, N=N, p=p, gamma=gamma)
  print(l)
}
