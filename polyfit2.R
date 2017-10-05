# Penalized polynomial smoothing/regression
# --- Match derivatives at curve endpoints to the corresponding
# ----- slopes of the annual time series

# wrapper
Poly = function(...) {
  poly(..., raw=TRUE, simple=TRUE)
}

polyfit2 = function(x, y, k = 5, lambda=.5) {
  dPoly = function(beta, x, ...) {
    # Compute poly derivatives
    cbind('0'=1, Poly(x, k - 1)) %*% ((1:k) * beta)
  }
  
  # left and right slopes
  # Maybe need another if baseYear > x[1]
  mLeft = y[2] - y[1]
  mRight = y[length(y)] - y[length(y) - 1]
  
  # objective fn
  fun = function(beta) {
    SSE = sum((y - Poly(x, k) %*% beta)^2)
    
    devs = dPoly(beta, x[c(1, length(x))]) - c(mLeft, mRight)
    
    SSE + lambda * t(devs) %*% devs
  }
  
  # find the optimal betas
  optBeta = optim(par=rep(0, k), fn=fun)$par
  
  structure(as.matrix(optBeta), class='polyfit2')
}

# A simple predict method
predict.polyfit2 = function(coef, newX) {
  fit = Poly(newX, k) %*% coef
  as.vector(fit)
}

