
library(itsmr)

acf_ci_ar <- function(x, phi) {
  ACF <- acf(x, lag.max = 40, ylim=c(-0.5,1))
  n <- length(x)
  acfhat <- as.numeric(ACF[1:40]$acf)
  i <- 1:40
  w <- ((1 - phi ^ (2 * i)) * (1 + phi ^ 2)) / (1 - phi ^ 2) -
    2 * i * phi ^ (2 * i)
  CIup <- acfhat + 1.96 * sqrt(w / n)
  
  CIdown <- acfhat - 1.96 * sqrt(w / n)
  points(i, CIup, col="blue", lty=2, type="l")
  points(i, CIdown, col="blue", lty=2, type="l")
  points(i, phi ^ i, col="red", pch=8)
  return(list(high = CIup, low = CIdown))
}

# Section 1.5.2 (Moving average filetr for even period d=2q) 
smooth.ma.even <- function(x, q){
 n <- length(x)
 d <- 2*q
 y <- rep(0, n-2*q)
 i <- 0
 for (t in (q+1):(n-q)){
   i <- i+1
   y[i] <- (0.5*x[t-q] + sum(x[(t-q+1):(t+q-1)]) + 0.5*x[t+q])/d
 }
 return(y)
}


# Section 1.6 Fit AR models using Yule-Walker (method="yw") or Burg (method="burg")
# or MA models using innovations algorithm (method="inv") from lag 0 to m
# and find the model with minimum order (x: time series)
minAICC <- function(x, m, method){
 aicc <- rep(1,m+1)
 aicc[1] <- AICC_order0(x)
 if (method=="yw")         { for (i in 1:m){aicc[i+1] <- YW(x, p=i)$aicc}
 } else if (method=="burg"){ for (i in 1:m){aicc[i+1] <- BURG(x, p=i)$aicc}
 } else if (method=="inv") { for (i in 1:m){aicc[i+1] <- INV_MA(x, q=i, m=i+15)$aicc}
 }
 min.order <- which.min(aicc) - 1
 min.aicc <- aicc[which.min(aicc)]
 list(min.order=min.order, min.aicc=min.aicc, aicc=aicc)
}


# Section 2.5.3 (Durbin-Levinson Algorithm)
# Inputs (n: no. of predictors X1,...,Xn,
# gn: (n+1) vector containg ACVFs gamma_0,gamma_1,...,gamma_n)
# Outputs (v: (n+1) vector containing MSE v0,v1,...,vn,
# P: n x n lower triangular matrix. mth row contains phi_{m1},...,phi_{mm}) 

DL <- function(n, gn){
 v <- rep(0, n+1)                 
 v[1] <- gn[1]
 P <- matrix(0, n, n)
 P[1,1] <- gn[2]/gn[1]
 for (i in 1:(n-1)){
   v[i+1] <- v[i]*(1 - P[i,i]^2) 
   P[i+1, i+1] <- (gn[i+2] - crossprod(P[i,1:i], rev(gn[2:(i+1)])))/v[i+1]
   P[i+1, 1:i] <- P[i,1:i] - P[i+1, i+1]*rev(P[i,1:i])
 }
 v[n+1] <- v[n]*(1 - P[n,n]^2) 
 list(v=v, P=P)
}



# Section 2.5.4 (Innovations Algorithm for stationary process)
# Inputs (n: number of predictors X1,...,Xn,
# gn: (n+1) vector containg ACVFs gamma_0,gamma_1,...,gamma_n)
# Outputs (v: (n+1) vector containing MSE v0,v1,...,vn,
# T: n x n lower triangular matrix. mth row contains theta_{m1},...,theta_{mm}) 

INV <- function(n, gn){
 v <- rep(0, n+1)                  
 v[1] <- gn[1]
 T <- matrix(0,n,n)
 for (i in 1:n){
  for (k in 0:(i-1)){
   if (k==0) { T[i, i-k] <- gn[i-k+1]/v[k+1] }
   if (k>0)  { T[i, i-k] <- (gn[i-k+1] - crossprod(rev(T[k,1:k]*T[i,(i-k+1):i]), v[1:k]))/v[k+1] }
  }
  v[i+1] <- gn[1] - crossprod(rev(T[i,1:i]^2), v[1:i]) 
 }
 list(v=v, T=T)
}



# Section 3.3 (Covariance function of Wt) 
# par: list containing phi, theta and sigma2 (parameters of ARMA(p,q) Xt)

kappa <- function(i, j, par){
 phi <- par$phi;    theta <- par$theta;   sigma2 <- par$sigma2
 p <- length(phi);  q <- length(theta);
 m <- max(p, q)
 gX <- aacvf(par, h=m)
 theta <- c(1, theta)
 b <- abs(i-j)
 if (max(i,j) <= m) { 
	kappa <- gX[b+1]/sigma2 
 } else if (min(i,j) > m & b <= q)  { 
	kappa <- crossprod(theta[1:(q-b+1)], theta[(b+1):(q+1)]) 
 } else if (min(i,j) <= m & max(i,j) <= (2*m) & b <= q){
     L <- abs((1-b):(p-b))
	L <- L + 1
	kappa <- (gX[b+1] - crossprod(phi, gX[L]))/sigma2 
 } else {
	kappa <- 0
 }
return(kappa)
}



# Section 3.3 (Innovations Algorithm for non-stationary process Wt) 
# Inputs (n: number of predictors X1,...,Xn,
# par: list containing phi, theta and sigma2 (parameters of ARMA(p,q) Xt)
# Outputs (v: (n+1) vector containing MSE v0,v1,...,vn,
# T: n x n lower triangular matrix. mth row contains theta_{m1},...,theta_{mm}) 

INV_W <- function(n, par){
 v <- rep(0, n+1)                  
 v[1] <- kappa(1,1, par)
 T <- matrix(0,n,n)
 for (i in 1:n){
  for (k in 0:(i-1)){
   if (k==0) { T[i,(i-k)] <- kappa(i+1, 1, par)/v[k+1] }
   if (k>0) { T[i,(i-k)] <- (kappa(i+1, k+1, par) - 
	crossprod(rev(T[k,1:k]*T[i,(i-k+1):i]), v[1:k]))/v[k+1] }
  }
 v[i+1] <- kappa(i+1,i+1, par) - crossprod(rev(T[i,1:i]^2), v[1:i]) 
 }
 list(v=v, T=T)
}


# Section 3.3 (One-step prediction for ARMA model)
# Inputs (x: observed time series,
# par: list of parameters (phi, theta, sigma2) of ARMA model)

onestep_pred_ARMA <- function(x, par){
 phi <- par$phi;  theta <- par$theta;  sigma2 <- par$sigma2
 n <- length(x);  p <- length(phi);    q <- length(theta)
 m <- max(p,q)
 F <- INV_W(n, par)
 T <- F$T
 r <- F$v 
 v <- sigma2*r
 xhat <- rep(0, n+1)
 for (i in 1:n){
   if (i < m){
     xhat[i+1] = crossprod(T[i,1:i], rev(x[1:i] - xhat[1:i]))
     } else {
     xhat[i+1] = crossprod(phi, rev(x[(i-p+1):i])) +
                crossprod(T[i,1:q], rev(x[(i-q+1):i] - xhat[(i-q+1):i])) 
 }}
 res <- (x - xhat[1:n])/sqrt(r[1:n])
 rescaled_res <- res/sqrt(sigma2)
 list(xhat=xhat, v=v, res=res, rescaled_res=rescaled_res)
}



# Section 3.3.1 (h-step prediction for ARMA model assuming n>m=max(p,q))
# x: time series, par: list of parameters (phi, theta, sigma2) of ARMA model
# H: no. of steps ahead

hstep_pred_ARMA <- function(x, par, H){
 phi <- par$phi;    theta <- par$theta;   sigma2 <- par$sigma2
 n <- length(x);    p <- length(phi);     q <- length(theta)
 m <- max(p,q)
 F1 <- onestep_pred_ARMA(x, par)
 xhat <- F1$xhat
 F2 <- INV_W(n+H-1, par)
 T <- F2$T
 v <- sigma2*F2$v
 if (length(phi)==1 & phi[1]==0){
   hp <- rep(0, H)
   hp[1] <- xhat[n+1]
   for (h in 2:H){
     if (h <= q){
       hp[h+p-1] = crossprod(T[n+h-1,h:q], rev(x[(n-q+h):n] - xhat[(n-q+h):n]))  
     }
   }  
 } else {
   hp <- rep(0, H+p-1)
   hp[1:p] <- c(x[(n-(p-1)+1):n], xhat[n+1])
   for (h in 2:H){
     if (h <= q){
       hp[h+p-1] = crossprod(phi, rev(hp[(h-1):(h+p-2)])) +
                crossprod(T[n+h-1,h:q], rev(x[(n-q+h):n] - xhat[(n-q+h):n])) 
     } else {
       hp[h+p-1] = crossprod(phi, rev(hp[(h-1):(h+p-2)])) 
     }
   } 
   if (p>=2) {hp <- hp[-(1:(p-1))]} 
 }
 chi <- rep(0, H+1)
 chi[1] <- 1
 for (j in 1:H){
   chi[j+1] <- crossprod(phi[1:min(p,j)], rev(chi[(j-min(p,j)+1):j]))
 }
 mse <- rep(0, H)
 mse[1] <- v[n+1]
 T <- cbind(rep(1,nrow(T)),T)
 for (h in 2:H){
   A <- rep(0, h)
   for (j in 1:h){
     for (r in 1:j){
        A[j] <- A[j] + chi[r]*T[n+h-r,j-r+1]
   }}
   mse[h] <- crossprod(A^2, rev(v[(n+1):(n+h)]))
 }
 list(hp=hp, mse=mse)
}


# Section 5.1.1 (Yule-Walker estimation of AR model)
# Inputs (x: time series, p: AR order)
# Outputs (phi: estimate of phi, sigma2: WN variance est (RSS/n),
# sigma2_yw: Yule-Walker WN variance est (v_m), aicc: AICC)

YW <- function(x, p){
 n <- length(x)
 F <- yw(x, p)                  # from itsmr library
 ACVF <- acvf(x, h=p)
 g0 <- ACVF[1]
 gp <- ACVF[2:(p+1)]
 sigma2 <- g0 - crossprod(F$phi, gp)
 sigma2 <- sigma2[1,1]
 aicc <- F$aicc
 loglik <- (aicc - 2*(p+1)*n/(n-p-2))/(-2)
 list(phi=F$phi, sigma2=F$sigma2, sigma2_yw=sigma2, loglik=loglik, aicc=aicc)
}


# Section 5.1.1 (95% Confidence intervals for phihat)
# Inputs (x: time series, phihat and sigma2hat: estimates of phi and sigma2)
# Outputs (CI: 95% condfidence intervals of phihat, se: standard errors of phihat)

phiCI <- function(x, phihat, sigma2hat){
  n <- length(x)
  p <- length(phihat)
  ACVF <- acvf(x, h=p-1)
  if (p==1){
    se <- sqrt(sigma2hat/(ACVF*n))
    CI <- phihat + c(-1,1)*1.96*se
  } else if (p>1) {
    CI <- matrix(0, p, 2)
    Gp <- matrix(0, p, p)
    for (i in 1:p){
      for (j in 1:p){
        Gp[i,j] <- ACVF[abs(i-j)+1]
    }}
    R <- chol(Gp)
    Ri <- solve(R)
    Gpi <- Ri %*% t(Ri)
    se <- sqrt(sigma2hat*diag(Gpi)/n)
    for (i in 1:p){
      CI[i,] <- phihat[i] + c(-1,1)*1.96*se[i]
    }
  }
  list(CI=CI, se=se)
}


# Section 5.1.2 (Burg estimation of AR model)
# Inputs (x: time series, p: AR order)
# Outputs (phi: estimate of phi, sigma2: WN variance est (RSS/n),
# sigma2_burg: Burg WN variance est (v_m), aicc: AICC)

BURG <- function(x, p){
 n <- length(x)
 F1 <- burg(x, p)                  # from itsmr library
 F2 <- ar.burg(x, aic=FALSE, order.max=p, demean=FALSE, var.method=2)
 aicc <- F1$aicc
 loglik <- (aicc - 2*(p+1)*n/(n-p-2))/(-2)
 list(phi=F1$phi, sigma2=F1$sigma2, sigma2_burg=F2$var.pred, loglik=loglik, aicc=aicc)
}


# Section 5.1.3 (Innovations Algorithm for fitting MA model) 
# Inputs (x: time series, q: MA order, m: maximum lag)
# Outputs (theta: MA coef, sigma2: est of WN variance (RSS/n)
# sigma2_inv: innovation WN variance est (v_m), aicc: AICC, 
# cf and se: theta_{m1},...,theta_{mm} and their standard errors, 
# rt: cf/(1.96*se))

INV_MA <- function(x, q, m){
 F1 <- ia(x, q, m)
 n <- length(x)
 gn <- acvf(x, h=m)
 F2 <- INV(m, gn)
 cf <- F2$T[m,]
 se <- sqrt(cumsum(c(1,cf^2))[1:m]/n)
 rt <- cf/(1.96*se)
 list(theta=F1$theta, sigma2=F1$sigma2, sigma2_inv=F2$v[m+1], 
	aicc=F1$aicc, cf=cf, se=se, rt=rt)
}


# Section 5.1.3 (Innovations Algorithm for fitting ARMA model) 
# Inputs (x: time series, p: AR order, q: MA order, m: maximum lag)

INV_ARMA <- function(x, p, q, m){
 n <- length(x)
 gn <- acvf(x, h=m)
 F <- INV(m, gn)
 cf <- c(1, F$T[m,])
 A <- matrix(0, p, p)
 for (k in 1:p){ 
     j <- k+q
 	A[k,1:min(j,p)] <- rev(cf[(j-min(j,p)+1):j]) 
 }
 phi <- solve(A, cf[(q+2):(q+p+1)])
 theta <- rep(0, q)
 for (j in 1:q){
   theta[j] <- cf[j+1] - crossprod(phi[1:min(j,p)], rev(cf[(j-min(j,p)+1):j]))
 }
 roots <- polyroot(c(1, -phi))
 if (any(abs(roots)<1)){
   cat(p, q, "noncausal","\n")
   list(theta=theta, phi=phi, aicc=NA)
 } else {
   par <- list(phi=phi, theta=theta, sigma2=1)
   osp <- onestep_pred_ARMA(x, par)
   sigma2 <- mean(osp$res^2)
   loglik <- -0.5*n*log(2*pi*sigma2) - 0.5*sum(log(osp$v)) - 0.5*n
   aicc <- -2*loglik + 2*(p+q+1)*n/(n-p-q-2)
   list(theta=theta, phi=phi, sigma2=sigma2, loglik=loglik, aicc=aicc)
 }
}


# Section 5.1.3 Find ARMA model (p>0, q>0) with minimum AICC using innovations algorithm
minAICC_ARMA_prelim <- function(x, pmax, qmax){
 aicc <- matrix(0, pmax, qmax)
 for (p in 1:pmax){
  for (q in 1:qmax){
   tryCatch({
     m <- p+q+15
     aicc[p, q] <- INV_ARMA(x, p, q, m)$aicc
   }, 
     warning = function(w){cat(p, q, "Warning:",conditionMessage(w), "\n")},
	error = function(e){cat(p, q, "Error:",conditionMessage(e), "\n")})
 }}
 aicc[aicc==0] <- NA
 idx <- which(aicc==min(aicc, na.rm=TRUE), arr.ind=TRUE)
 p <- as.numeric(idx[1,1])
 q <- as.numeric(idx[1,2])
 min.aicc <- aicc[p, q]
 list(p=p, q=q, min.aicc=min.aicc, aicc=aicc)
}


# Section 5.1.4 (Hannan-Rissanen Algorithm for fitting ARMA model) 
# Inputs (x: time series, p: AR order, q: MA order)

HR <- function(x, p, q){
 n <- length(x)
 m <- p+q+20
 phi <- YW(x, p=m)$phi
 X <- matrix(0, n-m, m)
 for (j in 1:m){ X[,j] <- x[(m-j+1):(n-j)] }
 z <- x[(m+1):n] - X %*% phi
 Z <- matrix(0, n-m-q, q)
 for (j in 1:q){ Z[,j] <- z[(q-j+1):(n-m-j)] }
 X <- cbind(X[(q+1):(n-m), 1:p], Z)
 y <- x[(m+1+q):n]
 fm <- lm(y~X-1)
 phi <- coef(fm)[1:p]
 theta <- coef(fm)[(p+1):(p+q)]
 sigma2_hr <- sum(fm$residuals^2)/(n-m-q)
 par <- list(phi=phi, theta=theta, sigma2=1)
 osp <- onestep_pred_ARMA(x, par)
 sigma2 <- mean(osp$res^2)
 loglik <- -0.5*n*log(2*pi*sigma2) - 0.5*sum(log(osp$v)) - 0.5*n
 aicc <- -2*loglik + 2*(p+q+1)*n/(n-p-q-2)
 list(theta=theta, phi=phi, sigma2_hr=sigma2_hr, sigma2=sigma2, loglik=loglik, aicc=aicc)
}


# Section 5.2 (Calculate AICC of AR/MA model of order 0) x: time series
AICC_order0 <- function(x){
 n <- length(x)
 sigma2 <- sum(x^2)/n
 aicc <- n*log(2*pi*sigma2) + n + 2*n/(n-2)
 return(aicc)
}


# Section 5.2 Find ARMA model (p>=0, q>=0) with minimum AICC using maximum likelihood
minAICC_ARMA_loglik <- function(x, pmax, qmax){
 n <- length(x)
 aicc <- matrix(0, pmax+1, qmax+1)
 for (p in 0:pmax){
  for (q in 0:qmax){
   tryCatch({
    L <- arima(x, order=c(p, 0, q), method="ML", include.mean=FALSE,
			optim.control=list(maxit=1000))$loglik
    aicc[p+1, q+1] <- -2*L + 2*(p+q+1)*n/(n-p-q-2)
   }, 
     warning = function(w){cat(p, q, "Warning:",conditionMessage(w), "\n")},
	error = function(e){cat(p, q, "Error:",conditionMessage(e), "\n")})
 }}
 aicc[aicc==0] <- NA
 idx <- which(aicc==min(aicc, na.rm=TRUE), arr.ind=TRUE)
 p <- as.numeric(idx[1,1])-1
 q <- as.numeric(idx[1,2])-1
 min.aicc <- aicc[(p+1), (q+1)]
 list(p=p, q=q, min.aicc=min.aicc, aicc=aicc)
}


# Section 6.4 Forecasting for SARIMA model
# x: original series, y = (1 - B)^d (1 - B^s)^D: stationary series
# fm: fitted ARMA model, mu: mean correction, H: number of steps ahead

hstep_pred_SARIMA <- function(x, y, fm, mu, d, D, s, H){
  p <- fm$arma[1];	q <- fm$arma[2]; P <- fm$arma[3]; Q <- fm$arma[4]; 
  cf <- coef(fm)
  sigma2 <- fm$sigma2 
  n <- length(x)

  pdn <- predict(fm, n.ahead=H)
  pY <- pdn$pred + mu
  pX <- c(x, rep(0,H))
  ar <- rep(0, d+s*D+1)
  for (i in 0:D){
    for (j in 0:d){
      ar[s*i+j+1] <- ar[s*i+j+1] + choose(D,i)*choose(d,j)*(-1)^(i+j)
  }} 
  aro <- ar 
  ar <- -ar[-1]
  m <- length(ar)
  for (h in 1:H){
    pX[n+h] <- pY[h] + crossprod(ar, rev(pX[(n+h-m):(n+h-1)]))
  }
  pX <- pX[(n+1):(n+H)]


  # MSE
  n <- length(y)
  if (p>0){phi <- c(-1, cf[1:p])} else {phi <- 1}
  if (q>0){theta <- c(1, cf[(p+1):(p+q)])} else {theta <- 1}
  if (P>0){Phi <- c(-1, cf[(p+q+1):(p+q+P)])} else {Phi <- 1}
  if (Q>0){Theta <- c(1, cf[(p+q+P+1):(p+q+P+Q)])} else {Theta <- 1}
  arcf <- rep(0, p+s*P+1)
  macf <- rep(0, q+s*Q+1)
  for (i in 0:P){
    for (j in 0:p){
      arcf[s*i+j+1] <- arcf[s*i+j+1] + phi[j+1]*Phi[i+1]
  }} 
  for (i in 0:Q){
    for (j in 0:q){
      macf[s*i+j+1] <- macf[s*i+j+1] + theta[j+1]*Theta[i+1]
  }} 
  arcfo <- arcf
  arcf <- -(arcf[-1])
  macf <- macf[-1]
  if (length(arcf)==0) {arcf <- 0}
  if (length(macf)==0) {macf <- 0}
  par <- list(phi=arcf, theta=macf, sigma2=sigma2)
  F1 <- onestep_pred_ARMA(y, par)
  xhat <- F1$xhat
  F2 <- INV_W(n+H-1, par)
  T <- F2$T
  v <- sigma2*F2$v

  L1 <- length(aro)-1
  L2 <- length(arcfo)-1
  AR <- rep(0, L1 + L2 + 1)
  for (i in 0:L1){
    for (j in 0:L2){
      AR[i+j+1] <- AR[i+j+1] + aro[i+1]*arcfo[j+1]
  }}
  AR <- -AR[-1]
  chi <- ARMAtoMA(ar=AR, ma=0, lag.max=H)
  chi <- c(1, chi)
  mse <- rep(0, H)
  mse[1] <- v[n+1]
  T <- cbind(rep(1,nrow(T)),T)
  for (h in 2:H){
    A <- rep(0, h)
    for (j in 1:h){
      for (r in 1:j){
         A[j] <- A[j] + chi[r]*T[n+h-r,j-r+1]
    }}
    mse[h] <- crossprod(A^2, rev(v[(n+1):(n+h)]))
  }
  list(pX=pX, mse=mse)
}



