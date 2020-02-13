library(R.matlab)
library(Matrix)
library(functional)
rm(list=ls())

## Handle function ##
phi_v1 <- function(x,EPS1){
  return( sqrt(abs(x)^2 + EPS1) )
}
wfun_v1 <- function(x, EPS1){
  return(  1/(sqrt(abs(x)^2 + EPS1)) )
}
phi_v2 <- function(x,EPS1){
  return(  abs(x) - EPS1 * log(abs(x) + EPS1) )
}
wfun_v2 <- function(x,EPS1){
  return( 1/(abs(x) + EPS1) )
}
theta <- function(x,EPS0,r){
  sum(x[which(x>EPS0)]) - (r*sum(x[which(x < -EPS0)])) + sum( (1+r)/(4*EPS0)*x[which(abs(x)<=EPS0)]^2 + (1-r)/2 *  x[which(abs(x)<=EPS0)] + EPS0*(1+r)/4 )
}
H <- function(x,A,B){
  return( B%*%solve(A,x) )
}


beads <- function(y,d,fc,r,lam0,lam1,lam2){
  
  # Baseline estimation and denoising using sparsity (BEADS)
  #
  # INPUT
  #   y: Noisy observation
  #   d: Filter order (d = 1 or 2)
  #   fc: Filter cut-off frequency (cycles/sample) (0 < fc < 0.5)
  #   r: Asymmetry ratio
  #   lam0, lam1, lam2: Regularization parameters
  #
  # OUTPUT
  #   x: Estimated sparse-derivative signal
  #   f: Estimated baseline
  #   cost: Cost function history
  
  # Reference:
  # Chromatogram baseline estimation and denoising using sparsity (BEADS)
  # Xiaoran Ning, Ivan W. Selesnick, Laurent Duval
  # Chemometrics and Intelligent Laboratory Systems (2014)
  # doi: 10.1016/j.chemolab.2014.09.014
  # Available online 30 September 2014
  
  # The following parameter may be altered.
  Nit <- 30       # Nit: Number of iterations
  pen <- 'L1_v2'  # pen : penalty function for sparse derivative ('L1_v1' or 'L1_v2')
  EPS0 <- 1e-6    # cost smoothing parameter for x (small positive value)
  EPS1 <- 1e-6    # cost smoothing parameter for derivatives (small positive value)
  
  switch(pen,
         L1_v1 = {
           phi <- Curry(phi_v1, EPS1=EPS1)
           wfun <- Curry(wfun_v1, EPS1=EPS1)
         },
         L1_v2 = {
           phi <- Curry(phi_v2, EPS1=EPS1)
           wfun <- Curry(wfun_v2, EPS1=EPS1)
         },
         {
           cat('penalty mus be L1_v1, L1_v2')
           x <- c()
           cost <- c()
           f <- c()
           return()
         }
  )
  
  y <- as.vector(y)
  x <- y
  cost <- matrix(0,nrow=1,ncol=Nit)
  N <- length(y);
  BAfiltRes <- BAfilt(d,fc,N)
  A <- BAfiltRes$A
  B <- BAfiltRes$B
  
  
  e <- matrix(1,nrow=N-1,ncol=1)
  DiagD1 <- vector("list", 2)
  DiagD1[[1]] <- -e
  DiagD1[[2]] <- e
  
  DiagD2 <- vector("list", 3)
  DiagD2[[1]] <- e
  DiagD2[[2]] <- -2*e
  DiagD2[[3]] <- e

  D1 <- bandSparse(N-1,N, k=c(0,1),diagonals = DiagD1)
  D2 <- bandSparse(N-2,N, k=c(0,1,2),diagonals = DiagD2)
  D <- rbind(D1,D2)
  
  BTB <- t(B)%*%B

  
  w <- c(lam1 * matrix(1,nrow=N-1,ncol=1), lam2 * matrix(1,nrow=N-2,ncol=1))
  b <- (1-r)/2 * matrix(1,nrow=N,ncol=1)
  d <- BTB %*% solve(A,y) - lam0 * t(A) %*% b
  
  gamma <- matrix(1,nrow=N,ncol=1)

  for (i in 1:Nit){
    Diag <- vector("list",1)
    Diag[[1]] <- w*wfun(D%*%x)
    Lambda <- bandSparse((2*N)-3, k=0, diagonals = Diag)
    k <- which(abs(x) > EPS0) 
    gamma[!k] <- ((1+r)/4) / abs(EPS0)
    gamma[k] <- ((1 + r)/4) /  abs(x[k])
    DiagG <- vector("list",1)
    DiagG[[1]] <- gamma
    Gamma <- bandSparse(N,k=0, diagonals=DiagG)
    
    M <- (2*lam0*Gamma) + (t(D)%*%Lambda%*%D)
    x <- A%*% ( solve(  BTB + t(A)%*%M%*%A , d) )
    
    cost[i] <- 0.5*sum(abs(H(y-x,A,B))^2) + lam0*theta(x,EPS0,r) + lam1*sum(phi(diff(x))) + lam2*sum(phi(diff(x,differences=2)))
    #print(cost[i])
}
 
  f <- y - x - H(y-x,A,B)  
  
  return(list(x=x, f=f, cost=cost))
}

BAfilt <- function(d,fc,N){
  # [A, B] = BAfilt(d, fc, N)
  # Banded matrices for zero-phase high-pass filter.
  # The matrices are 'sparse' data type in MATLAB.
  # INPUT
  #   d  : degree of filter is 2d (use d = 1 or 2)
  #   fc : cut-off frequency (normalized frequency, 0 < fc < 0.5)
  #   N  : length of signal
  
  b1 = c(1, -1);
  if (d > 1){
    for (i in 1:(d-1)){
      b1 <- convolve(b1, rev(c(-1, 2, -1)), type='open')
    }
  }
  
  b <- convolve(b1, rev(c(-1, 1)), type='open')
  
  omc <- 2*pi*fc
  t <- ((1-cos(omc))/(1+cos(omc)))^d
  
  a <- 1
  for (i in 1:d){
    a <- convolve(a, rev(c(1,2,1)),type='open')
  }  


  a <- b + (t*a)
  NbDiag <- length(0:d)
  DiagA <- vector("list", NbDiag)
  DiagB <- vector("list", NbDiag)
  
   
  
  for (i in 0:d){
    DiagA[[i+1]] <- a[d-i+1]*matrix(1,nrow=1,ncol=(N-i))
    DiagB[[i+1]] <- b[d-i+1]*matrix(1,nrow=1,ncol=(N-i))
  }
  
  A <- bandSparse(N, k=c(0:d), diagonals = DiagA, symm=TRUE)
  B <- bandSparse(N, k=c(0:d), diagonals = DiagB, symm=TRUE)
  
  return(list(A=A, B=B))
}

# y <- as.data.frame(read.csv('../../../FLIC-FLEA/191003 DFM_14.csv'))
# 
# y <- as.matrix(read.table('../../../../Downloads/BEADS-baseline-R-code/Data_1D_Baseline_Deg2.txt', header=F, sep='\t'))
# #y <- Data$data.1D.BaselineDeg2.Noise
# res <-  beads(y$W1-runmed(y$W1, k=m), 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)
# 
# plot(y$W7, type="l")
# lines(y$W7 - beads(y$W7-runmed(y$W7, k=m), 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)$x, col="Red")
# plot(beads(y$W7-runmed(y$W7, k=m), 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)$x, type="l")
