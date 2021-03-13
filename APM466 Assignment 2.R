

##### APM466 Winter 2021
##### Assignment 2
##### Sierra Watkins - 1005473685

N <- 52 # periods
P0 <- 1 # initial price
r <- 0 # rf rate
ra <- 0.10 # rate of asset change / period

############### 4 Up-Swing Call option 
AssetTree <- function(P, alpha, N) {
  
  # alpha is rate stock can go up or down each period
  
  A = matrix(0, nrow=N+1, ncol=N+1)
  
  for (i in 1:(N+1)) {
    for (j in 1:(N+1)) {
      if( i + j < N + 3){
        A[i, j] = P * (1+alpha)^(i-1) / (1+alpha)^(j-1)
      } else {A[i, j] = 0}
      
    }  }
  return(A)
}


SwingTree <- function(P, alpha, N) { # 1 up swing

  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(i + j - 2 == N) {
        B[i, j] = max(c(0,A[i, j]-P))
      } else if(i + j - 2 < N) {
        B[i, j] = A[i, j]*((B[i+1, j] - B[i, j+1])/(A[i+1, j]-A[i, j+1]))+ B[i, j+1] - (A[i, j+1]*
                         ((B[i+1, j] - B[i, j+1])/(A[i+1, j]-A[i, j+1])))
        # price * delta + Cd - Sd*delta
      } else {B[i, j] = 0}
      
    }  }
  return(B)
}


SwingTreeB <- function(P, alpha, N) { # 2 up swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] > P) {
        B[i, j] = C[i, j] + (A[i, j] - P)
        
        
      } else {B[i, j] = C[i, j]
      }
      
    }  }
  return(B)
}

SwingTreeC <- function(P, alpha, N) { # 3 up swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  D = SwingTreeB(P, alpha, N)
  
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] > P) {
        B[i, j] = D[i, j] + (A[i, j] - P)
        
        
      } else {B[i, j] = D[i, j]
      }
      
    }  }
  return(B)
}

SwingOptionP <- function(P, alpha, N) { # 4 up swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  D = SwingTreeB(P, alpha, N)
  E = SwingTreeC(P, alpha, N)
  
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] > P) {
        B[i, j] = E[i, j] + (A[i, j] - P)
        
        
      } else {B[i, j] = E[i, j]
      }
      
    }  }
  return(B)
}


assetprice <- AssetTree(1, 0.10, 52)
swing1 <- SwingTree(1, 0.10, 52)
swing2 <- SwingTreeB(1, 0.10, 52)
swing3 <- SwingTreeC(1, 0.10, 52)
swing4 <- SwingOptionP(1, 0.10, 52)

plot(assetprice, swing4, xlab = "Gas Price Per Litre", 
     ylab = "Option Value", main = "n-Swing Option Value")
lines(assetprice, swing4, col="blue",lty=2)
lines(assetprice, swing2, col="powderblue",lty=2)
lines(assetprice, swing3, col="slategray3",lty=2)
lines(assetprice, swing1, col="skyblue",lty=2)



############### 4 Down-Swing option 
DSwingTree <- function(P, alpha, N) { # 1 down swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(i + j - 2 == N) {
        B[i, j] = max(c(0,P-A[i, j]))
      } else if(i + j - 2 < N) {
        B[i, j] = A[i, j]*((B[i+1, j] - B[i, j+1])/(A[i+1, j]-A[i, j+1]))+ B[i, j+1] - (A[i, j+1]*
                                                                                          ((B[i+1, j] - B[i, j+1])/(A[i+1, j]-A[i, j+1])))
        # price * delta + Cd - Sd*delta
      } else {B[i, j] = 0}
      
    }  }
  return(B)
}

A <-DSwingTree(1,0.10,52)

DSwingTreeB <- function(P, alpha, N) { # 2 down swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] < P) {
        B[i, j] = C[i, j] + (P - A[i, j])
        
        
      } else {B[i, j] = C[i, j]
      }
      
    }  }
  return(B)
}

DSwingTreeC <- function(P, alpha, N) { # 3 down swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  D = SwingTreeB(P, alpha, N)
  
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] < P) {
        B[i, j] = D[i, j] + (P - A[i, j])
        
        
      } else {B[i, j] = D[i, j]
      }
      
    }  }
  return(B)
}

DSwingOptionP <- function(P, alpha, N) { # 4 down swing
  
  B = matrix(0, nrow=N+1, ncol=N+1)
  A = AssetTree(P, alpha, N)
  C = SwingTree(P, alpha, N)
  D = SwingTreeB(P, alpha, N)
  E = SwingTreeC(P, alpha, N)
  
  
  for (i in rev(1:(N+1))) {
    for (j in rev(1:(N+1))) {
      if(A[i, j] < P) {
        B[i, j] = E[i, j] + (P - A[i, j])
        
        
      } else {B[i, j] = E[i, j]
      }
      
    }  }
  return(B)
}

dswing1 <- DSwingTree(1, 0.10, 52)
dswing2 <- DSwingTreeB(1, 0.10, 52)
dswing3 <- DSwingTreeC(1, 0.10, 52)
dswing4 <- DSwingOptionP(1, 0.10, 52)

plot(assetprice, dswing4, xlab = "Gas Price Per Litre", 
     ylab = "Option Value", main = "n-Swing Option Value")
lines(assetprice, dswing4, col="blue",lty=2)
lines(assetprice, dswing2, col="powderblue",lty=2)
lines(assetprice, dswing3, col="slategray3",lty=2)
lines(assetprice, dswing1, col="skyblue",lty=2)






