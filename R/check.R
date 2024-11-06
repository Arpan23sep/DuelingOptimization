X = matrix(c(1,1,0,0,0,0,1,1), ncol = 2, byrow = F)
c<-c(3,-1)
svd(X)$d
d = X %*% solve(t(X) %*% X, c)

a = c(2,1,-1,0)
a_hat = X %*% solve(t(X) %*% X, t(X) %*% a)
t(d) %*% d
t(a) %*% a

c_1 <- c(1,-1)
Y <- c(6,-1,3,8)
t_num <- c %*% solve(t(X) %*% X, t(X) %*% Y)
P_x <- X %*% solve(t(X) %*% X, t(X))
I <- diag(x=c(1,1,1,1), nrow = 4, ncol = 4)
t_denom <- (t(Y) %*% (I - P_x) %*% Y ) / 2
t_statistic <- t_num/t_denom


#Problem 2
X <- matrix(c(1,1,0,1,
              1,1,0,2,
              1,1,0,-2,
              1,1,0,4,
              1,0,1,-1,
              1,0,1,3,
              1,0,1,0,
              1,0,1,3),ncol = 4, byrow = T)
svd(t(X))$d

zapsmall(svd(cbind(t(X), c(1,0,0,0)))$d)
zapsmall(svd(cbind(t(X), c(0,1,0,0)))$d)
zapsmall(svd(cbind(t(X), c(0,0,1,0)))$d)
zapsmall(svd(cbind(t(X), c(0,0,0,1)))$d)
zapsmall(svd(cbind(t(X), c(0,1,-1,0)))$d)


