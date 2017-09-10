# Compute the real cover for a list of stock and forecasts on each row
# s : a (nx1) stock element. Each row is a stock for a new item
# p : a (nxp) forecasts element. Each column is a new month of forecast
#     columns are ordered by chronological proximity with today
#     [!] forecasts have to be >0

calc.couv <- function(s,p){
  
  # data processing
  if(!is.matrix(s)) s <- as.matrix(s)
  if(!is.matrix(p)) p <- as.matrix(p)
  
  
  # function corpus
  i1 <- p[,1]<s & s>=0
  i2 <- p[,1]>s & s>=0
  if(sum(i1|i2)==0){
    return(0)
  } else if(dim(p)[2]==1) {
    return(s/p[,1] *i1)
  } else {
    a <- 1*i1+s/p[,1]*i2
    return(a+calc.couv(s-p[,1], p[,-1]))
  }
}


# Return the first appearance of a none-zero value of each row
# a           : a (nxp) data.frame
# lefttoright : direction of search

first.no.null <- function(a, lefttoright=TRUE){
  # 'a' needs to be a matrix (with row and column indexes)
  if(!is.matrix(a)) a <- as.matrix(a)
  n <- dim(a)[1]
  p <- dim(a)[2]
  
  # the column we will not considerate at the next iteration
  if(lefttoright) j <- 1 else j <- p
  
  # kernel computing
  i <- a[,j]==0
  
  # at the last column, none-zero will be considered as NA
  if(p==1){
    return(replace(1*i, !i, NA))
  } else {
    # next iteration only for rows which have not already a zeros
    return(
      rep(1, n)+replace(rep(0, n),!i,first.no.null( a[!i,-j], lefttoright=lefttoright))
    )
  }
}

# ----------------------
# Examples
# ----------------------

s  <- data.frame(stock=c(36,28, 0, 6, 29, 2))
p <- data.frame(
  prev_M1=c(18, 19, 6, 1, 0, 8),
  prev_M2=c(15, 16, 8, 1, 3,  0),
  prev_M3=c(14, 12, 12, 1, 0, 6),
  prev_M4=c(4, 0, 59, 6, 2, 4),
  prev_M5=c(2, 0, 78, 8, 1, 4))

first.no.null(p)
p[p<=0] <- 0.001
calc.couv2(s, p)
first.no.null(p)