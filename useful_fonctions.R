# compute the real cover for a list of stock and a data.frame of forecasts
# s : a (nx1) stock element. Each row is a stock for a new item
# p : a (nxp) forecasts element. Each column is a new month of forecast
#     columns are ordered by chronological proximity with today
#     [!] forecasts have to be >0

calc.couv2 <- function(s,p){
  
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
    return(a+calc.couv2(s-p[,1], p[,-1]))
  }
}

s  <- data.frame(stock=c(36,28, 0, 6, 29))
p <- data.frame(
  prev_M1=c(18, 19, 6, 1, 8),
  prev_M2=c(15, 16, 8, 1, 4),
  prev_M3=c(14, 12, 12, 1, 6),
  prev_M4=c(4, 0, 59, 6, 2),
  prev_M5=c(1, 1, 78, 8, 2))

p[p<=0] <- 0.001
calc.couv2(s, p)