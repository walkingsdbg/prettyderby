#visualize 2nd error
rm(list=ls())
z_alpha <- 1.282 #標準正規分布の上側alpha%点(alphaは第１種の過誤の確率)
p_0     <- 0.05
nlist <- c(10,100,1000)
for(n in nlist){
  A <- function(x) {sqrt(p_0*(1-p_0)/(x*(1-x)))}
  B <- function(x) {(x-p_0)/sqrt(x*(1-x)/n)}
  p_2nderr <- function(x) {pnorm(z_alpha*A(x)-B(x))}
  if(n==10){
    plot(p_2nderr,0,1,ylim=c(0,1),ann=F,lwd=2,lty=1)
    par(new=T)
  }
  if(n==100){
    plot(p_2nderr,0,1,ylim=c(0,1),ann=F,lwd=2,lty=2)
    par(new=T)
  }
  if(n==1000){
    plot(p_2nderr,0,1,ylim=c(0,1),lwd=2,lty=3,xlab="p")
  }
}

#calculate sample size
rm(list=ls())
z_alpha <- 1.282 #標準正規分布の上側alpha%点(alphaは第１種の過誤の確率)
z_beta  <- 1.282 #標準正規分布の上側beta%点（betaは第２種の過誤の確率）
p_0     <- 0.05
delta_0 <- 0.05
p_thr <- p_0 + delta_0
A <- function(x) {sqrt(p_0*(1-p_0)/(x*(1-x)))}
n <- p_thr*(1-p_thr)/((p_thr-p_0)^2)*(z_alpha*A(p_thr)+z_beta)^2 #サンプルサイズ

#calculate test statistic
rm(list=ls())
d <- read.csv("training_supercreek.csv",encoding="UTF-8")
n <- nrow(d)
n_fail <- sum(d$flag_failure)
p_hat  <- n_fail/n
p_0 <- 0.05
u_0 <-(p_hat-p_0)/sqrt(p_0*(1-p_0)/n)

