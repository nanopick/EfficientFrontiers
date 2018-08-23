#install.packages("quadprog")
#install.packages("ggplot2")

rm(list=ls())

# library(stockPortfolio)
library(quadprog)
library(ggplot2)
library(Matrix)

setwd("PUT YOUR DIRECTORY HERE")

# Color Scheme
RED  <- "#7D110C"
TAN  <- "#CDC4B6"
LIGHT_TAN <- "#F7F6F0"
BROWN  <- "#423C30"

TITLE <- "Efficient Frontier\nand Optimal Portfolio"
X_AXIS_TITLE <- "Risk (standard deviation of portfolio variance)"
Y_AXIS_TITLE <- "Return"

REIT_ALLOCATION_RANGE = c(0.00000, 0.20000)
Preciousmetals_ALLOCATION_RANGE = c(0.0000, 0.12000)
SP_ALLOCATION_RANGE = c(0.25000, 0.40000)
IEQUITY_ALLOCATION_RANGE = c(0.08000, 0.20000)
tenyrbond_ALLOCATION_RANGE = c(0.0000, 0.20000)
CBOND_ALLOCATION_RANGE = c(0.00000, 0.20000)
Commodities_ALLOCATION_RANGE=c(0.02000, 0.0700)

CSV_COLUMN_NAMES <- c('Time','REIT','Preciousmetals','SP500','IEQUITY','10yrbond','CBOND', 'Commodities')

longData <- read.csv('Returns.csv',header = T)

colnames(longData) <- CSV_COLUMN_NAMES
returns <- longData[,-1] # Removes the first column (year)

#place where return expectations are set

expectedReturns<-c(.05500,.06500,.11170,.14900,.05500,.04400,.01164)

efficient_frontier <-
  function (returns, short="no", max.allocation=NULL, risk.premium.up=.50000, risk.increment=.005) {
    #This part of code accounds for missing Commodities data

    covariance <- cov(returns, use="pairwise.complete.obs")
    covariance<-nearPD(covariance)
    covariance<-covariance$mat

    n <- ncol(covariance)
    Amat <- matrix(1, nrow=n)
      
    bvec = 1
    meq = 1

    if (short=="no") {
      Amat <- cbind(1, diag(n))
      bvec <- c(bvec, rep(0, n))
    }

    # And modify Amat and bvec if a max allocation (concentration) is specified
    if (!is.null(max.allocation)) {
      if (max.allocation > 1 | max.allocation < 0) {
        stop("max.allocation must be greater than 0 and less than 1")
      }

      if (max.allocation * n < 1) {
        stop("Need to set max.allocation higher; not enough assets to add to 1")
      }

      Amat <- cbind(Amat, -diag(n))

      #bvec <- c(bvec, rep(-max.allocation, n))
      bvec <-  c(1.0, #keep
                 REIT_ALLOCATION_RANGE[1],
                 Preciousmetals_ALLOCATION_RANGE[1], #column 1
                 SP_ALLOCATION_RANGE[1], #column 2
                 IEQUITY_ALLOCATION_RANGE[1], #column 3
                 tenyrbond_ALLOCATION_RANGE[1], #column 4
                 CBOND_ALLOCATION_RANGE[1],
                 Commodities_ALLOCATION_RANGE[1],#column 5
                 -REIT_ALLOCATION_RANGE[2],
                 -Preciousmetals_ALLOCATION_RANGE[2], #column 1
                 -SP_ALLOCATION_RANGE[2], #column 2
                 -IEQUITY_ALLOCATION_RANGE[2], #column 3
                 -tenyrbond_ALLOCATION_RANGE[2], #column 4
                 -CBOND_ALLOCATION_RANGE[2],
                 -Commodities_ALLOCATION_RANGE[2]) #column 7
    }

    # Calculate the number of loops based on how high to vary the risk premium and by what increment
    loops <- risk.premium.up / risk.increment + 1
    loop <- 1
    eff <- matrix(nrow=loops, ncol=n+3)

    # Now I need to give the matrix column names
    colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")

    # Loop through the quadratic program solver
    for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
      dvec <- expectedReturns*i #This moves the solution up along the efficient frontier, can change this to be the expected 
returns from the historical
      sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
      eff[loop, "Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
      eff[loop, "Exp.Return"] <- as.numeric(sol$solution %*% expectedReturns)
      eff[loop, "sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
      eff[loop, 1:n] <- sol$solution
      loop <- loop + 1
    }

    return(as.data.frame(eff))
  }

eff <- efficient_frontier(returns, max.allocation=TRUE, short='no', risk.premium.up = 5, risk.increment = .005) 
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
risk <- round(eff.optimal.point$Std.Dev, digits=4)

return <- round(eff.optimal.point$Exp.Return, digits=4)

sharpe <- round(eff.optimal.point$sharpe, digits=4)

ealred  <- "#7D110C"
ealtan  <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark  <- "#423C30"

ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe), color=ealred, size=5) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev, y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ", round(eff.optimal.point$Std.Dev*100, digits=2),"%\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=2),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2),  sep=""), hjust=0, vjust=0) +
  ggtitle("Efficient Frontier\nand Optimal Portfolio") + labs(x="Risk (standard deviation of portfolio variance)", y="Return") +

  theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))

print(eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),])
