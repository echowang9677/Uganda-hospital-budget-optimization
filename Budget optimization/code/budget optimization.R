install.packages("readxl")
install.packages("mice")
install.packages("proto")
install.packages("ggplot2")
install.packages("GGallay")
install.packages("Rglpk")
library(readxl)
library(mice)
library(proto)
library(ggplot2)
library(GGally)
library(Rglpk)

setwd("W:/www/Budget optimization and risk analysis")

#read data
data <- read.csv(file="budget.csv",header=TRUE, sep=",")
data$VEN <- as.character(data$VEN)
data$VEN[data$VEN == "V"] <- 0.5
data$VEN[data$VEN == "E"] <- 0.3
data$VEN[data$VEN == "N"] <- 0.2
data$VEN[data$VEN == ""] <- 0.3
data$VEN <- as.numeric(data$VEN)
test <- data[6:9]
sapply(data,class)
md.pattern(test)
tempdata <- mice(test,m=5,maxit=50,meth='pmm',seed=500)
summary(tempdata)
complete <- complete(tempdata,1)
data[6:9] <- complete
#formula <- CONSUMPTION.EST.QTY.~.
#lm <- lm(formula, complete)
#summary(lm)
#complete$CONSUMPTION.EST.QTY. <- -0.466*complete$FY17.18.PLAN.QTY+0.062*complete$MORBIDITY..EST..QTY+2.538*complete$NATIONAL.AVERAGE-2.258

data$min <- apply(data[c(6:10)],1,min)
data$max <- apply(data[c(6:10)],1,max)

mat <- matrix(ncol=289, nrow=289)
mat <- diag(289)
f.obj <- c(data$VEN * data$UNIT)
f.con <- matrix(as.vector(rbind(matrix(c(data$PRICE),nrow=1),mat,mat)),nrow=579)
f.dir <- c(rep("<=",290),rep(">=",289))
f.rhs <- c(12488671,data$max,data$min)
max= TRUE
solution <- Rglpk_solve_LP(f.obj,f.con,f.dir,f.rhs,max=max)$solution
solution <- as.integer(solution)
data$solution <- solution
data$cost <- data$PRICE * data$solution
sum <- sum(data$cost)
sum
result <- data.frame(data[c(1,2,3,5,13,14)])
colnames(result) <- c("CODE","DESCRIPTION","UNIT","PRICE","QUANTITY","COST")
write.csv(result,"budget_optim.csv")
