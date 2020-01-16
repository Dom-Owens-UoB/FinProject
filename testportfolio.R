data <- read.csv("data/combined_close_data.csv")
train <- retrieve.close(data,"2015-01-01","2017-01-01")
test <- retrieve.close(data,"2017-01-01","2018-01-01")
test <- dplyr::select(test,colnames(train))
train.returns <- returns(train,2)

model <- fit.factor.model(train.returns,10)
covars <- approx.covar(train.returns,model)
portfolio <- portfolio.solution(train.returns,model,covars,0.01)

test.returns <- returns(test,2)
exp.return <- t(portfolio) %*% rowMeans(test.returns)
port.val <- t(portfolio) %*% t(test[,-1])
port.val <- port.val * 1/port.val[1]
plot(1:251,port.val,type="l",ylim = c(1,2.5))

portfolio2 <- optim.portfolio(train.returns,covars$covar)
portfolio2 <- portfolio2$solution
exp.return2 <- t(portfolio2) %*% rowMeans(test.returns)
port.val2 <- t(portfolio2) %*% t(test[,-1])
port.val2 <- port.val2 * 1/port.val2[1]
lines(1:251,port.val2,type="l", col = "red")


portfolio3 <- rep(1/411,411)
exp.return3 <- t(portfolio3) %*% rowMeans(test.returns)
port.val3 <- t(portfolio3) %*% t(test[,-1])
port.val3 <- port.val3 * 1/port.val3[1]
lines(1:251,port.val3,type="l",col = "blue")


