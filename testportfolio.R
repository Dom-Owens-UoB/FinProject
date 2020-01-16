data <- read.csv("data/combined_close_data.csv")
train <- retrieve.close(data,"2016-06-01","2017-06-01")
test <- retrieve.close(data,"2017-06-01","2018-06-01")
test <- dplyr::select(test,colnames(train))
train.returns <- returns(train,2)

model <- fit.factor.model(train.returns,10)
covars <- approx.covar(train.returns,model)
portfolio <- portfolio.solution(train.returns,model,covars,0.1)

test.returns <- returns(test,2)
exp.return <- t(portfolio) %*% rowMeans(test.returns)
port.val <- t(portfolio) %*% test.returns
port.val <- port.val * 1/port.val[1]
plot(1:length(port.val),port.val,type="l")

t(portfolio) %*% rowMeans(test.returns)

portfolio3 <- rep(1/length(portfolio),length(portfolio))
exp.return3 <- t(portfolio3) %*% rowMeans(test.returns)
port.val3 <- t(portfolio3) %*% t(test[,-1])
port.val3 <- port.val3 * 1/port.val3[1]
lines(1:length(port.val3),port.val3,type="l",col = "blue")

  #Asses model fit.
test.error <- test.returns - rowMeans(test.returns)%*%t(rep(1,249)) - t(model@loadings) %*% model@scores
sig0 <- train.returns - rowMeans(train.returns)
sig0 <- sqrt(apply(sig0,1,var))
sig1 <- t(model@loadings) %*% model@scores - covars$error
sig1 <- sqrt(apply(sig1,1,var))
