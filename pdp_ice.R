#### function ####
pdp <- function(X_train, model, pred_var, grid_ratio=0.1, frac_to_build=0.1, ice=FALSE){
  library(ggplot2)
  library(dplyr)
  set.seed(42)
  X_train <- X_train[sample(nrow(X_train), nrow(X_train)*frac_to_build), ]
  
  n <- nrow(X_train)
  num_grid <- n*grid_ratio
  col_idx <- which(colnames(X_train)==pred_var)
  
  y <- matrix(0, nrow=n, ncol=num_grid)
  x <- matrix(0, nrow=n, ncol=num_grid)
  grid <- seq(min(X_train[,col_idx]), max(X_train[,col_idx]), length.out=num_grid)
  
  for (i in 1:n){
    X_rep <- do.call("rbind", replicate(num_grid, X_train[i, ], simplify = F))
    X_rep[, col_idx] <- grid
    pred <- predict(fit_xgb, X_rep)
    y[i, ] <- pred
    x[i, ] <- grid
  }
  
  if (ice==FALSE){
    #### pdp ####
    pd <- apply(y, 2, mean)
    pd_df <- data.frame(x=grid, y=pd)
    ggplot(pd_df, aes(x=x, y=y)) + 
      geom_line(size=0.7) + 
      ylim(0, 1) + xlab(pred_var) + ylab("yhat") + theme_bw()
  } else if (ice==TRUE){
    #### ice plot ####
    pd <- apply(y, 2, mean)
    pd_df <- data.frame(x=grid, y=pd)
    x_df <- data.frame(t(x)) %>% gather(value="x")
    y_df <- data.frame(t(y)) %>%  gather(value="y")
    ice_df <- cbind(x_df, y=y_df$y)
    ggplot() + 
      geom_line(aes(x=x, y=y, group=key), color="black", alpha=0.3, data=ice_df) + 
      geom_line(aes(x=x, y=y), color="red", size=1, data=pd_df) +
      ylim(0, 1) + xlab(pred_var) + ylab("yhat") + theme_bw()
  }
}


#### example ####
library(xgboost)
data("GermanCredit", package = "caret")
X_train <- data.matrix(GermanCredit[,-which(names(GermanCredit)=="Class")])
dtrain <- xgb.DMatrix(X_train, label = ifelse(GermanCredit$Class=="Bad", 1, 0))
param <- list(objective="binary:logistic", eta=0.1, gamma=1, seed=42)
fit_xgb <- xgboost(data=dtrain, verbose=0, params = param, nrounds = 100)

pdp(X_train=X_train, model=fit_xgb, pred_var="Amount")
pdp(X_train=X_train, model=fit_xgb, pred_var="Amount", ice=T)
