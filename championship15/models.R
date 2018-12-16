debugSource('lgb.R')

my.train.lm = function (XL, params) {
  model = glm(Y ~ ., data=XL,
              family=binomial(link='logit'))
  
  function (X) {
    X = as.data.frame(X)
    r2=predict(model, type="response", newdata=X)
    as.vector(r2)
  }
}

my.train.lmr = function (XL, params) {
  nvars = ncol(XL) - 1
  model = glmnet(as.matrix(XL[,-ncol(XL)]), as.double(XL$Y), family='binomial', alpha=0, standardize=T,
                 nlambda=params$nlambda, lambda.min.ratio=params$lambda.min.ratio, thresh=params$thresh,
                 maxit=100000)
  
  function (X) {
    X = as.matrix(X)
    as.double(predict(model, X, type="response", s=params$s))
  }
}
lmrParams = list(
  s=c(1e-6),
  nlambda=c(2),
  lambda.min.ratio=c(1e-6),
  thresh=c(1e-7)
)


my.train.lm2 = function (XL, params) {
  model = lm(Y~., as.data.frame(XL))
  
  function (X) {
    predict(model, as.data.frame(X))
  }
}

