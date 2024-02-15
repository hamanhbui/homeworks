# Ha Bui
# hbui13@jhu.edu

library(MASS)

# packages (e.g. boot) to do bootstrap, please do not use them.

# Input:
#           df: a data frame with n rows.
# Output:   a data frame with n rows obtained from the input dataframe by sampling rows with replacement.
df_resample = function(df){
    
    nrows <- dim(df)[1]
    index <- sample(1:nrows, nrows, replace = TRUE)
    df[index,]
}

# Input:
#           mat: a data matrix with n rows and k columns (rows are samples, columns are variables).
# Output:   a data frame with column (variable) names "x1" to "xk", and data from the matrix.
df_make = function(mat){
    
    df <- as.data.frame(mat)
    
    k <- dim(mat)[2]
    colnames(df) <- c(paste("x", 1:k, sep = ""))
    
    df
}


# Input:
#           df: a data frame to be resampled
#           k: number of resampled datasets to generate.
#           f: a function of df giving the statistic of interest (e.g. function(df) { mean(df$x1) })
#           q: a real number 0 < q < 0.5 giving the lower quantile of the desired confident interval.
# Output:   a three element vector giving the statistic of interest (first element), and lower and upper
#           confidence intervals around it, corresponding to q and 1-q quantiles (second and third elements).
bootstrap_ci = function(df, k, f, q){
    
    est <- f(df)
    
    mu <- rep(0, k)
    for(i in 1:k){
        df_r <- df_resample(df)

        mu[i] <- f(df_r)
    }
    mu <- sort(mu)
    
    c(est, est + quantile(est - mu, q, names = FALSE), est + quantile(est - mu, 1-q, names = FALSE),
    quantile(est - mu, 1-q, names = FALSE) - quantile(est - mu, q, names = FALSE)
    )
}


#########

# Input:
#           a set of features, and a set of weights for a logistic model.
# Output:
#           the predicted probability of the output feature obtaining the value 1.
logisprob = function(x, w){
    
    1 / (1 + exp(-(x %*% w)))
}

# Input:
#           a training dataset given as a set of feature rows, represented
#           by a n by k matrix X, and a set of corresponding
#           output predictions, represented by a n by 1 matrix y.
# Output:
#           A row vector of weights for a logistic regression model (with no intercept)
#           maximizing the likelihood of observing the data.
logisreg = function(X, y){

    w <- rep(0, dim(X)[2])
    k <- 0
    repeat {
        k <- k + 1
        w0 <- w
        p <- apply(X, 1, function(row) logisprob(row, w))
        W <- diag(p * (1 - p))
        w <- w + solve(t(X) %*% W %*% X) %*% t(X) %*% (y - p)
        if(abs(sum(w0 - w)) < 0.01){
            break;
        }
    }
    t(w)
}


nie_mixed_fitted = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]
    
    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }
    alpha <- linreg_intercept(X, y)
    beta <- linreg_intercept(cbind(a, C), y)
    gama <- logisreg(C, a)
    
    y_hat_1 = beta[1] + cbind(rep(1, length(y)), C) %*% beta[2:length(beta)]
    y_hat_m_1 = alpha[1] + cbind(rep(1, length(y)), X[, 2:11]) %*% alpha[2:length(alpha)]
    gama = as.vector(gama)
    a_hat = 1 / (1 + exp(-(C %*% gama)))

    ACE = 0
    for(i in 1:NROW(X)){
        ACE = ACE + (y_hat_1[i] - (((1 - a[i]) * y_hat_m_1[i]) / (1 - a_hat[i])))
    }

    ACE = (1/length(y)) * (ACE)

    return(ACE)
}

nie_mixed = function(df){
 
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]

    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }
    alpha <- linreg_intercept(X, y)
    beta <- linreg_intercept(cbind(a, C), y)
    
    y_hat_1 = beta[1] + cbind(rep(1, length(y)), C) %*% beta[2:length(beta)]
    y_hat_m_1 = alpha[1] + cbind(rep(1, length(y)), X[, 2:11]) %*% alpha[2:length(alpha)]
    
    a_hat_0 = 0
    for(i in 1:NROW(X)){
        if(a[i] == 0){
            a_hat_0 = a_hat_0 + 1
        }
    }
    a_hat_0 = a_hat_0/NROW(X)

    ACE = 0
    for(i in 1:NROW(X)){
        ACE = ACE + (y_hat_1[i] - (((1 - a[i]) * y_hat_m_1[i]) / a_hat_0))
    }

    ACE = (1/length(y)) * (ACE)

    return(ACE)
}

nde_mixed_fitted = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]
    
    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }
    alpha <- linreg_intercept(X, y)
    beta <- linreg_intercept(cbind(a, C), y)
    gama <- logisreg(C, a)
    
    y_hat_0 = beta[1] + cbind(rep(0, length(y)), C) %*% beta[2:length(beta)]
    y_hat_m_1 = alpha[1] + cbind(rep(1, length(y)), X[, 2:11]) %*% alpha[2:length(alpha)]
    gama = as.vector(gama)
    a_hat = 1 / (1 + exp(-(C %*% gama)))

    ACE = 0
    for(i in 1:NROW(X)){
        ACE = ACE + ((((1 - a[i]) * y_hat_m_1[i]) / (1 - a_hat[i])) - y_hat_0[i])
    }

    ACE = (1/length(y)) * (ACE)

    return(ACE)
}

nde_mixed = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]
    
    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }
    alpha <- linreg_intercept(X, y)
    beta <- linreg_intercept(cbind(a, C), y)
    gama <- t(logisreg(cbind(rep(1, length(y)), C), a))
    
    y_hat_0 = beta[1] + cbind(rep(0, length(y)), C) %*% beta[2:length(beta)]
    y_hat_m_1 = alpha[1] + cbind(rep(1, length(y)), X[, 2:11]) %*% alpha[2:length(alpha)]
    a_hat_0 = gama[1] + C %*% gama[2:length(gama)]

    a_hat_0 = 0
    for(i in 1:NROW(X)){
        if(a[i] == 0){
            a_hat_0 = a_hat_0 + 1
        }
    }
    a_hat_0 = a_hat_0/NROW(X)

    ACE = 0
    for(i in 1:NROW(X)){
        ACE = ACE + ((((1 - a[i]) * y_hat_m_1[i]) / a_hat_0) - y_hat_0[i])
    }

    ACE = (1/length(y)) * (ACE)

    return(ACE)
}

nie_g = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]

    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }

    w_md <- linreg_intercept(cbind(a, C), md)
    w_y <- linreg_intercept(X, y)
    return(w_md[2] * w_y[length(w_y)])
}

nde_g = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:11]

    # your code here
    a <- X[, 1, drop = FALSE]
    C <- X[, 2:10]
    md <- X[, 11, drop = FALSE]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }

    w_y <- linreg_intercept(X, y)
    return(w_y[2])
}

ace_iv = function(df){
    
    m <- as.matrix(df)
    # your code here
    y <- m[, c("lwage")]
    a <- m[, c("educ")]
    z <- m[, c("nearc4")]
    C <- m[, c("exper","black","smsa","south","smsa66","reg661","reg662","reg663",
    "reg664","reg665","reg666","reg667","reg668","expersq")]

    linreg_intercept = function(X, y){
        X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
        A <- solve(t(X) %*% X) %*% t(X) %*% y
        return(A)
    }

    w_a <- linreg_intercept(cbind(C, z), a)
    a_hat = w_a[1] + cbind(C, z) %*% w_a[2:length(w_a)]
    w_y <- linreg_intercept(cbind(C, a_hat), y)
    return(w_y[length(w_y)])
}

# Input:
#           a training dataset given as a set of feature rows, represented
#           by a n by k matrix X, and a set of corresponding
#           output predictions, represented by a n by 1 matrix y.
# Output:
#           A row vector of weights for a linear regression model (with an intercept)
#           maximizing the likelihood of observing the data.
linreg = function(X, y){
    
    t(solve(t(X) %*% X) %*% t(X) %*% y)
}

# Input:
#           df : a data frame representing a data set
# Output:
#           The average causal effect (ACE), assuming the data
#           was generated by a conditionally ignorable causal model, where
#           for any value a, Y(a) is independent of A conditional on C.
#           A values lie in the 1st column of the data frame, Y values lie in
#           the 12th column of the data frame, and C value lie in 2nd to
#           11th columns (inclusive) of the data frame.
#           Estimate the ACE using the parametric g-formula, with a linear
#           regression outcome model (intercept but no interaction terms).
ace_g = function(df){
    
    m <- as.matrix(df)
    y <- m[,12, drop=FALSE]
    X <- m[,1:10]
    
    X <- cbind(rep(1, dim(y)[1]), X)
    
    w <- linreg(X, y)
    
    w[2]
}


# Input:
#           none
# Output:
#           none
main = function(){

    library(foreign)

    set.seed(0)

    card <- read.dta("card.dta")
    df <- card[,c("nearc4","educ","exper","black","smsa","south","smsa66","reg661","reg662","reg663",
    "reg664","reg665","reg666","reg667","reg668","lwage","expersq")]
    k <- 100

    print(bootstrap_ci(df, k, f=function(df) ace_iv(df), q = 0.025))

    df <- read.table("Jobs-NoMiss-Cont.tab", header = TRUE)

    print(bootstrap_ci(df, k, f=function(df) ace_g(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nde_g(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nie_g(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nde_mixed(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nie_mixed(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nde_mixed_fitted(df), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) nie_mixed_fitted(df), q = 0.025))
}

main()

