# Ha Bui
# hbui13@jhu.edu

library(MASS)

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

# mean_1 <- function(df) { mean(df$x1) }
# > quantile(1:10, 0.25, names = FALSE)

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
# Input:
#          n: the number of samples to generate
# Output:  a data frame giving n data points sampled from a variant of the model given in Chapter 3 of
#          "Statistical Methods for Dynamic Treatment Regime."
ch3_sim_study = function(n = 500){
    
    eta0 <- -0.5
    eta1 <- 0.5
    eta2 <- 1   #variant

    zeta0 <- -0.8
    zeta1 <- 1.25
    
    delta1 <- 0.1
    delta2 <- 0.1

    o1 <- sample(c(0,1), n, 0.5)

    c1 <- rnorm(n, 0, 1)
    
    p <- logis(zeta0 + zeta1 * c1)

    a1 <- sapply(p, function(x) sample(c(0,1), 1, prob=c(1-x,x)))

    p <- logis(delta1 * o1 + delta2 * a1)

    o2 <- sapply(p, function(x) sample(c(0,1), 1, prob=c(1-x,x)))

    c2 <- rnorm(n, eta0 + eta1 * c1 + eta2 * o2, 1) #variant

    p <- logis(zeta0 + zeta1 * c2)

    a2 <- sapply(p, function(x) sample(c(0,1), 1, prob=c(1-x,x)))

    gamma1 <- 1
    gamma5 <- 1
    
    gamma <- c(0, gamma1, 0, -0.5, 0, gamma5, 0.25, 0.5, 0.5)
    
    mu <- gamma[1] + gamma[2] * c1 + gamma[3] * o1 + gamma[4] * a1 + gamma[5] * o1 * a1 + gamma[6] * c2 + gamma[7] * c2 + gamma[8] * o2 * a2 + gamma[8] * a1 * a2
        
    y <- rnorm(n, mu, 1)
    
    dat <- cbind(o1, c1, a1, o2, c2, a2, y)
    df_make(dat)
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
    repeat {
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

# Input:
#           a set of features, and a set of weights for a logistic model.
# Output:
#           the predicted probability of the output feature obtaining the value 1.
logisprob = function(x, w){
    
    logis(x %*% w)
}

logis = function(x){
    
    1 / (1 + exp(-x))
}


### ### ###

# Input:
#           df: a data frame, representing the simulation study dataset.
#           a_test: a size 2 vector containing 0 or 1 values representing treatment values
#           in the test arm.
#           a_control: a size 2 vector containing 0 or 1 values representing treatment values
#           in the control arm.
#           k: the number of trajectories to draw for each data row.
# Output:
#           An estimate of Y(a) - Y(a'), where values of a are given by a_test, and values of a'
#           are given by a_control.
#
#           This function follows the description in the homework.
seq_g = function(df, a_test, a_control, k = 20){
    
    # represent dataset as a matrix data type.
    m <- as.matrix(df)

    o1 <- m[,1, drop=FALSE]
    c1 <- m[,2, drop=FALSE]
    a1 <- m[,3, drop=FALSE]
    o2 <- m[,4, drop=FALSE]
    c2 <- m[,5, drop=FALSE]
    a2 <- m[,6, drop=FALSE]
    y <- m[,7, drop=FALSE]
    alpha <- logisreg(cbind(o1, a1, c1), o2)
    alpha = as.vector(alpha)
    X <- cbind(o1, a1, c1, o2, a2, c2)
    X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
    beta <- linreg(X, y)

    y_diff = 0
    for(i in 1:NROW(m)){
        y_hat_control = 0
        y_hat_test = 0
        for(j in 1:k){
            o2_hat = 1 / (1 + exp(-(cbind(o1[i], a_control[1], c1[i]) %*% alpha)))
            y_hat_control = y_hat_control + beta[1] + cbind(o1[i], a_control[1], c1[i], o2_hat, a_control[2], c2[i]) %*% beta[2:length(beta)]
        }
        for(j in 1:k){
            o2_hat = 1 / (1 + exp(-(cbind(o1[i], a_test[1], c1[i]) %*% alpha)))
            y_hat_test = y_hat_test + beta[1] + cbind(o1[i], a_test[1], c1[i], o2_hat, a_test[2], c2[i]) %*% beta[2:length(beta)]
        }
        # y_diff = y_diff + ((y_hat_test - y_hat_control)/k)^2
        y_diff = y_diff + ((y_hat_test - y_hat_control)/k)
    }
    # y_diff = y_diff/(NROW(m)-7)
    y_diff = y_diff/(NROW(m))
    return(y_diff)
}



# Input:
#           df: a data frame, representing the simulation study dataset.
#           coef: gives the coefficient of the marginal structural model to return (valid values 2 or 3).
# Output:
#           A coefficient of the linear marginal structural model corresponding to a causal effect of
#           either A1 on Y (for coef=2) or A2 on Y (for coef=3).
seq_ipw = function(df, coef = 3){
    
    # represent dataset as a matrix data type.
    m <- as.matrix(df)

    o1 <- m[,1, drop=FALSE]
    c1 <- m[,2, drop=FALSE]
    a1 <- m[,3, drop=FALSE]
    o2 <- m[,4, drop=FALSE]
    c2 <- m[,5, drop=FALSE]
    a2 <- m[,6, drop=FALSE]
    y <- m[,7, drop=FALSE]
    gamma <- logisreg(cbind(c1, o1), a1)
    gamma = as.vector(gamma)
    delta <- logisreg(cbind(c1, o1, a1, c2, o2), a2)
    delta = as.vector(delta)

    v <- c()
    for(i in 1:NROW(m)){
        a1_hat = 1 / (1 + exp(-(cbind(c1[i], o1[i]) %*% gamma)))
        a2_hat = 1 / (1 + exp(-(cbind(c1[i], o1[i], a1[i], c2[i], o2[i]) %*% delta)))
        v <- append(v, 1/(a1_hat * a2_hat))
    }
    # X <- cbind(o1, a1, c1, o2, a2, c2)
    X <- cbind(a1, a2)
    X <- cbind(matrix(c(1), nrow=dim(X)[1], ncol=1), X)
    out <- linreg_weighted(X, y, v)
    return(out[coef])
    #Did not correctly handle the probabilities in the weights (need to make sure you pick the A=0 or A=1 probability depending on the data row)
}

#########

# Input:
#           a training dataset given as a set of feature rows, represented
#           by a n by k matrix X, and a set of corresponding
#           output predictions, represented by a n by 1 matrix y.
#           a weight vector for each row.
# Output:
#           A row vector of weights for a linear regression model (with an intercept)
#           maximizing the likelihood of observing the data.
linreg_weighted = function(X, y, w){
    W = matrix(diag(1/w), ncol = dim(X)[1])
    t(solve(t(X) %*% W %*% X, t(X) %*% W %*% y))
}

linreg = function(X, y){
    
    t(solve(t(X) %*% X) %*% t(X) %*% y)
}

###### DO NOT EDIT PAST THIS POINT ######


main = function(){
    
    set.seed(0)
    k <- 100
    df <- ch3_sim_study(n = 500)

    print(bootstrap_ci(df, k, f=function(df) seq_g(df, c(0, 1), c(0, 0), k = 20), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) seq_g(df, c(1, 1), c(1, 0), k = 20), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) seq_ipw(df, coef=3), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) seq_g(df, c(1, 0), c(0, 0), k = 20), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) seq_g(df, c(1, 1), c(0, 1), k = 20), q = 0.025))
    print(bootstrap_ci(df, k, f=function(df) seq_ipw(df, coef=2), q = 0.025))
}

main()

