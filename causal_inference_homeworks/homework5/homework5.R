# Ha Bui
# hbui13@jhu.edu

library(MASS)


# Input:
#   df: a data frame to be resampled.
#   k: number of resampled datasets to generate.
#   f: a function of df giving a vector of statistics of interest of size n.
#   q: a real number 0 < q < 0.5 giving the lower quantile of the desired
#   confidence interval.
# Output:
#   an n by four matrix where each row represents one of the parameters of
#   interest (first element), lower and upper confidence intervals around it,
#   corresponding to q and 1-q quantiles (second and third elements),
#   and the size of the confidence interval (fourth element).
bootstrap_vector_ci = function(df, k, f, q) {
    est <- f(df)
    
    mu <- rep(0, k)
    for (i in 1:k) {
        df_r <- df_resample(df)
        
        mu[i] <- f(df_r)
    }
    mu <- sort(mu)
    
    cbind(
        est,
        est + quantile(est - mu, q, names = FALSE),
        est + quantile(est - mu, 1 - q, names = FALSE),
        quantile(est - mu, 1 - q, names = FALSE) - quantile(est - mu, q, names = FALSE)
    )
}

# Input:
#           df: a data frame.
#           x2: a set of column indices of size m representing features used for
#           the stage 2 regression model.
#           y2: a column index representing the outcome used for the stage 2
#           regression model.
#           x1: a set of column indices of size l representing feature used for the
#           stage 1 regression model.
#           q_features: indices in x2 used for predicting counterfactual reward
#           for a given trajectory under different assignments to stage 2 treatment.
#           a2_features: indices in x2 corresponding to columns which involve
#           stage 2 treatment.
#	     
# Output:
#           an (m+l) length vector, representing a concatenation of parameters of
#           the stage 2 model, followed by parameters of the stage 1 model.
q_learning = function(df, x2, y2, x1, q_features, a2_features) {
    m <- as.matrix(df)
    x2 <- m[, x2 , drop = FALSE]
    y2 <- m[, y2, drop = FALSE]
    x1 <- m[, x1, drop = FALSE]
    q_features <- m[, q_features, drop = FALSE]
    alpha <- linreg(x2, y2)
    q2 = rep(0, NROW(m))
    for(i in 1:NROW(m)){
        q_features_p = q_features[i,]
        q_features_n = q_features[i,]

        if(q_features_p[a2_features[1]] == 1){
            q_features_n[a2_features] = -q_features_n[a2_features]
        }else{
            q_features_p[a2_features] = -q_features_p[a2_features]
        }

        q2_p1 = q_features_p %*% t(alpha)
        q2_n1 = q_features_n %*% t(alpha)
        
        q2[i] = max(q2_p1, q2_n1)
    }
    beta <- linreg(x1, q2)

    # out_p = cbind(1, 1, 1, 0.5, 1) %*% t(beta)
    # out_n = cbind(1, 1, -1, 0.5, -1) %*% t(beta)
    # print(out_p)
    # print(out_n)
    # quit()

    # out_p = cbind(1, 1, -1, 0.5, 1, -1, -1, -1, 1) %*% t(alpha)
    # out_n = cbind(1, 1, -1, 0.5, -1, -1, -1, 1, -1) %*% t(alpha)
    # print(out_p)
    # print(out_n)
    # quit()

    return(c(alpha, beta))
}



# Input:
#          n: the number of samples to generate
# Output:  a data frame giving n data points sampled from a variant of the model given in Chapter 3 of
#          "Statistical Methods for Dynamic Treatment Regimes."
ch3_sim_study = function(n = 500) {
    eta0 <- -0.5
    eta1 <- 0.5
    eta2 <- 1   #variant
    
    zeta0 <- -0.8
    zeta1 <- 1.25
    
    delta1 <- 0.1
    delta2 <- 0.1
    
    o1 <- sample(c(0, 1), n, 0.5)
    
    c1 <- rnorm(n, 0, 1)
    
    p <- logis(zeta0 + zeta1 * c1)
    
    a1 <- sapply(p, function(x)
        sample(c(-1, 1), 1, x))
    
    p <- logis(delta1 * o1 + delta2 * a1)
    
    o2 <- sapply(p, function(x)
        sample(c(0, 1), 1, x))
    
    c2 <- rnorm(n, eta0 + eta1 * c1 + eta2 * o2, 1) #variant
    
    p <- logis(zeta0 + zeta1 * c2)
    
    a2 <- sapply(p, function(x)
        sample(c(-1, 1), 1, x))
    
    gamma1 <- 1
    gamma5 <- 1
    
    gamma <- c(0, gamma1, 0,-0.5, 0, gamma5, 0.25, 0.5, 0.5)
    
    mu <-
        gamma[1] + gamma[2] * c1 + gamma[3] * o1 + gamma[4] * a1 + gamma[5] * o1 * a1 + gamma[6] * c2
    + gamma[7] * c2 + gamma[8] * o2 * a2 + gamma[8] * a1 * a2
    
    y <- rnorm(n, mu, 1)
    
    dat <- cbind(o1, c1, a1, o2, c2, a2, y)
    df <- df_make(dat)
    
    # add certain interaction columns
    #
    #           intercept               o1*a1            a1*a2            a2*o2
    df <-
        cbind(rep(1, dim(df)[1]), df, df[, 1] * df[, 3], df[, 3] * df[, 6], df[, 4] * df[, 6])
}

# Input:
#           df: a data frame with n rows.
# Output:   a data frame with n rows obtained from the input dataframe by sampling rows with replacement.
df_resample = function(df) {
    nrows <- dim(df)[1]
    index <- sample(1:nrows, nrows, replace = TRUE)
    df[index, ]
}

# Input:
#           mat: a data matrix with n rows and k columns (rows are samples, columns are variables).
# Output:   a data frame with column (variable) names "x1" to "xk", and data from the matrix.
df_make = function(mat) {
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
# Output:   a four element vector giving the statistic of interest (first element), and lower and upper
#           confidence intervals around it, corresponding to q and 1-q quantiles (second and third elements),
#           and the size of the confidence interval (fourth element).
bootstrap_ci = function(df, k, f, q) {
    est <- f(df)
    
    mu <- rep(0, k)
    for (i in 1:k) {
        df_r <- df_resample(df)
        
        mu[i] <- f(df_r)
    }
    mu <- sort(mu)
    
    c(
        est,
        est + quantile(est - mu, q, names = FALSE),
        est + quantile(est - mu, 1 - q, names = FALSE),
        quantile(est - mu, 1 - q, names = FALSE) - quantile(est - mu, q, names = FALSE)
    )
}

# Input:
#           a training dataset given as a set of feature rows, represented
#           by a n by k matrix X, and a set of corresponding
#           output predictions, represented by a n by 1 matrix y.
# Output:
#           A row vector of weights for a linear regression model (with an intercept)
#           maximizing the likelihood of observing the data.
linreg = function(X, y) {
    t(solve(t(X) %*% X) %*% t(X) %*% y)
}

logis = function(x) {
    1 / (1 + exp(-x))
}

main = function() {
    set.seed(0)
    k <- 100
    df <- ch3_sim_study(n = 500)
    
    x2 <- c(1, 2, 4, 3, 7, 6, 9, 10, 11)
    y2 <- 8
    x1 <- c(1, 2, 4, 3, 9)
    q_features <- c(1, 2, 4, 3, 1, 6, 9, 4, 5)
    a2_features <- c(5, 8, 9)
    
    print(bootstrap_vector_ci(df, k, function(df)
        q_learning(df, x2, y2, x1, q_features, a2_features), q = 0.025))
}

main()
