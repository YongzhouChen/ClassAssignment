#This is a lda written by yourself
#Make sure that the data is constructed by this form:
# the first column is the y/label/group, while the last q-1 features are the features
# Moreover, data should be a frame
cyz_lda <- function(data,sigma_same=FALSE)
{
    y <- data[,ncol(data)]
    X <- data[,-ncol(data)]
    S <- cov(X)
    group_num <- length(unique(y))
    total_mean <- colMeans(X)
    B <- matrix(0,nrow=ncol(X),ncol=ncol(X))
    W <- matrix(0,nrow=ncol(X),ncol=ncol(X))
    Mean_vectors <- list()
    Cov_matrixes <- list()
    for(i in 1:group_num)
    {
        this_label <- as.matrix(unique(y))[i]
        this_group <- X[data[,ncol(data)]==this_label,]
        this_group_num <- nrow(this_group)
        group_mean <- colMeans(this_group)
        group_cov <- cov(this_group)
        Mean_vectors[[i]] <- group_mean
        Cov_matrixes[[i]] <- group_cov
        B <- B + (group_mean-total_mean) %*% t(group_mean-total_mean)
        W <- W + (this_group_num - 1) * group_cov
    }
    result <- list(B,W)
    return(result)
}




