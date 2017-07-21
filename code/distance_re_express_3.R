# Stage 1

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,3,2))
cats <- sort(unique(cats_vec))

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x %*% matrix(1,ncol=6) - t(x %*% matrix(1,ncol=6))
test_y <- y %*% matrix(1,ncol=6) - t(y %*% matrix(1,ncol=6))
test <- test_x^2 + test_y^2

dists
test


# Stage 2

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,3,2))
cats <- sort(unique(cats_vec))

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x %*% matrix(1,ncol=6) - t(x %*% matrix(1,ncol=6))
test_y <- y %*% matrix(1,ncol=6) - t(y %*% matrix(1,ncol=6))
test <- test_x^2 + test_y^2

dists %*% rep(1,6)
test %*% rep(1,6)


# Stage 3

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,3,2))
cats <- sort(unique(cats_vec))

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x %*% matrix(1,ncol=6) - t(matrix(1,ncol=6)) %*% t(x)
test_y <- y %*% matrix(1,ncol=6) - t(matrix(1,ncol=6)) %*% t(y)
test <- test_x^2 + test_y^2

dists %*% rep(1,6)
test %*% rep(1,6)


# Stage 4

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,3,2))
cats <- sort(unique(cats_vec))
n_cats <- length(cats)

cats_matrix <- apply(cats_vec,1,function(e) e == cats)
cats_matrix <- as.matrix(cats_matrix)

if(n_cats > 1){
  cats_matrix <- t(cats_matrix)
}

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x %*% matrix(1,ncol=6) - t(matrix(1,ncol=6)) %*% t(x)
test_y <- y %*% matrix(1,ncol=6) - t(matrix(1,ncol=6)) %*% t(y)
test <- test_x^2  %*% cats_matrix + test_y^2  %*% cats_matrix

dists %*% cats_matrix
test


# Stage 5

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,3,2))
cats <- sort(unique(cats_vec))

cats_matrix <- apply(cats_vec,1,function(e) e == cats)
cats_matrix <- as.matrix(cats_matrix)

if(n_cats > 1){
  cats_matrix <- t(cats_matrix)
}

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x^2 %*% matrix(1,ncol=6) -
  (2 * x) %*% t(x) +
  matrix(1,nrow=6) %*% t(x^2)

test_y <- y^2 %*% matrix(1,ncol=6) -
  (2 * y) %*% t(y) +
  matrix(1,nrow=6) %*% t(y^2)

test <- test_x + test_y

dists
test



# Stage 6

mat <- matrix(c(1,2,8,3,6,5,9,4,11,5,7,7),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

cats_vec <- matrix(c(1,2,3,1,4,2))
cats <- sort(unique(cats_vec))
n_cats <- length(cats)

cats_matrix <- apply(cats_vec,1,function(e) e == cats)
cats_matrix <- as.matrix(cats_matrix)

if(n_cats > 1){
  cats_matrix <- t(cats_matrix)
}

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x^2 %*% matrix(1,ncol=6) %*% cats_matrix -
  (2 * x) %*% t(x) %*% cats_matrix +
  matrix(1,nrow=6) %*% t(x^2) %*% cats_matrix

test_y <- y^2 %*% matrix(1,ncol=6) %*% cats_matrix -
  (2 * y) %*% t(y) %*% cats_matrix +
  matrix(1,nrow=6) %*% t(y^2) %*% cats_matrix

test <- test_x + test_y

# obtain the sum of squared distances
dists  %*% cats_matrix
test


# Stage 7
# For each observation, count the number of neighbours
# which do not belong to the same group it is in.

anti_iden <- 1 - diag(6)
count_mat <- anti_iden %*% cats_matrix    # count_mat can actually be computed quite early


# Compute the average squared distance
# and apply the exponential function to adjust

stdev_x_mat <- sqrt(test_x / count_mat)
stdev_y_mat <- sqrt(test_y / count_mat)
exp_adj <- exp(-(stdev_x_mat + stdev_y_mat))
exp_adj <- ifelse(is.nan(exp_adj),0,exp_adj)

# Multiply by category count again to obtain adjusted count
# to obtain the neighbourhood characteristic matrix

nc_matrix <- count_mat * exp_adj

# Scale each row to get percentages.

percentages <- t(apply(nc_matrix,1,function(e) e / sum(e)))
percentages

sum(percentages[4,])
