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


# Stage 2

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


# Stage 2

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
