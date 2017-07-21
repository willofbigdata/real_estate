# Stage 1

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x %*% matrix(1,ncol=4) - t(x %*% matrix(1,ncol=4))
test_y <- y %*% matrix(1,ncol=4) - t(y %*% matrix(1,ncol=4))
test <- test_x^2 + test_y^2

dists
test


# Stage 2

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- x^2 %*% matrix(1,ncol=4) -
          2 * (x %*% t(x)) +
          t(x^2 %*% matrix(1,ncol=4))

test_y <- y^2 %*% matrix(1,ncol=4) -
          2 * (y %*% t(y)) +
          t(y^2 %*% matrix(1,ncol=4))

test <- exp(-test_x) * exp(-test_y)

solution
test


# Stage 3

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-(x^2 %*% matrix(1,ncol=4) -
  2 * (x %*% t(x)) +
  t(x^2 %*% matrix(1,ncol=4))))

test_y <- exp(-(y^2 %*% matrix(1,ncol=4) -
  2 * (y %*% t(y)) +
  t(y^2 %*% matrix(1,ncol=4))))

test <- test_x * test_y

solution
test


# Stage 4

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-x^2 %*% matrix(1,ncol=4) +
                  2 * (x %*% t(x)) -
                  t(x^2 %*% matrix(1,ncol=4)))

test_y <- exp(-y^2 %*% matrix(1,ncol=4) +
                  2 * (y %*% t(y)) -
                  t(y^2 %*% matrix(1,ncol=4)))

test <- test_x * test_y

solution
test



# Stage 5

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-x^2 %*% matrix(1,ncol=4)) *
          exp(2 * (x %*% t(x))) *
          exp(-t(x^2 %*% matrix(1,ncol=4)))

test_y <- exp(-y^2 %*% matrix(1,ncol=4)) *
          exp(2 * (y %*% t(y))) *
          exp(-t(y^2 %*% matrix(1,ncol=4)))

test <- test_x * test_y

solution
test




# Stage 6

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-x^2) %*% matrix(1,ncol=4) *
  (exp(x %*% t(x)))^2 *
  exp(-t(x^2 %*% matrix(1,ncol=4)))

test_y <- exp(-y^2) %*% matrix(1,ncol=4) *
  (exp(y %*% t(y)))^2 *
  exp(-t(y^2 %*% matrix(1,ncol=4)))

test <- test_x * test_y

solution
test



# Stage 7

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-x^2) %*% matrix(1,ncol=4) *
  (exp(x %*% t(x)))^2 *
  exp(-t(x^2 %*% matrix(1,ncol=4)))

test_y <- exp(-y^2) %*% matrix(1,ncol=4) *
  (exp(y %*% t(y)))^2 *
  exp(-t(y^2 %*% matrix(1,ncol=4)))

test <- test_x * test_y

solution
test


# Stage 7

mat <- matrix(c(1,2,8,3,6,5,9,4),ncol=2)
dists <- dist(mat)
dists <- as.matrix(dists)
dists <- dists^2
solution <- exp(-dists)

x <- as.matrix(mat[,1])
y <- as.matrix(mat[,2])

test_x <- exp(-x^2) %*% matrix(1,ncol=4) *
  (exp(x) %*% t(x))^2 *
  exp(-t(x^2 %*% matrix(1,ncol=4)))

test_y <- exp(-y^2) %*% matrix(1,ncol=4) *
  (exp(y %*% t(y)))^2 *
  exp(-t(y^2 %*% matrix(1,ncol=4)))

test <- test_x * test_y

solution
test