# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Sample run:
## a = rbind(c(10, 2), c(2, 10))
## b = makeCacheMatrix(a)
## b$get()
##      [,1] [,2]
## [1,]   10    2
## [2,]    2   10

## No cache in the first run
## cacheSolve(b)
##      [,1]        [,2]
## [1,]  0.10416667 -0.02083333
## [2,] -0.02083333  0.10416667

## Retrieving from the cache in the second run
## > cacheSolve(b)
## getting cached data
##      [,1]        [,2]
## [1,]  0.10416667 -0.02083333
## [2,] -0.02083333  0.10416667
## > 
