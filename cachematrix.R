## Usage
#
# > m1 <- makeCacheMatrix(matrix(1:4, nrow = 2))
# > m1$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m1)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## Creates a vector of functions to access the stored matrix and its inverse
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solves the inverse of a matrix, but first trying to fetch it from a cached value
## Parameter x: the cacheMatrix vector provided by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
