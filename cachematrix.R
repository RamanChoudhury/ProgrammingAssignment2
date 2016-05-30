## There are 2 functions below




##makeCacheMatrix creates a vector that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(mInverse) m <<- mInverse
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve extracts the inverse of the matrix from the special data structure created in makeCacheMatrix

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}
