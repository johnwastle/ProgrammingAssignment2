
matrixWithCachableInverse <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(pMatrix) {
    matrix <<- pMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(pInverse) inverse <<- pInverse
  getInverse <- function() inverse
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
cachingSolveMatrix <- function(matrix, ...) {  
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("using cached data")
    return(inverse)
  }
  inverse <- solve(matrix$get(), ...)
  matrix$setInverse(inverse)
  inverse
}
