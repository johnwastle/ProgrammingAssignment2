## Don’t use comments unless they are absolutely necessary. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invP) inv <<- invP
  getinv <- function() inv
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
