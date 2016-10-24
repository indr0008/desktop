## makeCacheMatrix creates a list whose elements are matrices. Most importantly, getinv contains the inverse of the input matrix (x) and it is cached

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  return(list(set = set, get = get,
       setinv = setinv,
       getinv = getinv))
}

## cacheSolve returns the inverse of the input matrix (x), if it was already cached, returns the cache instead

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
