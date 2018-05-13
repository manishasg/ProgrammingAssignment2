## Compute and cache inverse of a matrix for further use.

##Saving the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(i) xi <<- i
  getinverse <- function() xi
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

##Computing,Caching and utilizing the cache for inverse of a matrix saved inside object x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- inv(data, ...)
  x$setinverse(xi)
  xi
}
