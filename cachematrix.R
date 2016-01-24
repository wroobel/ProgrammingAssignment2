## Function makeCacheMatrix creates a matrix that can cache its inverse, it accomplishes that by means of a set of functions:
## get, set, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Takes input from function above, depending on outcome of makeCacheMatrix function either retrieves inverted matrix
## or calculates the inverse 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("get data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
