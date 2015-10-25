## The following functions are used to create a special object that stores
## an invertible matrix and cache's the inverse of that matrix.

## The first function creates a list containing four functions which set and get
## the values of the vector and set & get the inverse matrix of 'x'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The next function return a matrix that is the inverse of 'x'. First, it checks
## if the inverse has already been calculated. If it's not, it calculates it and
## sets the value in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
