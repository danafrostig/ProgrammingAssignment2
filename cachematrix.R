## The first function creates a matrix object that can cache its inverse, and the second
## function retrieves the inverse of the matrix from cache if already calculated, otherwise
## it computes the inverse of the matrix object

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set inverse of matrix
  setminv <- function(minv) m <<- minv 
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getminv()
  if(!is.null(m)) {
    message("getting matrix inverse from cache")
    return(m)
  }
  data <- x$get()
  ## computes inverse of the matrix
  m <- solve(data, ...)
  x$setminv(m)
  m
}

##df