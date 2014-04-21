## These functions implement a cacheable invert operation for a matrix
## As this is a time consuming operation, we use the << operator to 
## cache the inverted matrix and then serve up the cached copy using the 
## cacheSolve() function if it is available.

## makeCacheMatrix takes a regular matrix and exposes getter and setter 
## functions for the cached version of the matrix. It returns a list of 
## functions that are usable with the cached matrix (get,set,getinverse,setinverse)

makeCacheMatrix <- function(x = matrix()) {
  ## m is the variable where the cache is stored
  m <- null
  ## set() voids the cache when the underlying matrix is changed and stores
  ## the new matrix in x
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  ## get() returns the raw matrix
  get <- function() x
  ## setinverse() stores the inverse into the cache variable m
  setinverse <- function(inverse) m <<- inverse
  ## getinverse() returns the cached value
  getinverse <- function() m 
  ## return the list of function prototypes for this structure
  list (set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## This function sets and fetches the cached value from the makeCacheMatrix
## structure.  If the value is already cached, it simply returnes the cached value,
## otherwise it computes the inverse using solve() and caches that value in 
## the makeCacheMatrix structure.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check the cached value, if it is not null, return it.
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Fetch the matrix and compute the inverse, store it, and return the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
