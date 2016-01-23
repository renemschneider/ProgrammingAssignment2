## Assignment 2: Caching inverse matrices

## This function receives a matrix as argument and runs 4 functions to set the value of the matrix
## in the cache and reserving a cache value for the corresponding inverted matrix,
## then: caclulating the inverted value
## then: getting the inverted value
## It finally packs all matrices in one list, which is the return value of this function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## creates empty matrix
  set <- function(y) { ## caches matrix x
    x <<- y ##sets argument x to y
    write (x)
    m <<- NULL ## creates empty matrix in the cache
  }
  get <- function() x ## simply gets back matrix x
  setInv <- function(solve) m <<- solve ## calculats inverted matrix and assigns it to the cached empty matrix
  getInv <- function() m ## gets the matrix from the cache
  write (m)
  list(set = set, get = get, ## returns all functions of function makeCacheMatrix as a list of functions
       setInv = setInv,
       getInv = getInv)  
}

## This function receives a functionname and a matrix in the ... argument 
## and returns the inverse of the ... argument as value
## It computes the inverted value only if it has not been set before in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() ##checks in the cache if inverted matrix exists
  if(!is.null(m)) { ## if inverted matrix exists, this matrix is given back from the cache and returned
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) ##calculates an inverted matrix and assigns it to m
  x$setInv(m) ##sets value of the inverted matrix
  m ##returns m
}
