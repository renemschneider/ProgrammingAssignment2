## Assignment 2: Caching inverse matrices

## This function receives a matrix as argument and runs 4 functions to set the value of the matrix
## in the cache and reserving a cache value for the corresponding inverted matrix,
## then: caclulating the inverted value
## then: getting the inverted value
## It finally packs all matrices in one list, which is the return value of this function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    write (x)
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  write (m)
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}

## This function receives a functionname and a matrix in the ... argument 
## and returns the inverse of the ... argument as value
## It computes the inverted value only if it has not been set before in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
