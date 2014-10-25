## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation. 
## This function caches the inverse of a matrix after first run
## rather than computing it repeatedly

## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(M = matrix()) {

  I <- NULL
  set <- function(Mparam) {
    M <<- Mparam
    I <<- NULL
  }
  get <- function() M
  setmean <- function(Inv) I <<- Inv
  getmean <- function() I
  
  invisible(list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean))
  
  
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(Mx, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- Mx$getmean()
  if(!is.null(I)) {
    message("getting cached Matrix")
    return(I)
  }
  
  I <- solve(Mx$get(), ...)
  Mx$setmean(I)
  I
}
