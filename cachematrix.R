## The two functions below are built to cache the inverse of a matrix.
## If the inverse has already been calculated, it will retrieve from cache.
## If not, the second function also stores the new inverse value into the cache.

## First function creates a matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## second function to use to check if the inverse has already been calc, retrieve cache, and add to cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv          ## Return a matrix that is the inverse of 'x'
}
