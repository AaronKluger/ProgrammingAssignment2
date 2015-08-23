## Aaron Kluger R Programming Assignment 2
## My functions create a matrix and calculate its inverse. If the inverse calculation has already been performed, the result will be retrieved from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set <- function(y) {
            x <<- y
            n <<- NULL
      }
      
      get <- function() x
      setmatrix <- function(solve) n <<- solve
      getmatrix <- function() n
      list(set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}

## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix. Additionally, cacheSolve checks if the inverse has already been calculated (and the matrix remains the same); if so, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      n <- x$getmatrix()
      if(!is.null(n)) {
            message("fetching cached data")
            return(n)
      }
      matrix <- x$get()
      n <- solve(matrix, ...)
      x$setmatrix(n)
      n
}
