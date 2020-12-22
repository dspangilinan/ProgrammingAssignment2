## These 2 functions are used to create a special matrix that can store the matrix and cache its inverse


## makeCacheMatrix function creates a special matrix using the list of functions inside it namely set, get, set.inverse, and get.inverse
## the special matrix created by this function can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <-  function(inverse) inv <<- inverse
  get.inverse <- function() inv
  list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}


## cacheSolve makes use of the special matrix returned by the makeCacheMatrix function
## It checks if the inverse has already been computed. If yes, it retrieves the inverse from the cache and skips the computation. If not, it proceeds to compute for the inverse and sets the inverse in the cache via set.inverse function

cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set.inverse(inv)
  inv
}
