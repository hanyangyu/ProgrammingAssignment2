## Put comments here that give an overall description of what your
## functions do

# set the value of matrix
# get the value of matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# computes the inverse of the special matrix 
# returned by makeCacheMatrix above
# if inverse has already been calculated
# cacheSolve should retrieve the inverse from the cache
# Otherwise, it calculates the inverse of the data 
# and sets the value in the cache via the set_inverse function

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
