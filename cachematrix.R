# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

# stores the cached value
# initialize to NULL
  inv <- NULL
  set <- function(y) {

# create the matrix in the working environment
# use `<<-` to assign a value to an object in an environment 
# different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  
# get the value of the matrix
  get <- function() x

# invert the matrix and store in cache
  setinverse <- function(inverse) inv <<- inverse

# get the inverted matrix from cache
  getinverse <- function() inv

# return the created functions to the working environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function calcluates the inverse of the matrix created in makeCacheMatrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If the inverted matrix does not exist in cache,
# it it created in the working environment and it's inverted value
# is stored in cache

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
#attempt to get the inverse of the matrix stored in cache
  inv <- x$getinverse()
  
# return inverted matrix from cache if it exists
# else create the matrix in working environment
  if(!is.null(inv)) {
    message("getting cached data.")
    
# display matrix 
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
