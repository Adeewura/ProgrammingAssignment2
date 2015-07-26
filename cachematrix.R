## Template used for this code is from the Example on the instrutions page for assignment
## The makeCachematrix creates a special "matrix", that can cache its inverse. The function
### sets the value of the matrix
### gets the value of the matrix
### sets the value of the inverse
### gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" given by the above makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated and if the matrix has not changed.
## If both these conditions are met, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}