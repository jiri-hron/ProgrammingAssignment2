## This code is used for construction of a special data structure which
## serves as a crate for both the matrix itself and for its inverse.
## The only purpose is to store inverse matrix after the first calculation
## so it won't be wasting processor time anymore.

## Data structure for storing the matrix and its inverse.
## The structure is not read-only, i.e. matrix data can be
## updated and the inverse is set to null -> call function
## cacheSolve below to solve and get the inverse.
## Returns a list of setter/getter functions for the matrix
## and for the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL;
  set <- function(y) {
    x <<- y;
    i <<- NULL;
  }
  
  get <- function() x;
  setInverse <- function(inverse) i <<- inverse;
  getInverse <- function() i;
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse);
}


## Solve and get the inverse matrix, store the inverse
## so that next time this function is called, it just
## returns the results of last call, unless the matrix
## data has changed.
## The function expects the matrix passed is non-singular,
## otherwise ends-up with an error.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse();
  
  if(!is.null(i)) {
##    message("getting cached data");
    return(i);
  }
  
  data <- x$get();
  i <- solve(data, ...);
  x$setInverse(i);
  
  i;
}
