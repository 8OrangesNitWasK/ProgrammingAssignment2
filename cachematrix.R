## Amod Bhole 1/12/2023
## The functions are intended to provide the ability to calculate the inverse 
## of a matrix and cache it to make repeated calculations of the same matrix 
## inversion more efficient.

## This function will create a list of functions to get and set the value of a 
## matrix, and get and set the value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)

}


## This function will check if the matrix in question has an inverse already
## cached. If not, it will calculate and cache that inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## If inverse is found (not null), return that value
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) ## solves for x such that data * x = identity
        x$setinverse(inv)
        inv
}
