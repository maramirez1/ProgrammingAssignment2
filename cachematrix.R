## Functions for caching the inverse of a matrix

## Sets and gets the value of the vector.
## Sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the matrix returned by
## the makeCacheMatrix() function. If the inverse has
## already been calculated, it retrieves the inverse from
## the cache, otherwise, it proceeds with the calculations.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
              message("Getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}
