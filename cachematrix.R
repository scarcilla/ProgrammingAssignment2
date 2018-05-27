## Below functions cache the inverse of a matrix so that computing
## for the inverse of the same matrix will not be done repeatedly.

## makeCacheMatrix is a function that creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              invmatrix <- NULL
              set <- function(y) {
                  x <<- y
                  invmatrix <<- NULL
              }
              get <- function() x
              setinverse <- function(inverse) invmatrix <<- inverse
              getinverse <- function() invmatrix
              list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not
## changed), then the cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {
            message("getting cached data")
            return(invmatrix)
        }
        mat <- x$get()
        invmatrix <- solve(mat, ...)
        x$setinverse(invmatrix)
        invmatrix
}
