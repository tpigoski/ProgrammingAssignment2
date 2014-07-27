## Two functions used to create a matrix and cache the inverse of the matrix. 

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_x <<- inverse
        getinverse <- function() inverse_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a matrix if changed, otherwise returns cached version
cacheSolve <- function(x, ...) {
        inverse_x <- x$getinverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        inverse_x
}

