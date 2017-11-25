## Programming Assignment2 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL

        set <- function(y) {
                x <<- y
                cache <<- NULL
}

        get <- function() x
        setMatrix <- function(inverse) cache <<- inverse
        getInverse <- function() cache

        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     cache <- x$getInverse()

        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }

        matrix <- x$get()
        tryCatch( {
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )
        return (cache)
}


