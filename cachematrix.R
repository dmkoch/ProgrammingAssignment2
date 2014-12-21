# makeCacheMatrix() and cacheSolve() implement a cache around
# the solve() function which computes the inverse of a matrix.
# Because solve() can take a long time to run on a large matrix,
# using a cache speeds up code where we need to compute a result
# repeatedly, for example within a loop.
#
# Example usage:
#
# x <- makeCacheMatrix(matrix(c(4, 3, 3, 2), 2, 2))
# x.inverse <- cacheSolve(x)


# Creates a special matrix wrapper that is really a list of
# functions that set or retrieve the value of the underlying matrix or
# that set or retrieve the cached value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        x.inverse <- NULL
        set <- function(y) {
                x <<- y
                x.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x.inverse <<- solve
        getinverse <- function() x.inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# A caching wrapper around solve() (which calculates the inverse
# of a matrix) that operates on cache matrix objects created by
# makeCacheMatrix() above.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        x.inverse <- x$getinverse()
        if (!is.null(x.inverse)) {
                message("getting inverse matrix from cache")
                return(x.inverse)
        }
        data <- x$get()
        x.inverse <- solve(data, ...)
        x$setinverse(x.inverse)
        x.inverse
}
