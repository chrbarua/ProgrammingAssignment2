# makeCacheMatrix() creates a special "matrix" object that
# can cache its inverse.
# The <<- operator assigns a value to an object in an
# environment that is different from the current environment.
# The solve() function is used to find the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve() calculates the inverse of the special "matrix"
# returned by makeCacheMatrix(). If the inverse has already
# been calculated, and the matrix has not changed, then
# cachSolve() will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
