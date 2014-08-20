## The functions create a matrix and cache its vaules such that they can be looked up in the cache rather than be recomputed, hence saving time.

## The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmatrix <- function(matrix) m <<- matrix
            getmatrix <- function() m
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)

}


## The 'cacheSolve' function computes the inverse of the special "matrix" returned by the above function. If the inverse has been calculated from the same matrix, it would retrieve the inverse from the cache instead of recomputing.

cacheSolve <- function(x, ...) {
        m <- x$get()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(solve(m))
            }
            data <- x$get()
            m <- matrix(data, ...)
            x$setmatrix(m)
            m
}
