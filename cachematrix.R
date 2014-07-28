## Caching the Inverse of a Matrix

#' Creates a special "matrix" object that can cache its inverse.
#'
#' @param x a square matrix object.
#' @return special "matrix" corresponds with `x`.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cache <<- inverse
        getinverse <- function() cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#' Computes the inverse of a special "matrix" object.
#'
#' @param x special "matrix" returned by `makeCacheMatrix`.
#' @param ... the rest of parameters are passed to solve() function.
#' @return inverse matrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
