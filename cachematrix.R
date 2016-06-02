## makeCacheMatrix and cacheSolve functions create a special object,
## that stores a numeric matrix (assumed to be invertible) and cache's its inversion.
## Before using cacheSolve special "matrix" must be created by calling makeCacheMatrix function.
## Supplied matrix must be invertible.
## For the purpose of calculation of the inverse of the matrix the R function solve(X) is used.


## makeCacheMatrix function creates a special "matrix", which is a list containing a function to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sol <<- solve
        getsolve <- function() sol
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function calculates the inverse of the special "matrix" created with makeCacheMatrix function.
## It first checks if the inverse has already been calculated. If so, it gets the inverse from the cache and returns it.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
sol <- x$getsolve()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}
