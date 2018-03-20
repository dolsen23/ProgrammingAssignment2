## The following two functions, when used in tandem, allow the user to calculate
## the inverse of a matrix and cache the inverse in memory.

## makeCacheMatrix creates an object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
                ## Initializes the inverse matrix object
                inv <- NULL
                
                ## Takes advantage of lexical scoping to assign the input argument
                ## to x in the parent environment. Also sets inv to NULL to clear any
                ## prior stored values.
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                
                ## Getter function to retrieve x from the parent environment.
                get <- function() {
                        x
                }
                
                ## Setter function for the inverse of the matrix
                setInverse <- function(solve) {
                        inv <<- solve
                }
                
                ## Getter function for the inverse of the matrix
                getInverse <- function() {
                        inv
                }
                ## Returns a list containing set, get, setInverse, and getInverse.
                ## This will be the input for the cacheSolve function defined below.
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix.
## If the inverse has already been calculated and the input matrix has not changed,
## then the function will simply retrieve the cached inverse instead of recomputing it.

cacheSolve <- function(x, ...) {
                ## Uses the getter function getInverse defined within the body
                ## of the makeCacheMatrix function and assigns its value to inv
                inv <- x$getInverse()
                
                ## Checks to see if the inverse is already cached and if so,
                ## returns the cached inverse
                if(!is.null(inv)) {
                        message("Getting cached data")
                        return(inv)
                }
                
                ## If there is no cached value, or if the matrix has changed,
                ## then gets the matrix whose inverse we want to compute
                matrix <- x$get()
                
                ## Computes the inverse
                inv <- solve(matrix, ...)
                
                ## Sets the inverse to be cached
                x$setInverse(inv)
                
                ## Returns the inverse
                inv
}

