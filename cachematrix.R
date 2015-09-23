## These functions take a matrix, cache it and its inverse.
## This allows for more efficient computation if the inverse of a the given matrix
## is used repeatedly, so that we can compute it once, and reuse it as many times
## as is required.

## makeCacheMatrix: takes a matrix and calculates its inverse. Both the original matrix
## and the computed inverse are cached for potential later reuse.

makeCacheMatrix <- function(x = matrix()) {
    ## creates a matrix-like object (a list really), that allows to
    ## cache the data and its inverse, using lexical scoping.
    
    i <- NULL
    
    # define each of the setter/getter functions
    
    set <- function(y) {
        # Sets the matrix object to 'x', and initial NULL inverse 'i'.
        # As neither 'x' nor 'i' are defined within the function, 
        # they correspond to the 'x' and 'i' in the this function's 
        # defined environment, namely 'x' is the argument to the parent function,
        # and i is defined in the parent function as well.
        x <<- y
        i <<- NULL
    }
    
    get <- function() { x } # returns the original matrix
    
    setinverse <- function(inverse) { i <<- inverse } # sets the inverse to the parent function's 'i' variable
    
    getinverse <- function() { i } # returns the inverse matrix.
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # returns a list with a set of getter/setter functions
}

## cacheSolve: takes a matrix and returns its inverse. This requires that the matrix object
## be created with the makeCacheMatrix, otherwise it will raise an exception. If the inverse 
## has not been created before, it computes it and caches it as well for future usage. Otherwise, 
## it uses the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    ## 'x' should be a list created using the 'makeCacheMatrix' function.
    
    
    i <- x$getinverse() # sets the inverse matrix from the 'x' list.
    
    if(!is.null( i )) {
        # is the inverse has been computed in a previous call, return that instance
        message("Returning cached inverse.")
        return( i )
    }
    
    # Compute the inverse
    
    d <- x$get() # get the original matrix

    message("Computing inverse and caching.")
    i <- solve(d, ...) # compute the inverse
    x$setinverse( i ) # cache the inverse for potential future use
    
    i # return the inverse matrix of 'x'
}
