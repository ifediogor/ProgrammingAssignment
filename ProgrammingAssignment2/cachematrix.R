## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to set and get the matrix, and to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse to NULL
    
    set <- function(y) {  # Function to set the matrix
        x <<- y
        inv <<- NULL  # Reset the cached inverse when the matrix is set
    }
    
    get <- function() {  # Function to get the matrix
        x
    }
    
    setInverse <- function(inverse) {  # Function to set the inverse of the matrix
        inv <<- inverse
    }
    
    getInverse <- function() {  # Function to get the cached inverse of the matrix
        inv
    }
    
    list(set = set, get = get,  # Return a list of the above functions
         setInverse = setInverse,
         getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse
    
    if (!is.null(inv)) {  # If the inverse is already cached, return it
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()  # Get the matrix from the special "matrix" object
    inv <- solve(mat, ...)  # Compute the inverse of the matrix
    x$setInverse(inv)  # Cache the inverse
    
    inv  # Return the inverse
}
