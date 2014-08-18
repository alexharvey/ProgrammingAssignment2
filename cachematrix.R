## makeCacheMatrix cache an inverse of a given matrix

## makeCacheMatrix
## Creates a special "matrix" which is a list containing:
## - a function to set the matrix (i.e. the values of its elements) -> set
## - a function to get the matrix  -> get
## - a function to set the inverse of the matrix -> setinverse
## - a function to get the inverse of the matrix -> getinverse
makeCacheMatrix <- function(x = matrix()) {
    # m will be a copy of the inverse matrix
    m <- NULL

    # Set the value of the matrix
    set <- function(Y) {
        X <<- Y
        m <<- NULL  
    }

    # Get the current matrix value
    get <- function() X 

    setinverse <- function(inverse) m <<- inverse 
    
    getinverse <- function() m
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve
## Computes the inverse of the matrix created with the makeCacheMatrix function. 
## If an inverse for the specified matrix has already been computed, return the cached copy
## Otherwise, compute the inverse, cache a copy and return it
cacheSolve <- function(X, ...) {
    # Check if we already have a copy
    m <- X$getinverse()
    if(!is.null(m)) { 
        return(m) 
    }
    
    # Calculate inverse and store in m
    data <- X$get() 
    m <- solve(data, ...) 
    
    # Cache computed copy
    X$setinverse(m) 
    
    # return the inverse
    m
}
