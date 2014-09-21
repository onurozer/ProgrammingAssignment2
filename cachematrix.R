# This program contains functions to calculate the inverse of a matrix, 
# store it in a cache, and return the value from cache.

# The makeCacheMatrix function below takes a matrix as argument and 
# returns a list that contains functions to get and set the value of
# the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    # Create a matrixinverse variable and set its value to NULL
    matrixinverse <- NULL
    
    # Set the value of the x in the parent environment to argument y
    # and set the matrixinverse variable to NULL    
    set <- function(y){
        x <<- y 
        matrixinverse <<- NULL
    }
    
    # Return the value of variable x
    get <- function() x
    
    # Get the inverse of the matrix x and assign it to matrixinverse
    # variable in the parent environment
    setinverse <- function() matrixinverse <<- solve(x)
    
    # Return the value of variable matrixinverse
    getinverse <- function() matrixinverse
    
    # Return a named list that holds the functions set, get, 
    # setinverse and getinverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    }

## TODO Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    matrixinverse <- x$getinverse()
    
    if(!is.null(matrixinverse)){
        message("getting data from cache")
        return(matrixinverse)
    }
    data <- x$get()
    matrixinverse <- solve(data, ...)
    x$setinverse()
    matrixinverse
}

