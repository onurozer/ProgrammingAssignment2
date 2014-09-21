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

# The cacheSolve function checks if the inverse of a matrix has already
# been calculated and returns it from cache it if has. Otherwise, it calculates
# the inverse of a matrix and sets the value of matrixinverse variable in the cache
# via the setinverse function.


cacheSolve <- function(x, ...) {
    
    # Set matrixinverse variable value by using the getinverse function
    matrixinverse <- x$getinverse()
    
    # If the matrixinverse variable value is not null (cache is not empty),
    # return the value and exit the function
    if(!is.null(matrixinverse)){
        message("getting data from cache")
        return(matrixinverse)
    }
    
    # Cache is empty, so get value of the matrix by using the get function 
    # and assign it to "data"
    data <- x$get()
    
    # Calculate the inverse of data matrix and store it in matrixinverse variable
    matrixinverse <- solve(data, ...)
    
    # Call the setinverse function to assign the matrixinverse value
    x$setinverse()
    
    # Return matrixinverse value
    matrixinverse
}