#-----------------------------------------------------------------------------------#
#                                 Cache Inverse Matrix                              #
#                                                                                   #
# Programmed by:  Caroline Buckner                                                  #
# Programmed for: R Programming - Coursera                                          #
# Date:           12 March 2015                                                     #
# Date modified:  -- March 2015                                                     #
#-----------------------------------------------------------------------------------#
# Description:                                                                      #
#     The function takes in a square matrix. The matrix is converted into a         #
#     cache-able object. The matrix is inverted. Result is cached.                  #
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
#  The function creates a matrix object that can cache its inverse. Appropriate     #
#  attributes and methods are defined.                                              #
#-----------------------------------------------------------------------------------#

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL                                         # Set inverse to empty matrix
    
    set <- function(x) {                                # Create cache-able object
        x <<- x                                         # Define matrix attribute
        inv <<- NULL                                    # Define inverse attribute
    }

    
    get <- function() x                                 # Define method which obtians the matrix

    
    setinverse <- function(solve) inv <<- solve         # Define method to compute the inverse
    getinverse <- function() inv                        # Define method to retrieve cached inverse
    list(set = set, get = get,                          # List of object methods
         setinverse = setinverse,
         getinverse = getinverse)

}


#-----------------------------------------------------------------------------------#
# The  function computes the inverse of the object returned by makeCacheMatrix.     #
#  If the matrix is unchanged and the inverse has already been calculated,          #
#  then the function retrieves the inverse from the cache.                          #
#-----------------------------------------------------------------------------------#

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                               # Retrieve stored value of inverse
    if(!is.null(inv)) {                                 # If the stored value is NOT an empty matrix:
        message("Retrieving cached data...")            # display processing message
        return(inv)                                     # return the stored inverse.
    }else{                                              # Otherwise:
        message("Computing inverse...")                 # display processing message
        data <- x$get()                                 # retrieve the matrix
        inv <- solve(data, ...)                         # invert the matrix
        message("Caching result...")                    # display progress message
        x$setinverse(inv)                               # set the inverse in cache
        inv                                             # return the inverse
    }   
}


#######################################################################################