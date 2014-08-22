## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# +------- makeCacheMatrix -----------------------------------------+
# |  This function creates a special object of type matrix          |
# |  with several methods:                                          |
# |    1. set(m): loads the matrix with a usual R-matrix object (m) |
# |    2. get() : gets the value of the matrix                      |
# |    3. setInverse(inv) : sets inv as the Inverse matrix value    |
# |    4. getInverse() : gets the current Inverse matrix value      |
# |  The methods are returned as a list                             |
# +-----------------------------------------------------------------+
makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL # Slot for the Inverse
    set <- function (y) {
        # Sets a new value for matrix x
        x <<- y
        # Since the x value has changed the
        # Inverse has to be recalculated and this is
        # signaled to the cacheSolve function by setting
        # its value to NULL
        Inverse <<- NULL
    }
    get <- function() x # Simply returns x value
    setInverse <- function(inv) Inverse <<- inv # Sets the Inverse
    getInverse <- function() Inverse # Simply returns Inverse's value
    # The four methods are to be returned:
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## Write a short comment describing this function
# +-------------------- cacheSolve ---------------------+
# | Provided there is an special objetc of the type     |
# | matrix, created with the makeCacheMatrix function,  |
# | when giving it as argument to this function it      |
# | computes the inverse of the matrix, if it didn't    |
# | exist previously, or just retrieves its value in    |
# | the other case.                                     |
# +-----------------------------------------------------+
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        # the inverse already exists
        # and its value has been retrieved
        # in inv. Let's notify this ...
        message("getting cached data")
        # ... and return the value
        return (inv) # We're done!
    }
    # From here on, we have to compute
    # the inverse.
    # First, let's recover the stored matrix
    m <- x$get()
    # Then, let's compute the inverse ...
    message("... computing ...") # notifying what we're doing
    inv <- solve(m, ...)
    # ... and store its value for a possible
    # future use
    x$setInverse(inv)
    # ... and finally return the computed value:
    inv
}

# +------- test --------------------------------------+
# | This function is to test the functions in this    |
# | file.                                             |
# | You can call it as follows:                       |
# |   test()                                          |
# | or with your own test matrix                      |
# |   m <- rbind(c(4,8),c(-2,0))                      |
# |   test(m)                                         |
# +---------------------------------------------------+
test <- function(m=rbind(c(3,3,0),c(-1,4,2),c(1,0,-1))) {
    # Let's create a matrix object for the given 'm'
    m.obj1 <- makeCacheMatrix(m)
    # Print the value:
    print("The value is:")
    print(m.obj1$get())
    # Let's get its inverse
    print(cacheSolve(m.obj1)) # Computing
    # Let's do it for the second time
    print(cacheSolve(m.obj1)) # Retrieving
    # Now, let's change the value of the
    # matrix in the object
    m.obj1$set(rbind(c(3,3,0),c(1,0,-1),c(-1,4,2)))
    # Let's get its inverse for this new value
    print(cacheSolve(m.obj1)) # Computing
    # Let's do it for the second time
    print(cacheSolve(m.obj1)) # Retrieving
}
