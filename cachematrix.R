## Put comments here that give an overall description of what your
## functions do

## This function caches the inverse of the matrix.
## The inverse is accessed by getinverse() and setinverse() methods

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse of the matrix x
        inverse_x <- NULL 
        
        # setter function for the matrix x
        set <- function(y){
                x <<- y                         # setting the matrix object to x
                inverse_x <<- NULL
        }
        
        # getter function for the matrix x
        get <- function() x                     # returns the matrix object x
        
        # setter function for the inverse matrix object
        setinverse <- function(inv_x) inverse_x <<- inv_x
        
        # getter function for the inverse matrix object
        getinverse <- function() inverse_x     # returns the inverse object x
        
        # creating a list to hold all the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of a matrix.
## The inverse is first checked in the cache. If there is a cache miss, the inverse is computed using 'solve' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # attempts a cache read
        inverse_x <- x$getinverse()
        
        if(!is.null(inverse_x)) {
                # there is a cache hit.
                message("getting cached data")
                return(inverse_x)
        }
        
        # there is a cache miss for inverse. Compute inverse by solve()
        data <- x$get()
        inverse_x <- solve(data, ...)
        x$setinverse(inverse_x)
        inverse_x
}

## TESTING THE FUNCTIONS
# Call  the makeCacheMatrix() function and assign it's return value ( a list of four functions) to a variable, x
# x is now a list of four functions
mat_x <- makeCacheMatrix()

# use x's set() to create invertible matrix
mat_x$set(matrix(c(4,3,3,2), nrow=2,ncol=2, byrow=T))

# check the value of vector x by using the get() function of x
mat_x$get()

# Invoke cacheSolve() with the vector x. The inverse of the matrix should be [-2 3; 3 -4] (Read row-wise).
cacheSolve(mat_x)

# Execute the above statement again to find the message "getting cached data", which indicates the use of cache for retrieval