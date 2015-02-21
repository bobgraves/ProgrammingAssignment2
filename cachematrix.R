## cachematrix.R
#####################################
## Bob Graves, R Programming (rp-011). Assignment #2
## This file includes functions that use caching to optimize the runtime for matrix inversions
## makeCacheMatrix() - provides get/set functions for operating on a matrix
##
## cacheSolve() - caclulates the inverse of a matrix made by makeCacheMatrix, or re-uses 
## the inverse if the inverse has already been computed.
##
## testSolve() - unit tests to verify the makeCacheMatrix and cacheSolve functions
#####################################

## makeCacheMatrix is a constructor function for matrices.
## This function works with cacheSolve to reuse computed inverses of a matrix
## makeCacheMatrix returns a list with functions to get and set the value of the matrix,
## as well as get and set the inverse (solve)
## makeCacheMatrix assumes that the supplied matrix (x) is always invertable

makeCacheMatrix <- function(x = matrix()) {
    ## m is the inverted matrix 
    ## get and set are typical accessor/mutator functions
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## setsolve uses the <<- operator assign the value of the inverse to m, outside of the function's environment
    setsolve <- function(solve) m <<- solve
    
    ## getsolve returns the inverted matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## cacheSolve returns a matrix that is the inverse of 'x', either by directly 
## computing with solve() or by returning a pre-computed (i.e. cached) inverse
## x must be a "made" matrix using makeCacheMatrix()
## addition arguments (...) are fowarded to solve()

cacheSolve <- function(x, ...) {
    ## if the inverse has been computed previously, then notify that a cached 
    ## value is being returned
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached inverse of the matrix")
        return(m)
    }
    ## since m is null at this point, we get the matrix using the helper function
    ## and then solve() for the inverse and then store the inverse using setsolve 
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


## testSolve contains test cases for verifying the makeCacheMatrix and cacheSolve functions
## For each test case, we create a test matrix, make a cached matrix out of that matrix by
## calling makeCacheMatrix, then call the cacheSolve twice to show that on the second call 
## we observe that the message about using cached values is shown
##
## Verification is by inspection. 

testSolve <- function() {
    # Case 1 (1x1 matrix)
    print("Case 1 - 1x1 matrix")
    m <- matrix(1:1,nrow=1,ncol=1)
    mx <- makeCacheMatrix(x = m)
    print("Case 1 - first call to CacheSolve ")
    mi <- cacheSolve(mx)
    print(mi)
    print("Case 1 - second call to CacheSolve")
    mi2 <- cacheSolve(mx)
    print(mi2)
    print("Case 2 - 2x2 data=1:4 matrix")
    m <- matrix(1:4,nrow=2,ncol=2)
    mx <- makeCacheMatrix(x = m)
    print("Case 2 - first call to CacheSolve ")
    mi <- cacheSolve(mx)
    print(mi)
    print("Case 2 - second call to CacheSolve ")
    mi2 <- cacheSolve(mx)
    print(mi2)
    print("Case 3 - 2x2 matrix")
    m <- matrix(c(4,3,3,2),nrow=2,ncol=2)
    mx <- makeCacheMatrix(x = m)
    print("Case 3 - first call to CacheSolve ")
    mi <- cacheSolve(mx)
    print(mi)
    print("Case 3 - second call to CacheSolve ")
    mi2 <- cacheSolve(mx)
    print(mi2)
}
