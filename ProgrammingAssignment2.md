##
## ProgrammingAssignment2
Bob Graves  
R Programming (rp-011)  
Assignment #2  

## cachematrix.R
This file includes functions that use caching to optimize the runtime for matrix inversions

makeCacheMatrix() - provides get/set functions for operating on a matrix

cacheSolve() - caclulates the inverse of a matrix made by makeCacheMatrix, or re-uses 
the inverse if the inverse has already been computed.

testSolve() - unit tests to verify the makeCacheMatrix and cacheSolve functions
