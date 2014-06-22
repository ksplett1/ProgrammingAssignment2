    ## ##################################################################
##
## Coursera course: R Programming 
## Course number: rprog-004
## Programming Assignment 2
##
## Assignment: 	Cache the inverse of a matrix
## Purpose:    	Write an R function that is able to cache potentially 
##		time-consuming computations

## From the Assignment Description:
## Write the following functions:
##
## 1.  makeCacheMatrix : This function creates a special "matrix" object 
##     that can cache its inverse.
## 2.  cacheSolve : This function computes the inverse of the special "matrix" 
##     returned by  makeCacheMatrix  above. If the inverse has already been 
##     calculated (and the matrix has not changed), then cacheSolve should retrieve 
##      the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.


## Example Usage:
## example: identity matrix (note: the R diag function creates a diagonal matrix)
## > m <- makeCacheMatrix ( diag(nrow = 2, ncol = 2) )
## > m$get()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## >  cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## Revision History
## 06/21/2014 Katherine Splett	Initial Version

##      *******************************************************

## note: The ginv function works for all matrices. Suggest considering ginv 
##       instead of solve for future courses

## Notes from "Quick Review of Matrix Algebra in R"
## http://www.johnmyleswhite.com/notebook/2009/12/16/quick-review-of-matrix-algebra-in-r/
##
## The definition of a matrix’s inverse is that the product of the matrix and 
## its inverse is the identity matrix, if the inverse exists. 
## 
## More interestingly, the MASS package defines a ginv function, which gives the 
## matrix pseudoinverse, a generalization of matrix inversion that works for all 
## matrices:library('MASS')
##
## > m <- matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2)
##
## > solve(m)
## Error in solve.default(m) : 
##  Lapack routine dgesv: system is exactly singular
##
## > ginv(m)
##      [,1] [,2]
## [1,]  0.1  0.1
## [2,]  0.2  0.2

## ########################################################################
## ########################################################################


    ## ##################################################################

    ## makeCacheMatrix 

    ## Description:
    ## Create a special matrix object
    ## with get and set functions for a matrix and its inverse matrix.
    ##
    ## Matrix and inverse matrix values are saved in variables within
    ## the makeCacheMatrix function environment. These variables exist
    ## for the lifetime of the makeCacheMatrix object.
    ##
    ## Returns a list of functions that can be called for the matrix object

    ## Parameters:
    ## x: the R matrix to save in cache. Default value is an empty matrix.

    ## ##################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    # save matrix in function parameter x
    # save inverse matrix in local variable minverse

    # initialize inverse matrix to be empty
    minverse <- NULL


    # assign y to matrix  
    set <- function(y) {
        x <<- y

	# assign inverse matrix to empty
        minverse <<- NULL
    }

    # return matrix
    get <- function() x

    # assign m to inverse matrix
    setsolve <- function(m) minverse <<- m

    # return inverse matrix
    getsolve <- function() minverse


    # return list of functions that can be called for the matrix object
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


    ## ##################################################################

    ## cacheSolve 

    ## Description:
    ## Compute the inverse of a matrix 
    ##
    ## If the inverse has already been calculated and the matrix has not 
    ## changed, then cacheSolve returns the inverse from the cached value.
    ##
    ## If the inverse has not been calculated, call the R solve function
    ## to compute the matrix inverse.
    ##
    ## Returns the matrix inverse as an R matrix data type

    ## Parameters:
    ## x: Object of type makeCacheMatrix created for the matrix
 
    ## ##################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # get the cached inverse matrix value
    m <- x$getsolve()

    # if a cached inverse matrix value is found, return the cached inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # if cached inverse matrix not found
    # get the matrix
    data <- x$get()

    # compute the inverse matrix
    ## note: matrix assumed to be an invertable matrix 
    m <- solve(data, ...)

    # assign inverse to the cached value for the inverse matrix
    x$setsolve(m)

    # return the computed inverse matrix
    m
}
