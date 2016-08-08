
## Creates list of functions to be used as inputs for cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
      
      ## Clear any data that may be in 'inverse'
      inverse <- NULL
      
      ## Creates element that assigns function input to 'x' in parent 
      ## environment and clears data in 'inverse'
      set <- function(y) {
            
            x <<- y
            inverse <<- NULL
      }
      
      ## Creates element that contains input matrix
      get <- function () x
      
      ## creates element that stores inverted matrix in parent environment
      setinverse <- function(inversemat) inverse <<- inversemat
      
      ## Creates element that contains inverted matrix
      getinverse <- function() inverse
      
      ## Creates list of all elements created in function
      list <- list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve checks to see if the inverse of matrix 'x' has been previously 
## calculated and stored in memory. If it has been calculated, cacheSolve pulls
## the inverse matrix from memory. If not, it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## Pulls the data currently stored in x$getinverse into the function
      inverse <- x$getinverse
      
      ## Checks if the variable 'inverse' has been calculated
      if(!is.null(inverse)) {
            
            ## If 'inverse' was calculated, it displays result & exits 
            ## function
            message("Getting cached data")
            return(inverse)
      }
      
      ## If 'inverse' did not contain data, inverts the matrix provided in 
      ## makeCacheMatrix, stores result in cache, and displays result

      data <- x$get
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
