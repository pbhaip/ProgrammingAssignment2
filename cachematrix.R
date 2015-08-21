# Summary of functions for speedy matrix inversion using cache
#   Matrix inversion is compute intensive and it is beneficial to cache the inverse 
#   if the inverse has already been calculated (for the matrix which has not changed).
#   Following are set of two functions makeCacheMatrix which creates a special "matrix" object
#   holding the inverse and set of functions. The second function "cacheSolve" will source in the matrix for
#   which the inverse has to be calculated. The second function uses the special "matrix" object
#   to check if the inverse already exist. If the inverse does not exist it will use the "solve" 
#   function to get the inverse. The cacheSolve will message if inverse alread exists
#   and retrieve from the cache. This will save the computation time needed to
#   recalculate the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix ()) {
#
# Description
#   This function creates a special "matrix" object which will be used to cache (store) the matrix inverse.
#
# Usage
#   makeCacheMatrix(x) where x = Matrix of dimension n*n
#  
# Arguments :
#   x : Matrix of dimension n * n  
#
# Returns :
#   special "matrix" object holding matrix inverse along with functions (set, get, setinverse, getinverse)
#
# Example 1
#   X <- matrix (c(2,5,3,1), nrow = 2, ncol = 2, byrow = TRUE)
#   z <- makeCacheMatrix(X)
# Example 2
#   X <- matrix(rnorm(10*10)  nrow=10)
#   z <- makeCacheMatrix(X)  
#==============================================================================
#initalize inverse_matrix 
      inverse_matrix <- NULL 
 
# set function is used to set the global parameters x and inverse_matrix which holds the inverse of matrix      
        set <- function(y) {
	    x <<- y
	    inverse_matrix <<- NULL
	}
		    
 # get is a function to return x  
	get <- function() x

# setinverse is used to assign the global inverse_matrix with the passed parameter   
        setinverse <- function(inv) inverse_matrix <<- inv

#getinverse is used to return the inverse_matrix   
      getinverse <- function() inverse_matrix
			    
#list defines the set of functions for this special matrix object  
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
       }

#===============================================================================
cacheSolve <- function(x, ...) {
#
# Description
#   This function computes the inverse of the matrix contained in the special "matrix" object
#   returned by makeCacheMatrix. 
#   If the inverse has already been calculated (and the input matrix has not changed), then 
#   the cacheSolve will retrieve the inverse from the cache. This saves computation time for calculating
#   the inverse of the matrix.  
#
# Usage
#   cacheSolve(z) where "z" is special "matrix" object holding matrix inverse along with 
#   functions (set, get, setinverse, getinverse) 
#  
# Arguments :
#   z : special "matrix" object holding matrix inverse along with 
#   functions (set, get, setinverse, getinverse) returned by makeCacheMatrix   
#
# Returns :
#   matrix inverse (cache copy if it already exists), if not cached thn calcuates matrix inverse
#
# Examples 1
#    X <- matrix (c(2,5,3,1), nrow = 2, ncol = 2, byrow = TRUE)
#    z <- makeCacheMatrix(X)
#    cacheSolve(z)
#    The following call will return from cache    
#    cacheSolve(z)   
#  
# Example 2
#    X <- matrix(rnorm(10*10), nrow=10)
#    z <- makeCacheMatrix(X)
#    cacheSolve(z)
#    The following call will return from cache    
#    cacheSolve(z)

#invoke the getinverse function with the special "matrix object" to get the inverse matrix  
	inverse_matrix <- x$getinverse()
						       
# check if the object returned inverse_matrix is not null, if not null implies that this object is holding
# the inverse matrix previously computed. If null implies that inverse needs to be calculated.
        if(!is.null(inverse_matrix)) {
	     message("getting cached data")
	     return(inverse_matrix)
	   }
								     
#get the matrix for which the inverse has to be computed  
       data <- x$get()
#  print(data)
							         
#performs the inverse of the given matrix using the Solve function  
 	inverse_matrix <- solve(data, ...)
#  print(inverse_matrix)

# this function sets the inverse global value in the special matrix object.
       x$setinverse(inverse_matrix)
       return (inverse_matrix)
}


