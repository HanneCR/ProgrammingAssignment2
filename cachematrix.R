# The first function, makeVector creates a special "matrix"
# which is really a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      # Set the value of the matrix
      setmatrix <- function(y) { 
            x <<- y
            inverse <<- NULL
      }
      # Get the value of the matrix
      getmatrix<-function() x
      # Set the value of the inverse
      setmatrixinv<-function(solve) inverse<<- solve
      # Get the value of the inverse
      getmatrixinv<-function() inverse
      
      list(setmatrix=setmatrix, getmatrix=getmatrix, setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv)
}


#cacheSolve: Compute the inverse of the matrix
#If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      inverse<-x$getmatrixinv()
      # If the inverse is already calculated, it returns the cached inverse
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      # Otherwise, it calculates
      matrix<-x$getmatrix()
      inverse<-solve(matrix, ...)
      x$setmatrixinv(inverse)
      inverse
}

#Example
x<-matrix(c(0,1,-2,-2,1,5,1,0,4),3,3) #create a matrix x 
dx <- makeCacheMatrix(x) #create a special matrix
dx$getmatrix() #Return the matrix
cacheSolve(dx) #Return the inverse
cacheSolve(dx) #Return the cached inverse
