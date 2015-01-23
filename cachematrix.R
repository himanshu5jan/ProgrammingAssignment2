############################################################################################################
## This program attempts to reduce the computation cost for Matrix inversion by providing pair of functions
## to compute the inverse of supplied matrix and caching the result to avoid redundant computation in cases 
## where input remains unchanged.
## 
## Assumption : This program works under the assumption that matrix supplied is always invertible.
############################################################################################################


## ################################################################################################
## Function Name 	  : makeCacheMatrix 
## Description	  : This function creates a special "matrix" object that can cache its inverse
## Param In 	  : An invertible matrix
## Param Out   	  : List of functions defined inside function makeCacheMatrix (getMatrix, setMatrix,setInvMatrix, getInvMatrix)
## ################################################################################################
makeCacheMatrix <- function(x = matrix()) {
invMatrix<-NULL

# Function to retrieve the input matrix
getMatrix<-function() {  
		x
	}

# Function to change the existing matrix
setMatrix<-function(y) {
       x<<-y
       invMatrix<<-NULL
	}

# Function to variable 'invMatrix' in the containing environment
setInvMatrix<-function(inv) {
      invMatrix<<-inv
	}

# Function to retrieve the inverse of supplied matrix
getInvMatrix<-function() {
	invMatrix
	}
list(getMatrix=getMatrix,setMatrix=setMatrix,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}


## ################################################################################################
## Function Name   : cacheSolve
## Description 	 : This function is responsible for caching the inverse of the supplied matrix, 
##		         and uses the chached data to print the result if input matrix is unchanged.
## Param In  	 : An invertible matrix and other variable number of arguments represent by ...
## Param Out       : Inverse of the input Matrix
## ################################################################################################
cacheSolve <- function(x, ...) {

      invMatrix<-x$getInvMatrix()

	#check if invMatrix is already already calculated 
      if(!is.null(invMatrix)) {
           message("getting cached inv matrix")
	     return(invMatrix)
	}     
	matrixData<-x$getMatrix()
      invMatrix<-solve(matrixData)
	x$setInvMatrix(invMatrix)
      invMatrix     	
}
