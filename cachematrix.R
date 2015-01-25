## This file contains the code for R Programming course: Assignment02
## functions do

## makeCacheMatrix creates list containing a function 
## set the value of the Matrix
## get the value of the Matrix
## set the inverse of the Matrix
## get the inverse of the Matrix

makeCacheMatrix <- function(m = matrix()) {
#Reset inverse 
inverse <- NULL
set <- function(y) {
# Reset values of m and inverse in parent environment 
m <<- y
inverse <<- NULL
}
get <- function() m
setinverse <- function(i) inverse <<-i # Reset values of inverse in parent environment 
getinverse <- function() inverse
#Create and return an anonymous list made of four components
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}



## cacheSolve calculates the inverse of the special "vector" created with the makeCacheMatrix function. 
## It calculates inverse only if the matrix is invertible. It is ensured using condition determinant !=0
## Only if it is not already created. For this purpose it first checks the cached inverse, to avoid re-computation. 

cacheSolve <-function(matrixList, ...) {
        
		#check if Inverse is already computed
        inverse <- matrixList$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
		#get the matrix created using makeCacheMatrix
        temp <- matrixList$get()
		#Check if matrix is invertible
		if(det(temp)!=0) {
		inverse <- solve(temp)
		#Compute inverse of matrix
		matrixList$setinverse(inverse)
        return(inverse)
		}
		else
		{
			message("It is not an Invertible Matrix")  
			return(inverse)
		}		
        
}
