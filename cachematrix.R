## This script is able to cache a potential time consuming computation that is calculating inverse of matrix.
## Define a function to create a object which contains functions to hold and return matrix and its inverse.
makeCacheMatrix <- function(x = matrix())
{
	inv<-NULL
	## Function to set the matrix.
	set <- function(y)						
	{
	x <<- y
	inv <<- NULL 
	}
	## Return the matrix.
	get <- function() x				
	## Set the inverse of matrix.
	setinverse <- function(inverse) inv <<- inverse			
	## Return the inverse of matrix.
	getinverse <- function() inv				
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Define a function to compute the inverse or if already computed retrieve it from cache
cacheSolve <- function(x, ...)
{
  ## Retrieve the inverse of matrix.
	inv <- x$getinverse()					
	## Condition to test retrieved inverse value.
	if (!is.null(inv))						
	{
		message("getting cached data")		
	## If retrieved inverse is present return it and get out of function.
		return(inv)						
	}								
	## If retrieved inverse is not present get the original matrix   
  data <- x$get()							
	## calculate the inverse					
  inv <- solve(data, ...)					
	## set it 
	x$setinverse(inv)						
	## and also return it
	inv								
}		
