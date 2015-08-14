## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function sets the value of a matrix, gets the value of the matrix, 
##sets the value of the inverse and gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) 
{
	invrse <- NULL
	set<- function(y)
	{
		x <<- y
		invrse <<- NULL
	}
	get <- function() x
	setinverse <-function(inverse) invrse <<- inverse
	getinverse <- function() invrse
	list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##This function first checks to see if the inverse has already been computed. If not then it will calculate
##the inverse and set the value in the cache via setinverse
cacheSolve <- function(x, ...) 
{
	invrse <- x$getinverse()
	if(!is.null(invrse))
	{
		message("getting cached data")
		return(invrse)
	}
	data <- x$get()
	invrse <- solve(data)
	x$setinverse(invrse)
	invrse
        ## Return a matrix that is the inverse of 'x'
}
