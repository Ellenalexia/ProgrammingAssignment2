## Assignment: 	Programming Assignment 2 
## Class: 		R Programming July 2015.
## Learning 
## objectives:	computational efficiency considerations
## 				scoping rules
##				git and github basics
##
## NOTES ABOUT THIS ASSIGNMENT
## * Did not test that matrix is inversible, per instructions
##
## * VERY USEFUL extensive comments on model code are
##   by Community TA Vladimir Kirilenko at
##   https://github.com/DanieleP/PA2-clarifying_instructions
##
## * Watch out for typos - dot syntax habits
##   from other programming languages will break things :)
##
## * Sample to test work (for this July 2015 class) is at
##   https://class.coursera.org/rprog-030/forum/thread?thread_id=561#comment-2518

## store a list of mini functions
makeCacheMatrix <- function(x = matrix()) {
	my_inv <- NULL

	## mini function to set values in parent environment
	set <- function(y){
		x <<- y
		my_inv <<- NULL
	}
	## mini function to retrieve value of x
	get <- function() x
	
	## mini function to store value in parent environment
	setinverse <- function(solve) my_inv <<- solve
	
	## mini function to retrieve value of my_inv
	getinverse <- function() my_inv
	
	## create list with mini functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Use mini functions from makeCacheMatrix above
## If inverse exists & matrix hasn't changed, get inverse from cache
## If inverse !exist, create it

cacheSolve <- function(x, ...) {

	## call mini function to get value of my_inv
	my_inv <- x$getinverse()
	
	## if my_inv is not null, say so + return its value + end
	if(!is.null(my_inv)){
		message("getting cached data")
		return(my_inv)
	}
	
	## call mini function to get value of x and store it in `data`
	data <- x$get()
	## call solve to get inverse of `data`
	my_inv <- solve(data, ...)
	## call mini function to set inverse
	x$setinverse(my_inv)
	## return my_inv
	my_inv
}
