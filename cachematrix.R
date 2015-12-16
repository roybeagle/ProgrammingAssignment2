## Put comments here that give an overall description of what your
## functions do

## takes in a matrix creates a special matrix which is a LIST containing a function 
## to 1.set the value of the vector 2.get the value of the vector 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # initialize var for inverse value
  set <- function(y) {  #setter
    x <<- y
    s <<- NULL
  }
  get <- function() x  #getter
  setsolver <- function(solver) s <<- solver #sets value to global var s
  getsolver <- function() s  #gets value s
  list(set = set, get = get,  #list of functions to execute
       setsolver = setsolver,
       getsolver = getsolver)
}


## This takes in the special matrix from above and gets 
## a value for s, the inv solution from cache.  If s= NULL then
##it calculates an inverse of s, a var retrieved from getsolver

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolver()  #get s for s
  if(!is.null(s)) {  #checks if solution already present
    message("getting cached data")
    return(s)
  }
  data <- x$get() #gets matrix via get
  s <- solve(data, ...) #solves if no s
  x$setsolver(s) #sets solutons s via set solver
  s
}


