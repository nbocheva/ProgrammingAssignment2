## Put comments here that give an overall description of what your
## functions do

## Function MakeCacheMatrix takes as argument a matrix and returns a list of functions that 
## allow to change the value of the matrix, using $set, to view the value of the matrix with 
## $get, to set the value of the inverse matrix by $setInverse or to take the value of the 
## inverse matrix by $getInverse. One drawback of this function is that it does not check 
## whether the input matrix is square, as needed to inverse calculation. No error occurs as
## in fact this function does not calculate the inverse matrix, it only takes the result 
## from cacheSolve.
##Function cacheSolve calculates an inverse matrix if it is not calculated before or uses 
## the already calculated inverse matrx. To do this, its argument is a special matrix, 
## created by MakeCacheMatrix. 

## Write a short comment describing this function

## a function that creates a special "matrix" object - a list that contains function to set
## and get a matrix and to set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                        # assigns a NULL to indicate an empty object in the 
                                                   # current environment
  set <- function (mtx)   {      
    x <<- mtx                                      # assign to x the variable mtx from the parent environment.    
    m <<- NULL                                     # In this case this is the global environment    
  }
  get <- function()x
  setInverse <- function(inverse) {                # inverse is the name given to the parameter of setInverse
    m <<- inverse                                  # as m is not defined in the current environment, it is necessary  
    }                                              # to assign this parameter to it in the parent environment   
  getInverse <- function()m
  list(set = set, get = get,                       # list wrapping four functions: set, get, setInverse and getInverse
       setInverse = setInverse,
       getInverse = getInverse)
  
}





## Write a short comment describing this function

## CacheSolve is a function that gives the inverse of a special "matrix" object created by makeCacheMatrix. 
## If the inverse exists, the calculation is skipped and the cache value obtained by getInverse is 
## returned. Otherwise, the inverse is calculated and the result is assigned to the special "matrix" object
## using setInverse so that this value could be used as cache if needed later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                             # query whether x is cache
  if (!is.null(m)) {                              # if the cache exists
    message("getting cached data")                # indicate that this
  return(m)                                       # use the cache
  }
  data <- x$get()                                 # if not, get the value of the matrix
  m <- solve(data,...)                            # calculate its inverse
  x$setInverse(m)                                 # assing the inverse matrix to x
  m                                               # return the result
}
    

