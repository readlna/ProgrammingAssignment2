## *sorry for the grammar mistakes*

## The functions below are used to save on computation time and resource when calculating
## the inverse of many matrices. It creates the inverse of the matrix and also "saves" it 
## for later, just in case we need it again. The second time it is needed, it will NOT be 
## calculated again, but taken from the cache

## makeCacheMatrix() creates a list of functions (set, get, setInverse, getInverse) which will
## be referenced in the second function (cacheInverse)

makeCacheMatrix <- function(x = matrix()) {

            m <- NULL
            set <- function(y) { ## sets the value of the matrix
                  x <<- y
                  m <<- NULL
            }
            get <- function() x ## gets the value og the matrix
            setInverse <- function(solve) m <<- solve ## sets the inverse of the matrix
            getInverse <- function() m ## gets the inverse of the matrix
            ## at this step a list of functions is created, which is the actual output
            ## of the makeCacheMatrix function
            list(set = set, 
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
      }
      

## cacheInverse() will receive a list of functions created previously
## checks if the inverse of the matrix is already in the cache
## if so, it will take it from there, otherwise will calculate it and save it for later

cacheInverse <- function(x, ...) {
            ## tries to get the inverse from the cache
            m <- x$getInverse()
            ## if found, will print a message saying that the inverse was already calculated
            ## and taken from the cache
            if(!is.null(m)) {
                  message("getting cached inverse")
                  return(m)
            }
            ## otherwise calculates it: "data" will store the matrix itself, which is
            ## reached by the get() function (see previous function)
            data <- x$get()
            ## "m" stores the inverse of the matrix calculated with the solve() function
            m <- solve(data, ...)
            ## the inverse of the matrix (stored in m) will be passed to the cache 
            ## by the setInverse() function 
            x$setInverse(m)
            ## the inverse is then printed on the screen
            message("calculating inverse")
            m
      }
      
## Example:

# > matrix<-matrix(1:4, 2, 2)
# > matrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# > CacheMatrix<-makeCacheMatrix(matrix)
# > CacheMatrix
# $set
# function (y) 
# {
#       x <<- y
#       m <<- NULL
# }
# <environment: 0x00000000122c3968>
#       
#       $get
# function () 
#       x
# <environment: 0x00000000122c3968>
#       
#       $setInverse
# function (solve) 
#       m <<- solve
# <environment: 0x00000000122c3968>
#       
#       $getInverse
# function () 
#       m
# <environment: 0x00000000122c3968>
# 
## Running the first time it is calculated
# > cacheInverse(CacheMatrix)
# calculating inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
## Running the second time it gets from the cache
# > cacheInverse(CacheMatrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
## Just to make sure that a simple "solve()" function gives the same output:
# > solve(matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
