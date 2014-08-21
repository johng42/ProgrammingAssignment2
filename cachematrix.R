## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and caches its solved value so that it does
## not need to be recomputed if requested a second time

makeCacheMatrix<-function(m=matrix())
{
    n<-NULL
    set <- function(y)
    {
        m<<- y
        n<<- NULL
    }
    get<-function() m
    #need to use the solve() function to compute the inverse
    setInverse <-function(solve) n<<-solve
    getInverse <- function() n
    #return a list of the matrices
    list(set=set,get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

## checks to see if the solved data is cached, and returns that value w/o computation if so

cacheSolve <-function(m,...)
{
    #quick check to see if the results of solve are cached
    n<-m$getInverse()
    if(!is.null(n))
    {
        #results are in the cache, so just return the results w/o computation
        message("Getting cached data...")
        return (n)
    }
    #results were not in cache so need to compute
    data<-m$get()
    n<-solve(data,...)
    #return the inverse
    m$setInverse(n)
}