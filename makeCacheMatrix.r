makeCacheMatrix<-function(m=matrix)
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