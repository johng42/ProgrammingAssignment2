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