
##############################################################
#' Simulate a seedDispRcv process on a landscape.
#'
#' Simulate seed dispersal using user defined function. This is a "receiving pixel" focused dispersal approach.
#' It is the "potentially receiving" cell that looks around itself for potential seed sources. If it finds
#' a single seed source, that passes the probability function described by the dispersalFn, then the
#' cluster ends and the receiving cell index is returned as part of a vector of indices of all
#' successfully cells that received seeds. This function can therefore only be used for a relatively
#' specific situation where there is a yes/no returned for each potential receiving cell, i.e., not abundance.
#' This function is also not cumulative, i.e,. there is no higher abundance of seeds received if
#' a receiving cell has lots of seed sources around it vs. a single seed source. The difference will
#' come with a higher probability of successfully receiving a "seed".
#'
#' \code{dispersalFn} must be an expression that returns a probability distribution. Because
#' it is a dispersal kernal, it must be a probability distribution. The expression that can
#' take an argument named "dis" (without quotes) as this will be calculated internally and
#' represents the distance from the initial (receiving) pixel and all active pixels within that
#' cluster of active pixels. \code{SpaDES} includes the \code{\link{Ward}} kernel as defined in the
#' Landis documentation.
#'
#' @param seedSrc  A \code{RasterLayer} object where pixels indicate the presence (or abundance) of seed source
#' pixels
#'
#' @param seedRcv  A \code{RasterLayer} object where pixels indicate the potential pixels to receive seeds
#'
#' @param dispersalFn  An expression that can take a "dis" argument. See details. Default is "Ward"
#'
#' @param plot.it  If TRUE, then plot the raster at every iteraction, so one can watch the
#' seedDispRcv event grow.
#' @param effDist Landis species- and ecoregion-specific effective distance parameter
#'
#' @param maxDist  Landis species- and ecoregion-specific effective distance parameter
#'
#' @param b  Landis ward seed dispersal calibration coefficient (set to 0.01 in Landis)
#'
#' @param k  Landis ward seed dispersal the probability that seed will disperse within
#' the effective distance (eg., 0.95)
#'
#' @param ...   Additional parameters. Currently none
#'
#' @return A numeric vector of raster pixel indices, in the same resolution and extent as
#' \code{seedSrc} raster.
#'
#' @import data.table
#' @import raster
#' @import dplyr
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name seedDispRcv
#' @aliases seedDispRcv
#' @rdname seedDispRcv
setGeneric("seedDispRcv", function(seedSrc, seedRcv=seedSrc,
                                   dispersalFn=Ward,
                                   effDist=100, maxDist=150, b=0.01, k=0.95,
                                   plot.it=FALSE, ...) {
  standardGeneric("seedDispRcv")
})

#' @rdname seedDispRcv
#' @examples
#' library(raster)
#'
#' # Make random forest cover map
#' a <- raster(extent(0,1e4,0,1e4),res=100)
#' hab <- gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
#' names(hab)="hab"
#'
#' seedSrc <- hab>5
#' setColors(seedSrc,1) <- c("white","black")
#'
#' seedRcv <- hab>5
#' system.time(seeds <- seedDispRcv(seedSrc, seedRcv=seedRcv,
#'   maxDist=250, plot.it=TRUE))
#' seedRcvRaster <- raster(seedSrc)
#' if(length(seeds)>0) {
#'   seedRcvRaster[seeds] <- 1
#'   Plot(seedRcvRaster, cols="black")
#' }
setMethod("seedDispRcv",
          signature(seedSrc="RasterLayer"),
          definition = function(seedSrc, seedRcv, dispersalFn,
                                effDist, maxDist, b, k,
                                plot.it=FALSE, ...) {
            cellSize=unique(res(seedSrc))
            
            seedSrcVec <- getValues(seedSrc)
            seedRcv <- Which(seedRcv>0, cells=TRUE)
            if(length(cellSize)>1) stop("seedSrc resolution must be same in x and y dimension")
            ### should sanity check map extents
            if (is.null(seedRcv))  {
              # start it in the centre cell
              seedRcv <- (nrow(seedSrc)/2L + 0.5) * ncol(seedSrc)
            }
            lociReturn <- data.table(fromInit=seedRcv,key="fromInit")
            seedsArrived <- data.table(fromInit=numeric(),key="fromInit")
            
            if(plot.it) {
              wardSeedDispersalHab1 <- raster(seedSrc)
              wardSeedDispersalHab1[] <- NA
              assignGlobal("wardSeedDispersalHab1",wardSeedDispersalHab1)
              Plot(seedSrc, new=TRUE)
            }
            
            n <- cellSize
            
            potentials <- data.table("fromInit"=seedRcv,key="fromInit")
            potentials[,from:=fromInit]
            setkey(potentials,"from", "fromInit")

            while (length(seedRcv) & ((n-cellSize)<=maxDist)) { # while there are active cells and less than maxDistance

              # identify neighbours
              adjCells <- adj(seedSrc, seedRcv, directions=8, pairs=TRUE) %>%
                data.table(key="from")
              if(n>cellSize) {
                # replace "from" column with the values from the previous "to" column
                potentials[,`:=`(from=NULL,dis=NULL)][,from:=to][,to:=NULL]
                setkey(potentials,"from", "fromInit")
              }
              potentials <- potentials[adjCells, allow.cartesian=TRUE]
              
              if(plot.it) {
                wardSeedDispersalHab1[potentials[,from]] <- n
                assignGlobal("wardSeedDispersalHab1",wardSeedDispersalHab1)
                Plot(wardSeedDispersalHab1, addTo="seedSrc")
              }
              
              
              # Section - omit cells based on one of three criteria
              # 1. Can't spread backwards, within a cluster
              # keep only neighbours that have not been seedDispRcv to yet, within each
              # cluster. This means that a cell can't seedDispRcv backwards, but two different
              # clusters can be on the same cell
              potentials <- potentials[from!=to,.SD,by="fromInit"]

              # 2. Can't have more than one "arrival" in a potential "to" cell, by cluster
              # Don't know how to do next within data.table syntax - remove duplicate "to"
              #  within a cluster
              #  unique(., by=c("fromInit", "to"))

              potentials  <- potentials %>%
                group_by(fromInit) %>%
                filter(!duplicated(to)) %>%
                data.table

              # 3. remove any that are not within a 1 unit doughnut of
              # discard those that more than "n" units from a "from" cell. This keeps spreading
              #   in a circle. It is somewhat wasteful, because the distances are calculated above
              #   and then deleted here, but this may be the most efficient way
              nr <- NROW(potentials)
              
              xys <- xyFromCell(seedSrc, as.matrix(potentials[,list(fromInit,to)]))
              potentials[,dis:=pointDistance(xys[1:nr,], xys[(nr+1):(2*nr),], lonlat=FALSE)]
              potentials <- potentials[((n-cellSize) < dis) & (dis <= min(n,maxDist)),]
              #potentials <- potentials[abs(dis - n)<=min((n-cellSize),(maxDist-cellSize)),]

              # for speeding up. If no pixels within the doughnut are a seed source,
              #  just skip next block
              potentialsWithSeed <- as.logical(seedSrcVec[potentials[,to]])
              if(any(potentialsWithSeed)) {
                
                potentialsWithSeedDT  <- potentials[potentialsWithSeed,]
                nr <- NROW(potentialsWithSeedDT)
                setkey(potentialsWithSeedDT, "fromInit")
                
                potentialsWithSeedDT[,receivesSeeds:=runif(nr)<eval(dispersalFn)]
                receivedSeeds <- potentialsWithSeedDT[,any(receivesSeeds), by="fromInit"]
                
                #drop any that received seeds from potentials, as they are now in lociReturn
                if(NROW(receivedSeeds[V1==TRUE])>0) {
                  seedsArrived <- rbindlist(list(seedsArrived,lociReturn[receivedSeeds[V1==TRUE]][,V1:=NULL]))
                  setkey(seedsArrived, "fromInit")
                  setkey(potentials, "fromInit")
                  potentials <- potentials[!seedsArrived]
                }
              }
              
              n <- n+cellSize
              
              # refresh so that "to" cells become new "from" cells
              seedRcv <- potentials[,to]
              
            }
            return(seedsArrived$fromInit)
            
          }
)

##############################################################
#' Ward Seed Dispersal kernel
#'
#' A probability distribution used in Landis.
#'
#' @export
#' @docType methods
#'
#' @author Eliot McIntire
#'
#' @name Ward
#' @rdname Ward
Ward <- expression(if(cellSize<=effDist) {
  ifelse(dis<=effDist,
         exp((dis-cellSize)*log(1-k)/effDist)-
           exp(dis*log(1-k)/effDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
} else {
  ifelse(dis<=cellSize,
         exp((dis-cellSize)*log(1-k)/effDist)-(1-k)*
           exp((dis-effDist)*log(b)/maxDist),
         (1-k)*exp((dis-cellSize-effDist)*log(b)/maxDist)-
           (1-k)*exp((dis-effDist)*log(b)/maxDist))
})


