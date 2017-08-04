require(sp)
require(raster)
require(spatstat)
require(KernSmooth)
require(geometry)

generateMesh <- function(CrimeData,timeRange,spaceRange,pixRes,ENode,plot=TRUE){
  # subset data
  year1 <- timeRange[1]
  year2 <- timeRange[2]
  CrimeData.sub <- subset(CrimeData,YEAR==year1|YEAR==year2,select=c("DATEOCC","X_COORD","Y_COORD","INC_CNT"))
  
  nx <- pixRes[2]
  ny <- pixRes[1]
  
  X_range=spaceRange$x
  Y_range=spaceRange$y
  
  r <- raster(ncol=nx,nrow=ny,xmn=X_range[1],xmx=X_range[2],ymn=Y_range[1],ymx=Y_range[2])
  CrimeData.subRaster <- rasterize(CrimeData.sub[,c("X_COORD","Y_COORD")], r, 
                                   CrimeData.sub$INC_CNT, fun=sum)
  
  # rasterize the data to an image
  rasterMat <- matrix(CrimeData.subRaster@data@values,nx,ny)
  rasterMat[is.na(rasterMat)] = 0 
  
  # KDE
  # find the optimal kernel
  grd <- expand.grid(list(X_COORD = seq(X_range[1], X_range[2], length.out=nx), 
                          Y_COORD = seq(Y_range[1], Y_range[2], length.out=ny)))
  CrimeData.pp <- ppp(grd$X_COORD,grd$Y_COORD,window=owin(xrange=X_range,yrange=Y_range),marks=as.vector(rasterMat))
  h <- bw.diggle(CrimeData.pp)
  h <- round(as.numeric(h))
  
  # Smoothing
  KDE.df <- data.frame(X_COORD=grd$X_COORD,Y_COORD=grd$Y_COORD,VALUE=rep(NA,nx*ny))
  kernSm <- bkde2D(data.matrix(CrimeData.sub[,c("X_COORD","Y_COORD")]), bandwidth=c(h,h), 
                   gridsize=c(nx, ny), range.x=list(X_range,Y_range))
  KDE.df$VALUE <- as.vector(kernSm$fhat)     
  
  # scale KDE values to [0,1]
  KDE.df$val_scale <- rep(NA,nrow(KDE.df))
  KDE.df$val_scale <- KDE.df$VALUE-min(KDE.df$VALUE)
  KDE.df$val_scale <- KDE.df$val_scale/max(KDE.df$VALUE)
  
  # error diffusion
  M <- ny
  N <- nx
  
  threshold <- sum(KDE.df$val_scale)/(2*ENode)
  
  pixelIm <- matrix(KDE.df$val_scale,M,N) 
  
  source("Dither.R")
  
  b = errorDiffusion(pixelIm, 1, threshold)
  
  # node placement
  meshgrd <- expand.grid(x=seq(X_range[1],X_range[2],length.out=N), y=seq(Y_range[1],Y_range[2],length.out=M))
  
  Reg_x <- matrix(meshgrd$x,M,N)
  Reg_y <- matrix(meshgrd$y,M,N)
  
  temp <- t(Reg_x)
  Node_x <- temp[t(b)==1]
  temp <- t(Reg_y)
  Node_y <- temp[t(b)==1]
  
  Node_x[Node_x>X_range[2]] <- X_range[2]
  Node_y[Node_y>Y_range[2]] <- Y_range[2]
  Node_x[max(Node_x)==Node_x] <- X_range[2]
  Node_y[max(Node_y)==Node_y] <- Y_range[2]
  
  # Delaunay triangulation
  tri1 <- delaunayn(cbind(Node_x,Node_y))
  pts <- cbind(Node_x,Node_y)
  meshVertices <- rbind(tri1[, -1], tri1[, -2], tri1[, -3])
  
  if (plot==TRUE){
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    
    par(mfrow=c(1,2),oma=c(0,0,2,0))
    image(x=seq(X_range[1],X_range[2],length.out=N),y=seq(Y_range[1],Y_range[2],length.out=M),
          z=pixelIm,col=jet.colors(256),xlab="X coordinate",ylab="Y coordinate")
    plot.new()
    plot.window(xlim=c(X_range[1],X_range[2]),ylim=c(Y_range[1],Y_range[2]))
    axis(1)
    axis(2)
    box()
    segments(pts[meshVertices[, 1], 1], pts[meshVertices[, 1], 2], pts[meshVertices[, 2], 1], pts[meshVertices[, 2], 2], col="blue")
    title(paste0("Crime Density (",year1,"-",year2,") and Mesh"), outer=TRUE, cex.main=1)
  }
  
  return(list(Tri=tri1,meshNode=pts,meshVertices=meshVertices,pixelIm=pixelIm))
}

