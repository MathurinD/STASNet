########################### plot_heatmap.R ###########################
# function to generate heatmap with cutoff and colormap scale function

#' Plot a customized heatmap with
#' @param mat a numeric matrix that should be plotted
#' @param lim a single number indicating the maximal range [-lim lim] for the color map
#' @param fixedRange logical inidicating whether the range of lim should at all means be kept (ensures comparability with other heatmaps)
#' @param stripOut numeric between [0 1] to determine the range of colors (excluding outliers from distorting the colormap) 
#' @param main a character string representing the title to be passed
#' @param col colorRampPalette object indicating the colormap
#' @param textCol string denoting the color of inset text
#' @return Nothing
#' @export
#' @author Bertram Klinger \email{bertram.klinger@charite.de}
plot_heatmap <- function(mat,main = "",lim = Inf,fixedRange = F,stripOut=0.05,col = colorRampPalette(c("deepskyblue","white","red1")),textCol = "gray10"){
  # helper functions
  define_breaks <- function(m,lim = Inf,fixedRange = F){ # when containing only one sign: 0...+-limit, otherwise -limit...+limit 
    if (!fixedRange){
    limit = min(lim,(max(abs(m),na.rm = T)))
    return(seq(-1.1*(limit)*ifelse(min(m,na.rm=T)<0,1,0),1.1*limit*ifelse(max(m,na.rm=T)>0,1,0),length.out=22))
    }else{
    return(seq(-1.1*lim,1.1*lim,length.out=22))  
    }
  }
  
  # cutoff and transformation of colour
  m = mat
  if (stripOut >= 0.5) { stop("Cannot strip more than 50% of the data to generate the color scale") } 
  if (!is.numeric(lim)) {stop("lim should be numeric")}
  lowLim = max(-lim,quantile(mat,stripOut))
  upLim = min(lim,quantile(mat,1-stripOut))
  m[m < lowLim] = lowLim
  m[m > upLim] = upLim
  breaks=define_breaks(m,lim,fixedRange)
  
  # linearized matrix order (1,1) (2,1) (3,1) (4,1)... for text inset
  ref=c(t(as.matrix(mat[nrow(mat):1,]))) 

    # plot heatmap with textual inset
  p<- levelplot(x = t(as.matrix(m[nrow(m):1,])),
                col.regions = col,
                at = breaks,
                colorkey = list(space = "left"),
                aspect = "fill",
                xlab = "",
                ylab = "",
                main = main,
                scales = list(alternating = 2,tck = c(0,1),x = list(rot = 90)),
                panel=function(...){
                      arg<-list(...)
                      panel.levelplot(...)
                      panel.text(arg$x,
                                 arg$y,
                                 signif(ref,2),
                                 col = textCol,
                                 cex = 0.8)})
  print(p)
}