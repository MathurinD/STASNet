# Helper functions for the STASNet package

#' Geometric mean
#'
#' Compute geometric mean with na.rm == TRUE
geom_mean <- function(xx, na.rm=TRUE) {
    xx=xx[xx>0]
    if (all(is.na(xx))) {
        return(NaN) # Return NaN for consistency with mean
    } else if (length(xx)>0) {
        xx = xx[!is.na(xx)]
        return( exp(sum(log(xx), na.rm=na.rm)/length(xx)) )
    } else {
        warning("No or only negative values for geom_mean !")
        return(NA)
    }
}


#' Standard deviation of the log
#'
#' Standard deviation of the log in linear space
linear_sd_log <- function(xx, na.rm=TRUE) {
    xx = xx[xx>0]
    if (all(is.na(xx))) {
        return(NaN)
    } else if (length(xx) > 0) {
        return( exp(sd(log(xx), na.rm=na.rm)) )
    } else {
        warn("Only negative values for linear_sd_log !")
        return(NA)
    }
}

#' Plot residuals vs rank
#'
#' Plot all residuals vs rank and best residuals vs rank to assess the quality of the optimisation
#' @param A list of residuals
residuals_plot <- function(residuals, model_name="default") {
    order_resid = order(residuals,na.last = T)
    order_id = order_resid[1:min(20, length(residuals))]
    if ( all(is.na(residuals)) ) {
        warning("All residuals are NAs! (no residual plot possible)")
    } else {
        plot(1:length(order_resid), residuals[order_resid], main=paste0("Best residuals ", model_name), ylab="Likelihood", xlab="rank", log="y",type="l",lwd=2)
        lines(1:length(order_id), residuals[order_id], col="red")
        if (length(order_resid) >= 100) {
            hundred_best = residuals[order_resid[1:100]]
            plot(1:100, hundred_best, main=paste0("Best 100 residuals ", model_name), ylab="Likelihood", xlab="rank", log="y",type="l",lwd=2, ylim=c(hundred_best[1], hundred_best[100]+1))
            lines(1:length(order_id), residuals[order_id], col="red")
        }
    }
}

# return output-friendlier numbers trimmed by the specified behind_comma and non-zeros  
trim_num <- function(x, non_zeros=2, behind_comma = 2){
    if (non_zeros==0){
        error("number should have a digits reconsider setting non_zeros larger 0!!")
    }
    trim_it <- function(x, non_zeros, behind_comma){  
        if (is.na(x)){ return(x) }
        
        if (!is.numeric(x)){ oldx =x; x = as.numeric(as.character(x)) } 
        
        if (is.na(x)){ stop(paste("Number or NA expected '", oldx ,"' received as input!")) } 
        
        if (abs(x >= 1)){ 
            newx = round(x*10^behind_comma)/10^behind_comma
        } else{
            newx =  signif(x,non_zeros)  
        }
        
        if (nchar(gsub("\\.|-","",as.character(newx))) > max(5,non_zeros+behind_comma)){
            newx = format(newx, scientific = 0)  
        }
        return(newx)
    }
    
    if (is.null(dim(x))){
        return(sapply(x,"trim_it",non_zeros,behind_comma))
    }else{
        newx=sapply(1:ncol(x),function(y) sapply(x[,y],"trim_it",non_zeros,behind_comma))
        dimnames(newx) <- dimnames(x)
        return(newx)
    }
}

# to estimate running time
get_running_time <- function(init_time, text="") {
    run_time = proc.time()["elapsed"]-init_time
    run_hours = run_time %/% 3600;
    run_minutes = (run_time - 3600 * run_hours) %/% 60;
    run_seconds = round(run_time - 3600 * run_hours - 60 * run_minutes,0);
    return(paste(run_hours, "h", run_minutes, "min", run_seconds, "s", text))
}

# helper function to determine variable links
not_duplicated <- function(x){
    tmp = duplicated(x)
    return(!all(tmp[-1]))
}
