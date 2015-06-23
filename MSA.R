#' Seasonal Adjustment Plots
#'
# Ajuste sazonal
MSA <- function(x,spec = NULL, ...){
  
  if(!("seasonal" %in% installed.packages())){
    stop(paste("package 'seasonal' not found."))
  }
  
  # x: object of class "mts"
  if(is.mts(x)){
  names <- colnames(x)  
  out_x13 <- c()
  
  # Para cada uma das séries, aplicar a rotina abaixo.
  for(name in names){
    x.ts <- x[,name]
    x.start <- start(x.ts)
    x.end <- end(x.ts)
    
    out_x13[[name]] <- tryCatch(
      seasonal::seas(x.ts, ...),
      error = function(e){NULL})
    
    if(is.null(out_x13[[name]])){
      out_x13[[name]]$x <- x.ts
      warning(paste(name, "cannot be seasonally adjusted!"))
    }
  }
  out_x13 
  }else{
    stop(paste("x is not an object of class 'mts'"))
  }
}

# Graphical Analysis
MSA.GA <- function(x){
  names <- colnames(x)
  for(name in names){
    x.ts <- na.omit(x[,name])
    par(mfrow = c(2,1))
    plot(x.ts, main = paste("Original Series:", name), ylab = name)
    monthplot(x.ts, col.base = 2, lty.base = 2, labels = month.abb, 
              main = "Monthplot Original Series")
    par(mfrow = c(1,1)) 
  }
}

# Evaluation automatic model
MSA.EV <- function(x){
  
  if(!("ggplot2" %in% installed.packages())){
    stop(paste("package 'ggplot2' not found."))
  }
  if(!("gridExtra" %in% installed.packages())){
    stop(paste("package 'gridExtra' not found."))
  }
  
  names <- names(x)
  for(name in names){
    if(class(x[[name]]) == "seas"){
      x.seas <- x[[name]]
      x.ts <- x.seas$x
      cat(paste('# ------- begin', name, '--------- #'))
      print(summary(x.seas))
      print(qs(x.seas))
      plot(x.seas, main = paste0(name, "Original and Adjusted Series"))
      monthplot(x.seas, col.base = 1, lty.base = 2, labels = month.abb, lwd.base = 2)
      
      spec.orig <- tryCatch(data.frame(series(x.seas, "sp0")), error = function(e) NULL)
      spec.seas <- tryCatch(data.frame(series(x.seas, "s1s")), error = function(e) NULL)
      spec.irr <- tryCatch(data.frame(series(x.seas, "s2s")), error = function(e) NULL)
      spec.rsd <- tryCatch(data.frame(series(x.seas, "spr")), error = function(e) NULL)
      
      if(!is.null(spec.orig)){
        orig <- ggplot2::ggplot(aes(x=Pos,y = X10.Log.Spectrum_AdjOri.), data = spec.orig, colour = "black") +
          ggplot2::geom_line() +
          ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
          ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
          ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
          ggplot2::ggtitle("Spectral plot of the first-differenced original series") +
          ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
        
        seass <- ggplot2::ggplot(aes(x=Pos,y = X10.Log.Spectrum_SA_SEATS.), data = spec.seas, colour = "black") +
          ggplot2::geom_line() +
          ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
          ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
          ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
          ggplot2::ggtitle("Spectrum of the differenced final SEATS seasonal adjustment") +
          ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
        
        
        irr <- ggplot2::ggplot(aes(x=Pos,y = X10.Log.Spectrum_Irr_SEATS.), data = spec.irr, colour = "black") +
          ggplot2::geom_line() +
          ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
          ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
          ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
          ggplot2::ggtitle("Spectrum of the final SEATS irregular") +
          ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
      
        rsd <- ggplot2::ggplot(aes(x=Pos,y = X10.Log.Spectrum_Rsd.), data = spec.rsd, colour = "black") +
          ggplot2::geom_line() +
          ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
          ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
          ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
          ggplot2::ggtitle("Spectral plot of the regARIMA model residuals") +
          ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
        
        gridExtra::grid.arrange(orig, seass, irr, rsd, ncol = 2)
      }
      cat(paste('# ------- end', name, '--------- # \n'))
      cat('\n')
      }
  }
}





 
    
