script.dir <- dirname(sys.frame(1)$ofile)

PARALLEL.SCRIPT.BASENAME <- paste("Par", "process-uni-ARX", sep = "-")
PARALLEL.SCRIPT.BASENAME <- paste(PARALLEL.SCRIPT.BASENAME, "R", sep = ".")
PARALLEL.SCRIPT.NAME <- file.path(script.dir, PARALLEL.SCRIPT.BASENAME)
source(PARALLEL.SCRIPT.NAME)

library(doParallel)
library(e1071)
library(ggplot2)
library(signal)


process.parallel <- function(
  src.dir,
  tgt.dir,
  tgt2.dir,
  src.ext,
  tgt.ext,
  parametros,
  lags,
  keep.nstats,
  src.basename,
  export.names = export.names,
  export.packages = export.packages
){
  step.test = FALSE
  tgt.basename <- paste(src.basename, tgt.ext, sep = ".")
  tgt.file <- file.path(tgt.dir, tgt.basename)

  lags.df <- expand.grid(lags, KEEP.OUT.ATTRS = FALSE)
  lags.results <- foreach(
     lag.list = t(lags.df),
    .inorder = TRUE,
    .export = export.names,
    .packages = export.packages,
    .errorhandling = "pass",
    .verbose = FALSE
  ) %dopar%
    get.results(
      parametros,
      lags = lag.list,
      src.basename = src.basename,
      src.dir = src.dir,
      src.ext = src.ext,
      keep.nstats = keep.nstats,
      step.test
    )
  lags.results <- do.call(rbind, lags.results)
  
  i <- order(-lags.results["test.cor"])
  lags.results <- lags.results[i, ]
  if(nrow(lags.results) > keep.nstats)
  {
    i <- 1:keep.nstats
    lags.results <- lags.results[i, ]
  }
  #aqui partir la prueba del escalon
  rownames(lags.results) <- NULL
  saveRDS(lags.results, file = tgt.file, compress = FALSE)
  for(i in 1:nrow(lags.results)){
    lags <- list(MABP = lags.results[i,"MABP"],CBFV = lags.results[i,"CBFV"],fold = lags.results[i,"fold"])
    cost = lags.results[i,"cost"]
    nu = lags.results[i,"nu"]
    parametros <- list(
      nu = nu,
      cost = cost
    )
    result<-get.results(
      parametros,
      lags = lag.list,
      src.basename = src.basename,
      src.dir = src.dir,
      src.ext = src.ext,
      keep.nstats = keep.nstats,
      step.test = TRUE
    )
    if(result[8]==1){
      tgt.file <- file.path(tgt2.dir, tgt.basename)
      saveRDS(result, file = tgt.file, compress = FALSE)
      break
    }
  }
}

run<- function(
  
  nworkers =detectCores(),#pueden ser m?x 48, pero con 32 o 16 andar? bien
  src.basenames = c(
                    'ALSA',
                    'ANGL',
                    'ARVA',
                    'BYLA',
                    'CLSE',
                    'DAOC',
                    'DASI',
                    'GAGO',
                    'JOBO',
                    'FEBE',
                    'FEGA',
                    'HEMU',
                    'HEFU',
                    'SEVE',
                    'GOAC',
                    'JULE',
                    'LACA',
                    'LMAR',
                    'MAIN',
                    'ROMI',
                    'ROAL',
                    'PAAR',
                    'MIMO',
                    'NIGA',
                    'MIRA',
                    'CLHE',
                    'DABA',
                    'HC036101',
                    'HC061101'
                    ),
  src.ext = "txt",
  tgt.ext = "RData",
  src.folder = "Data",
  results.folder = "Results",
  type.folder = "Univariado",
  model.folder = "ARX",
  bestmodel.folder = "Best models"
)
{
 
  src.dir <- file.path(script.dir, src.folder)
  tgt.dir <- file.path(script.dir, results.folder,type.folder,model.folder)
  tgt2.dir <- file.path(script.dir, results.folder,type.folder,model.folder,bestmodel.folder)
  dir.create(tgt.dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tgt2.dir, showWarnings = FALSE, recursive = TRUE)

#   cost <- 1024
#   nu <- 0.5
#   lags <- list(MABP = 7,CBFV = 3,fold = 1)
  
  cost2 <- 2^seq(-2, 10, 1)
  cost <- c(cost2,10000)
  nu <- seq(0.1, 0.9, 0.1)
  lags <- list(MABP = 3:8,CBFV = 1:3,fold = 1:2)


  parametros <- list(
                  nu = nu,
                  cost = cost
                )
  
  export.names <- c(
    'get.results',
    'retardos_multi',
    'training',
    'eval.model',
	'lag.abp'
  )
  
  export.packages <- c("e1071")
  
  cluster <- makeCluster(nworkers)
  registerDoParallel(cluster)
  start1.time <- Sys.time()
  for (src.basename in src.basenames) {
    name.subject <<- src.basename
    #datos =read.table(paste("C:\\Users\\Feffyta\\Documents\\Universidad\\tesis\\Programas\\Programa escalon Matlab\\Prueba escalon ARX\\Parametros\\TEC\\ARX\\",src.basename,".txt",sep=""),header = T, sep=" ")
    cat("instance ", src.basename, "\n")
    process.parallel(
      src.dir = src.dir,
      tgt.dir = tgt.dir,
      tgt2.dir = tgt2.dir,
      src.ext = src.ext,
      tgt.ext = tgt.ext,
      parametros = parametros,
      lags = lags,
      keep.nstats = 2000,
      src.basename = src.basename,
      export.names = export.names,
      export.packages = export.packages
    )  
    
    
    
  }
  end1.time <-Sys.time()
  print(end1.time-start1.time)
  stopCluster(cluster)
}
