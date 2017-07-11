read_DwCA <- function(path, pathOnly = FALSE, simple = False, verbose = FALSE){
    ptmReadDirectory <- proc.time()
    fileWithReadDirectory <- dwca_read(path, read = !pathOnly)
    proc.time() - ptmReadDirectory
    fileWithReadDirectory
}
