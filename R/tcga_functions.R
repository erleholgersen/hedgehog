#' Convert vector of -1, 0, 1 CNA calls to vector of colours
#' 
#' @param cna.callss vector of -1, 0, 1 coded CNAs
#' @param loss.col colour of losses, defaults to 'blue'
#' @param gain.col colour of gains, defaults to 'red'
#' @param neutral.col colour of neutral regions, defaults to 'white'
#' 
#' @return vector of colours
#' 
#' @export colour.code.cnas
colour.code.cnas <- function(cna.calls, loss.col = 'blue', gain.col = 'red', neutral.col = 'white') {
   
    colour.dictionary <- c(
        '-1' = loss.col,
        '0' = neutral.col,
        '1' = gain.col
        );
    
    colours <- colour.dictionary[as.character(cna.calls)];
    
    return(colours);
}

#' Call copy numbers from TCGA level 3 data
#'
#' @param cnv.data  Data frame of TCGA Level 3 data
#' @param colour.code   Logical indicating if colours should be returned instead of -1, 0, 1 coding
#'
#' @return cna.callss vector of CNA calls
#' 
#' @export call.tcga.cnas
call.tcga.cnas <- function(cnv.data, colour.code = FALSE) {
  
    # get absolute number of copies
    copy.numbers <- round(2*(2^cnv.data$Segment_Mean));
    
    # convert to -1, 0, 1 coding
    cna.calls <- rep(0, length(copy.numbers));
    cna.calls[copy.numbers < 2] <- -1;
    cna.calls[copy.numbers > 2] <- 1;
    
    # fix Y-chromosome
    cna.calls['Y' == cnv.data$Chromosome] <- cna.calls['Y' == cnv.data$Chromosome] + 1;
    cna.calls['Y' == cnv.data$Chromosome & 0 == copy.numbers] <- -1;
    
    if(colour.code) {
        cna.calls <- colour.code.cnas(cna.calls);
    }
    
    return(cna.calls);
    
}