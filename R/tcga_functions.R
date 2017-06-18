#' Call copy numbers from TCGA level 3 data
#'
#' @param cnv.data  Data frame of TCGA Level 3 data
#' @param colour.code   Logical indicating if colours should be returned instead of -1, 0, 1 coding
#'
#'@export call.tcga.cnas
call.tcga.cnas <- function(cnv.data, colour.code = FALSE) {
  
    # get absolute number of copies
    copy.number <- round(2*(2^cnv.data$Segment_Mean));
    
    # convert to -1, 0, 1 coding
    cna.call <- rep(0, length(copy.number));
    cna.call[copy.number < 2] <- -1;
    cna.call[copy.number > 2] <- 1;
    
    # fix Y-chromosome
    cna.call['Y' == cnv.data$Chromosome] <- cna.call['Y' == cnv.data$Chromosome] + 1;
    cna.call['Y' == cnv.data$Chromosome & 0 == copy.number] <- -1;
    
    if(colour.code) {
        colour.dictionary <- c(
            '-1' = 'blue',
            '0' = 'white',
            '1' = 'red'
        );
        
        cna.call <- colour.dictionary[as.character(cna.call)];
    }
    
    return(cna.call);
    
}