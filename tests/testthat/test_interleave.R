library(eRle);

context('interleave');

test_that(
    'Equal length vectors interleave correctly', {
        
        expect_equal(
            interleave(1:2, 3:4),
            c(1, 3, 2, 4)
            );
    
        expect_equal(
            interleave(1, 2), 
            c(1, 2)
            );
    });