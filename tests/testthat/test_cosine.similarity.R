context('cosine.similarity');

test_that('Parallel vectors return 1', {
    
    expect_equal(
        cosine.similarity(1, 1),
        1
        );
    
    expect_equal(
        cosine.similarity(1, 10),
        1
        );
    
    expect_equal(
        cosine.similarity(c(1, 2), c(2, 4)),
        1
        );
    
});

test_that('Perpendicular vectors return 0', {

    expect_equal(
        cosine.similarity(c(1, 0), c(0, 1)),
        0
        );
    
});