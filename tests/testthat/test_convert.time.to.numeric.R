context('convert.time.to.numeric');

test_that('Simple examples work', {
    
    expect_equal(convert.time.numeric('12:30'), 12.5);
    expect_equal(convert.time.numeric('12:45'), 12.75);
    expect_equal(convert.time.numeric('00:00'), 0);
    expect_equal(convert.time.numeric('00:03'), 0.05);
    expect_equal(convert.time.numeric('24:30'), 24.5);
    
});

test_that('Can be vectoried', {
    
    expect_equal(
        convert.time.numeric( rep('12:30', 3) ), 
        rep(12.5, 3)
        );

    expect_equal(
        convert.time.numeric( rep('00:03', 3) ), 
        rep(0.05, 3)
    );
    
    expect_equal(
        convert.time.numeric( c('1:00', '1:15', '1:30', '1:45', '2:00') ), 
        c(1, 1.25, 1.5, 1.75, 2)
    );
    
});

test_that('Gracefully handles whitespace', {
    
    expect_equal(
        convert.time.numeric('    12:30  '), 
        12.5
        );
    
    expect_equal(
        convert.time.numeric('\t\t\t12:30'), 
        12.5
        );
});

test_that('Throws error if input does not match expected format', {
    
    problem.strings <- list(
        'hello',
        c('12:30', 'hello'),
        'XX:YY',
        '123:456'
        );
    
    for( x in problem.strings ) {
        expect_error( convert.time.numeric( x ) );
    }

});

