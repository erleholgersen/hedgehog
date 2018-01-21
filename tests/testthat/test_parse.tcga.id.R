context('parse.tcga.id');

test_that('All components of website example work', {
   sample.id <- 'TCGA-02-0001-01C-01D-0182-01'; 
    
   expect_equal(
       parse.tcga.id(sample.id, field = 'project'),
       'TCGA'
       );
  
   expect_equal(
       parse.tcga.id(sample.id, field = 'tss'),
       '02'
        );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'participant'),
       '0001'
       );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'sample'),
       '01'
   );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'vial'),
       'C'
   );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'portion'),
       '01'
    );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'analyte'),
       'D'
   );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'plate'),
       '0182'
   );
   
   expect_equal(
       parse.tcga.id(sample.id, field = 'center'),
       '01'
   );

});