## ---- include = FALSE----------------------------------------------------
# set figure sizes
knitr::opts_chunk$set(fig.width = 7, fig.height = 5) 

## -----------------------------------------------------------------------------------
library(hedgehog);
library(survival);

# using example from survival package
km.plot(
    Surv(futime, death) ~ sex, 
    mgus
    );

## -----------------------------------------------------------------------------------
km.plot(
    Surv(futime, death) ~ sex, 
    mgus,
    show.risk.table = TRUE
    );

## -----------------------------------------------------------------------------------
test.data <- data.frame(
    x = sample(letters[1:3], 100, replace = TRUE),
    y = rnorm(100)
    );

stripboxplot(y ~ x, test.data);

## -----------------------------------------------------------------------------------
stripboxplot(
    y ~ x, 
    test.data,
    points.col = c('darkgreen', 'orange', 'firebrick')
    );

## -----------------------------------------------------------------------------------
stripboxplot(
    y ~ x, 
    test.data,
    points.col = c('darkgreen', 'orange', 'firebrick'),
    group.names = c('Y', 'A', 'Y')
    );

## ---- fig.width = 5-----------------------------------------------------------------
show.colour.palette( get.colour.palette(7) );

## ---- fig.width = 5-----------------------------------------------------------------
show.colour.palette( colours(7) );

