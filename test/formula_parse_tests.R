

test.formula <- as.formula(
  test(x) ~ w + frailty(z) + z + frailty(v)
  );

model.terms <- terms(
  test.formula,
  specials = c('frailty'
  );