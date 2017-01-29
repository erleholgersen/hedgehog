### survival.R ####################################################################################
# Test survival functions - both plotting and non-plotting ones.

### KM plots ######################################################################################

km.plot(
  Surv(futime, fustat) ~ 1,
  ovarian
  );

### CENSORING DISTRIBUTION ########################################################################

censoring.estimates <- censoring.km.estimate(
  t = seq(100, 1500, by = 100), 
  time = ovarian$futime, 
  status = ovarian$fustat
  );