### survival.R ####################################################################################
# Test survival functions - both plotting and non-plotting ones.


survival.object <- Surv(ovarian$futime, ovarian$fustat);

### KM plots ######################################################################################

km.plot(
  Surv(futime, fustat) ~ 1,
  ovarian
  );

### CENSORING DISTRIBUTION ########################################################################

censoring.estimates <- censoring.km.estimate(
  t = seq(100, 1500, by = 100), 
  survival.object = survival.object
  );

### FINE-GRAY MODEL WEIGHTS #######################################################################

weights <- fg.model.ipcw(
  700, 
  survival.object
  );