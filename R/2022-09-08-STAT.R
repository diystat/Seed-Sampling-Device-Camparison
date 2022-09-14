## Reproduce results in the paper submitted to Stat Special Issue on
## "Statistical Consulting and Collaboration"
##
## 2022-09-14
##

source("test.equal.proportion.R");

## Load the data frame
dd = "2021-08-10";
file.out = sprintf("../data/%s.seed.df.Rdata", dd);
load(file.out);


## Test equal proportions for all runs 
##
## Note that in the data set, we actually had five rounds. Round 1 was a test
## run and was not included in the analysis reported in the paper.

main.1 = function() {
  p = matrix(NA, 9, 15);
  t = matrix(NA, 9, 15);
  n = matrix(NA, 9, 15);
  
  cn = rep("", 15)
  j = 0;
  for (pid in 1:5) {
    for (bid in 1:3) {
      j = j + 1;
      cn[j] = sprintf("P%dB%d", pid, bid);
      for (d in 1:9) {
        ## b = 1; d = 3;
        s = df$Plan == pid & df$Batch == bid & df$Device==d; 
        if (any(s) & sum(s)>1) {
          ys = df[s,]
          ## t.ep = test.equal.proportion(ys$Weight, ys$Number);
          t.ep = test.equal.proportion(ys$Number, ys$Weight);
          n[d, j] = sum(s);
          t[d, j] = t.ep$statistic;
          p[d, j] = t.ep$p.value;
        }
      }
    }
  }
  
   
  colnames(p) = cn;
  print(round(p * 1000)/1000);
  
  file.csv = sprintf("../output/test.prop.%s.csv", Sys.Date());
  write.csv(round(p*1000)/1000, file=file.csv);
  
}

## Interaction plots 
plot.errors = function(d, subsample=4) {
  if (d == 6) {
    dd = df[df$Device==d & df$Plan>1,]; 
  } else {
    dd = df[df$Device==d & df$Subsample==subsample & df$Plan>1,]; 
  }
  
  n.hat = dd$Number * dd$TotalWeight / dd$Weight;
  print(all.equal(dd$Estimate, n.hat));
  
  Round = dd$Plan - 1;
  
  interaction.plot(dd$Placement, Round, n.hat - dd$TotalNumber, 
                   ylim=c(-80,80), 
                   main = sprintf("Device %d", d),
                   type="b",
                   pch =19,
                   xlab="Weed Placement", ylab="Estimation Error");
  abline(h=c(-50, 50), lty=2)
  
  invisible(n.hat)
}


## Simulate MSE
simulate.MSE = function(N=1e5) {
  n.hat = rpois(12*N, 10) * 10;
  dim(n.hat) = c(N, 12);
  MSE = rowMeans((n.hat - 100)^2);
  hist(MSE, main="Histogram of MSE from Perfectly Random Subsamples");
  invisible(MSE);
}


plot.simulated.errors = function(seed) {
  set.seed(seed)
  ## Extract data for device 6
  d = 6;
  dd = df[df$Device==d & df$Plan>1,]; 
  
  ## 
  n.hat = rpois(12, 10) * 10;
  n = 100;
  
  
  Round = dd$Plan - 1;
  
  interaction.plot(dd$Placement, Round, n.hat - n,
                   ylim=c(-80,80), 
                   main = "Errors from Perfect Subsamples",
                   type="b",
                   pch =19,
                   xlab="Weed Placement", ylab="Estimation Error");
  abline(h=c(-50, 50), lty=2)
  
  invisible(n.hat)
}

estimation.error = function(d, subsample=4) {
  
  ## Extract data for device 1
  if (d == 6) {
    dd = df[df$Device==d & df$Plan>1,]; 
  } else {
    dd = df[df$Device==d & df$Subsample==subsample & df$Plan>1,]; 
  }
  
  n.hat = dd$Number * dd$TotalWeight / dd$Weight;
  y = n.hat - dd$TotalNumber;
  Bias = mean(y, na.rm=TRUE);
  MSE = mean(y^2, na.rm=TRUE);
  
  
  list(dd=dd, y=y, Bias=Bias, MSE=MSE);
}


main.2  = function() {
  
  ## Simulate Poisson quantiles
  y = rpois(1e6, 10) * 10;
  hist(y)
  quantile(y, c(0.05, 0.95))
  quantile(y, c(0.025, 0.975))
  
  ## Interaction Plots
  for (d in 1:9) {
    file.png = sprintf("../output/error.plot.device.%d.png", d)
    png(file.png)
    plot.errors(d);
    dev.off();
  }
  
  ## summarize bias and MSE
  st = matrix(NA, 2, 9); 
  colnames(st) = 1:9;
  rownames(st) = c("Bias", "MSE");
  
   
  for (d in 1:9) {
    res = estimation.error(d);
    st[1, d] = res$Bias;
    st[2, d] = res$MSE;
    
    cat(sprintf("Device %d\n", d));
  }
  
  file.MSE = sprintf("../output/%s.MSE.csv", Sys.Date());
  write.csv(st, file=file.MSE)
  
  ## Histogram of simulated MSE
  file.MSE.png = sprintf("../output/%s.MSE.png", Sys.Date());
  png(file.MSE.png);
  MSE = simulate.MSE();
  dev.off();
  print(quantile(MSE, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)))
  
  
  ## Plot simulated errors
  file.simulated.errors = sprintf("../output/%s.simulated.errors.png", Sys.Date());
  png(file.simulated.errors);
  par(mfrow=c(2,2))
  for (i in 1:4) {
    plot.simulated.errors(i);
  }
  dev.off();
  
  file.simulated.errors = sprintf("../output/%s.simulated.errors.2.png", Sys.Date());
  png(file.simulated.errors);
  par(mfrow=c(2,2))
  for (i in 5:8) {
    plot.simulated.errors(i);
  }
  dev.off();
  
  file.simulated.errors = sprintf("../output/%s.simulated.errors.3.png", Sys.Date());
  png(file.simulated.errors);
  plot.simulated.errors(3);
  dev.off();
  
 
}
