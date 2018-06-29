#|-> checking versions of packages (and installing if needed)
packages = matrix(c(
  "lavaan",  "0.6-1",
  "mirt",    "1.28",
  "GPArotation", "2014.11-1",
  "polycor", "0.7-9",
  "ggplot2", "2.2.1"),
  nrow = 2)
for (i in 1:ncol(packages)) {
  version = tryCatch(packageVersion(packages[1, i]),
                     error = function(x){return(0)})
  if (version < packages[2, i]) {
    install.packages(packages[1, i], repos = "https://cloud.r-project.org")
  }
}
#|<-
#|-> let's find out if it works
library(mirt)
data = expand.table(LSAT7)
mm = mirt(data, 1)
summary(mm)
#
#           F1    h2
# Item.1 0.502 0.252
# Item.2 0.536 0.287
# Item.3 0.708 0.501
# Item.4 0.410 0.168
# Item.5 0.397 0.157
#
# SS loadings:  1.366
# Proportion Var:  0.273
#
# Factor correlations:
#
#    F1
# F1  1
library(lavaan)
dataLavaan = as.data.frame(lapply(data, ordered))
model = '
F1 =~ NA*Item.1 + Item.2 + Item.3 + Item.4 + Item.5
F1 ~~ 1*F1'
ml = cfa(model, data = dataLavaan)
summary(ml)
# lavaan (0.6-1) converged normally after  18 iterations
#
# Number of observations                          1000
#
# Estimator                                       DWLS      Robust
# Model Fit Test Statistic                       9.131      11.677
# Degrees of freedom                                 5           5
# P-value (Chi-square)                           0.104       0.039
# Scaling correction factor                                  0.785
# Shift parameter                                            0.041
# for simple second-order correction (Mplus variant)
#
# Parameter Estimates:
#
#   Information                                 Expected
# Information saturated (h1) model        Unstructured
# Standard Errors                           Robust.sem
#
# Latent Variables:
#                  Estimate  Std.Err  z-value  P(>|z|)
# F1 =~
#   Item.1            0.506    0.064    7.962    0.000
#   Item.2            0.531    0.060    8.835    0.000
#   Item.3            0.699    0.064   10.957    0.000
#   Item.4            0.432    0.056    7.780    0.000
#   Item.5            0.384    0.066    5.824    0.000
#
# Intercepts:
#                  Estimate  Std.Err  z-value  P(>|z|)
#   .Item.1           0.000
#   .Item.2           0.000
#   .Item.3           0.000
#   .Item.4           0.000
#   .Item.5           0.000
#   F1                0.000
#
# Thresholds:
#                  Estimate  Std.Err  z-value  P(>|z|)
#   Item.1|t1        -0.946    0.047  -20.206    0.000
#   Item.2|t1        -0.407    0.041   -9.959    0.000
#   Item.3|t1        -0.745    0.044  -16.969    0.000
#   Item.4|t1        -0.269    0.040   -6.693    0.000
#   Item.5|t1        -1.007    0.048  -21.021    0.000
#
# Variances:
#                  Estimate  Std.Err  z-value  P(>|z|)
#   F1                1.000
#   .Item.1           0.743
#   .Item.2           0.718
#   .Item.3           0.511
#   .Item.4           0.813
#   .Item.5           0.852
#
# Scales y*:
#                  Estimate  Std.Err  z-value  P(>|z|)
#   Item.1            1.000
#   Item.2            1.000
#   Item.3            1.000
#   Item.4            1.000
#   Item.5            1.000
library(polycor)
corMat = hetcor(dataLavaan)
mf = factanal(covmat = corMat$correlations, factors = 1, n.obs = nrow(data))
print(mf)
#
# Call:
#   factanal(factors = 1, covmat = corMat$correlations, n.obs = nrow(data))
#
# Uniquenesses:
#   Item.1 Item.2 Item.3 Item.4 Item.5
# 0.764  0.704  0.519  0.815  0.852
#
# Loadings:
#        Factor1
# Item.1 0.486
# Item.2 0.544
# Item.3 0.694
# Item.4 0.430
# Item.5 0.385
#
# Factor1
# SS loadings      1.346
# Proportion Var   0.269
#
# Test of the hypothesis that 1 factor is sufficient.
# The chi square statistic is 53.59 on 5 degrees of freedom.
# The p-value is 2.55e-10
#|<-

