# Cleaned Soybean data:
Source: Soybean Disease Dataset
Source: UCI Machine Learning Repository

Package: "mlbench"; dataset name: "Soybean"
This data are cleaned using this command: 
if (sum(is.na(Soybean)) > 0) {
+     imputed_data <- mice(Soybean, method = 'pmm', m = 5, seed = 500, silent=TRUE, printFlag = FALSE)
+     completeData <- complete(imputed_data, 1)  # using only the first completed dataset
+     trainSet <- completeData
+ }
