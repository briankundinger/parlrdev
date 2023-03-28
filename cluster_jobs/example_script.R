library(RecordLinkage)
library(fastLink)

data <- RLdata500
cd<- compare.dedup(data, identity = identity.RLdata500)
weights <- emWeights(cd)
out <- emClassify(weights, my = .00001)
results <- getErrorMeasures(out)
saveRDS(results, file = "test_result")
results
thing <- readRDS("test_result")
