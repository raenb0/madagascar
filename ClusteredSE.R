library(plm)
library(clubSandwich)

model1 <- plm(formula = defor ~ CFM * time + pop + edge, data = w.matched.reorg, effect = "twoways", model = "within", index = c("UID", "time")) # plm model

V_CR2 <-vcovCR(model1,cluster=w.matched.reorg$clusterID,type="CR2") # clustering SE. "clusterID": CFM site or PA identification code in the data
coef_test(model1, vcov=V_CR2, test="Satterthwaite") # p-values
conf_int(model1, vcov=V_CR2, test="Satterthwaite") # 95% CI. I personally prefer presenting CI because of all the controversies surrounding p-values
