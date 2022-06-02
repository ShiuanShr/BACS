# 20220530 Course Implementation

# install.packages("seminr")
library(seminr)
df <- read.csv('security_data_sem.csv', header = T)
colnames(df)


measurements1 <- constructs(
    composite("TRUST", multi_items("TRST", 1:4)),
    composite("SEC", multi_items("PSEC", 1:4)),
    composite("REP", multi_items("PREP", 1:4)),
    composite("INV", multi_items("PINV", 1:3)),
    composite("POL", multi_items("PPSS", 1:3)),
    composite("FAML", single_item("FAML1")),
    # composite("FAML", "FAML1"), ## 等同上一段程式碼
    interaction_term(iv = "REP", moderator = "POL", method =  orthogonal))

# 若欄位名稱叫做TRUST_a, TRUST_b, TRUST_c, 請用指令letters如下 

multi_items("TRUST_", letters[1:3])

# Describe the structural model of causal relationships between constructs (and interaction terms):

structure1 <- relationships(
    paths(from = c("REP", "INV", "POL", "FAML",  "REP*POL"), to = "SEC"),
    paths(from = "SEC", to = "TRUST")
)


plot(structure1) # 也可以單獨plot 結構模型

## 若有1個DV  TRUST, 1個 mediator SEC ?


# 1. 方法1: 不優
structure_Demo <- relationships(
    paths(from = c("REP", "INV", "POL", "FAML",  "REP*POL"), to = "SEC"),
    paths(from = c("REP", "INV", "POL", "FAML",  "REP*POL"), to = "TRUST"),
    paths(from = "SEC", to = "TRUST")
)

plot(structure_Demo) 
# 1. 方法2:  Cleaner
structure_Demo <- relationships(
    paths(from = c("REP", "INV", "POL", "FAML",  "REP*POL"), to = c("SEC", 'TRUST')),
    paths(from = "SEC", to = "TRUST")
)

plot(structure_Demo) 

pls_model1 <- estimate_pls(data = df, 
                           measurement_model = measurements1, 
                           structural_model = structure1)
plot(pls_model1)

#####---------------
report1 <-summary(pls_model1)
report1$weights
report1$vif_antecedents



# Bootstrapped
# Bootstrapped path coefficients: t-values, 95% CI
# use 1000 bootstraps and utilize 2 parallel cores
boot_pls <- bootstrap_model(seminr_model = pls_model1, nboot = 1000) # 正式研究，資料足夠，請盡量用10000以上

summary(boot_pls)


plot(boot_pls) # boot完成後仍然可以plot，會得到相當detailed的圖

#### Q2:Common-Factor Models using CB-SEM


#### a. Create a common factor model using SEMinR, with the following characteristics:


#- i. Either use the as.reflective() function to convert your earlier measurement model to being entirely reflective.



meansurement2_cf <- as.reflective(measurements1)



#- ii. Use the same structural model as before (you can just reuse it again!)

#*Ans:*
    
    
    


structural_1 <-relationships(
    paths(from = c("REP", "INV", "POL","FAML", "REP*POL"), to = "SEC"),
    paths(from= "SEC",to= "TRUST"))


cbsem_model <-estimate_cbsem(data=df,
                             measurement_model = meansurement2_cf,
                             structural_model = structural_1)


plot(cbsem_model)

summary(cbsem_model)

```

