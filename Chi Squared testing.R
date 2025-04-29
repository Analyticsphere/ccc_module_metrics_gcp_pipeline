# letter_data <- data.frame(Consent_Answer=c("Consented", "Did Not Consent"),
#                           Letter=c("A", "B", "C", "D", "X"))
# print(letter_data)
# 
# letter_data$Consent_Answer <- factor(letter_data$Consent_Answer, levels=c("Consented", "Did Not Consent"))
# letter_data$Letter <- factor(letter_data$Letter, levels=c("A", "B", "C", "D", "X"))




### All Letters with consent data -- if I did it correctly, no significance 

letter_data <- data.frame(matrix(ncol=5, nrow=2))
colnames(letter_data) <- c("A", "B", "C", "D", "X")
rownames(letter_data) <- c("Consented", "Did Not Consent")

letter_data[1,] <- c(218, 242, 271, 230, 244)
letter_data[2,] <- c(6370, 7674, 7735, 6861, 7153)

218/6588
0.03309047
242/7916
0.030571
271/8006
0.03384961
230/7091
0.03243548
244/7397
0.03298635

chisq.test(letter_data)
data:  letter_data
X-squared = 1.5214, df = 4, p-value = 0.8228


# ## may not be able to do it this way since the number of participants is not the same for each letter
# letter <- data.frame(matrix(ncol=5, nrow=8006))
# letter$A <- c(rep(1,218), rep(0,6370), rep(NA, 8006-6588))
# letter$B <- c(rep(1,242), rep(0,7674), rep(NA, 8006-7916))
# letter$C <- c(rep(1,271), rep(0,7735), rep(NA, 8006-8006))
# letter$D <- c(rep(1,230), rep(0,6861), rep(NA, 8006-7091))
# letter$X <- c(rep(1,244), rep(0,7153), rep(NA, 8006-7397))
# 
# library(tidyr)
# letter_long <- gather(letter, key = "Letter", value = "Count")
# 
# # Convert 'Count' to a factor with appropriate labels
# letter_long$Count <- factor(letter_long$Count, levels = c(0, 1), labels = c("Did Not Consent", "Consented"))
# 
# # Perform chi-squared test
# chisq_test <- chisq.test(table(letter_long$Letter, letter_long$Count))
# print(chisq_test)
# 
# 
# 
# LetterData<-data.frame(read.csv('...\\Titanic.csv',header=T,sep=','))
# 
# LetterData$A<-factor(LetterData$A,c(0,1),labels=c("Did Not Consent", "Consented"))
# LetterData$B<-factor(LetterData$B,c(0,1),labels=c("Did Not Consent", "Consented"))
# LetterData$C<-factor(LetterData$C,c(0,1),labels=c("Did Not Consent", "Consented"))
# LetterData$D<-factor(LetterData$D,c(0,1),labels=c("Did Not Consent", "Consented"))
# LetterData$X<-factor(LetterData$X,c(0,1),labels=c("Did Not Consent", "Consented"))
# 
# # cross <- table(LetterData$A,LetterData$B)
# # addmargins(cross)
# 
# 
# #Error in chisq.test(LetterData) :  all entries of 'x' must be nonnegative and finite
# 
# chisq.test(LetterData)




# cols <- c("A", "B", "C", "D", "X")
# letter_data[,cols] <- lapply(letter_data[,cols], factor)
# sapply(letter_data, class)
# 
# 
# 
# ## doesn't work with 5 variables?
# # chisq.test(letter_data$A, letter_data$B, letter_data$C, letter_data$D, letter_data$X)
# 
# 
# 
# ## Says all p values are 1, don't think thats right
# CHIS <- lapply(letter_data[,-1], function(x) chisq.test(letter_data[,1], x)); CHIS
# do.call(rbind, CHIS)[,c(1,3)]
# 
# 





### All Letters with refusal data -- if I did it right, strong significance. Signicant but NOT meaningful with slight differences 

letter_data <- data.frame(matrix(ncol=5, nrow=2))
colnames(letter_data) <- c("A", "B", "C", "D", "X")
rownames(letter_data) <- c("Refused", "Did Not Refuse")

letter_data[1,] <- c(873, 1144, 1156, 955, 1034)
letter_data[2,] <- c(5715, 5444, 5432, 5633, 5554)


chisq.test(letter_data)
data:  letter_data
X-squared = 67.921, df = 4, p-value = 6.232e-14

873/6588
1144/7916































### Logistic regression Model 1 for Consent-- none of the letters are statistically significant 
# LetterData <- data.frame(matrix(ncol=5, nrow=8006))
# LetterData$A <- c(rep(1,218), rep(0,6370), rep(NA, 8006-6588))
# LetterData$B <- c(rep(1,242), rep(0,7674), rep(NA, 8006-7916))
# LetterData$C <- c(rep(1,271), rep(0,7735), rep(NA, 8006-8006))
# LetterData$D <- c(rep(1,230), rep(0,6861), rep(NA, 8006-7091))
# LetterData$X <- c(rep(1,244), rep(0,7153), rep(NA, 8006-7397))

LetterData <- read.csv("~/GitHub/ccc_module_metrics_gcp_pipeline/LetterData.csv")

library(tidyr)
library(ggplot2)
LetterData_long <- pivot_longer(LetterData, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogit <- glm(cons ~ letter_name, data = LetterData_long, family = "binomial")
summary(mylogit)


# Call:
#   glm(formula = cons ~ letter_name, family = "binomial", data = LetterData_long)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.374860   0.068878 -48.998   <2e-16 ***
#   letter_nameB -0.081796   0.094903  -0.862    0.389    
# letter_nameC  0.023468   0.092539   0.254    0.800    
# letter_nameD -0.020669   0.096113  -0.215    0.830    
# letter_nameX -0.003259   0.094775  -0.034    0.973    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 10623  on 36997  degrees of freedom
# Residual deviance: 10622  on 36993  degrees of freedom
# (3032 observations deleted due to missingness)
# AIC: 10632
# 

# Number of Fisher Scoring iterations: 6
odds_ratios <- exp(coef(mylogit))

# View odds ratios- relatively the same, C slightly higher 
odds_ratios
# (Intercept) letter_nameB letter_nameC letter_nameD letter_nameX 
# 0.03422292   0.92146005   1.02374528   0.97954266   0.99674609 

#Logistic Model- not really helpful, only hsowing points at 1 and 0 with no curve
ggplot(LetterData_long, aes(x=letter_name, y=cons)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial))

# Predict probabilities using the fitted model
LetterData_long$predicted_prob <- predict(mylogit, newdata = LetterData_long, type = "response")

# Plot predicted probabilities - basically the same as below
ggplot(LetterData_long, aes(x = letter_name, y = predicted_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = predicted_prob - 1.96 * sqrt(predicted_prob * (1 - predicted_prob)),
                    ymax = predicted_prob + 1.96 * sqrt(predicted_prob * (1 - predicted_prob))), width = 0.2) +
  geom_line() +
  labs(x = "Letter Variant", y = "Predicted Probability of Consent") +
  ggtitle("Predicted Probability of Consent by Letter Variant") +
  theme_minimal()





# #Same as above, just confirming the structure of the data was ok
# LetterData_long2 <- data.frame(matrix(ncol=2, nrow=36998))
# LetterData_long2$letter <- c(rep("A",6588), rep("B",7916), rep("C", 8006), rep("D", 7091), rep("X", 7397))
# LetterData_long2$cons <- c(rep(1,218), rep(0,6370), 
#                            rep(1,242), rep(0,7674), 
#                            rep(1,271), rep(0,7735), 
#                            rep(1,230), rep(0,6861), 
#                            rep(1,244), rep(0,7153))
# 
# logistic_model <- glm(cons ~ letter, data =LetterData_long2, family = "binomial")
# summary(logistic_model) ## Exactly the same as above
# 
# # Predict probabilities using the fitted model
# LetterData_long2$predicted_prob <- predict(logistic_model, newdata = LetterData_long2, type = "response")
# 
# # Plot predicted probabilities
# ggplot(LetterData_long2, aes(x = letter, y = predicted_prob)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = predicted_prob - 1.96 * sqrt(predicted_prob * (1 - predicted_prob)),
#                     ymax = predicted_prob + 1.96 * sqrt(predicted_prob * (1 - predicted_prob))), width = 0.2) +
#   geom_line() +
#   labs(x = "Letter Variant", y = "Predicted Probability of Consent") +
#   ggtitle("Predicted Probability of Consent by Letter Variant") +
#   theme_minimal()









#The log odds ratio consent for Letter C compared to letter B. Letter C had the highest consent, Letter B had the highest refusal, and Letter X was the constant.
letterC_consent <- (271/8006)
letterB_consent <- (242/7916)
letterX_consent <- (244/7397)

oddsraioCB= ((letterC_consent/(1-letterC_consent)))/((letterB_consent/(1-letterB_consent)))
oddsraioCB
#1.111003 


oddsraioCX= ((letterC_consent/(1-letterC_consent)))/((letterX_consent/(1-letterX_consent)))
oddsraioCX
#1.027087 <- very little difference in increased consent from best preforming letter to constant   











##Model 1 redone for active refusals instead of consented- Letter B and C had some significance 
ActiveRefusals <- read.csv("~/GitHub/ccc_module_metrics_gcp_pipeline/ActiveRefusals.csv")

library(tidyr)
ActiveRefusals_long <- pivot_longer(ActiveRefusals, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogit2 <- glm(cons ~ letter_name, data = ActiveRefusals_long, family = "binomial")
summary(mylogit2)
# Call:
#   glm(formula = cons ~ letter_name, family = "binomial", data = ActiveRefusals_long)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.87891    0.03634 -51.707   <2e-16 ***
#   letter_nameB  0.10065    0.04840   2.080   0.0376 *  
#   letter_nameC  0.09963    0.04829   2.063   0.0391 *  
#   letter_nameD  0.01870    0.05030   0.372   0.7101    
# letter_nameX  0.06185    0.04944   1.251   0.2110    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 29901  on 36997  degrees of freedom
# Residual deviance: 29894  on 36993  degrees of freedom
# (3032 observations deleted due to missingness)
# AIC: 29904
# 
# Number of Fisher Scoring iterations: 4







## Model 2- by Site. Would I need 5 different models and then combine? 

#No significance
HF <- data.frame(matrix(ncol=5, nrow=3063))
HF$A <- c(rep(1,54), rep(0,2321), rep(NA, 3063-2375))
HF$B <- c(rep(1,59), rep(0,3004), rep(NA, 3063-3063))
HF$C <- c(rep(1,76), rep(0,2978), rep(NA, 3063-3054))
HF$D <- c(rep(1,66), rep(0,2579), rep(NA, 3063-2645))
HF$X <- c(rep(1,63), rep(0,2674), rep(NA, 3063-2737))

HF_long <- pivot_longer(HF, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogitHF <- glm(cons ~ letter_name, data = HF_long, family = "binomial")
summary(mylogitHF)



#No significance
MF <- data.frame(matrix(ncol=5, nrow=454))
MF$A <- c(rep(1,18), rep(0,311), rep(NA, 454-329))
MF$B <- c(rep(1,23), rep(0,421), rep(NA, 454-444))
MF$C <- c(rep(1,17), rep(0,437), rep(NA, 454-454))
MF$D <- c(rep(1,16), rep(0,356), rep(NA, 454-372))
MF$X <- c(rep(1,14), rep(0,387), rep(NA, 454-401))

MF_long <- pivot_longer(MF, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogitMF <- glm(cons ~ letter_name, data = MF_long, family = "binomial")
summary(mylogitMF)



#Letter C has some significance 
UC <- data.frame(matrix(ncol=5, nrow=1998))
UC$A <- c(rep(1,15), rep(0,1934), rep(NA, 1998-1949))
UC$B <- c(rep(1,9), rep(0,1986), rep(NA, 1998-1995))
UC$C <- c(rep(1,4), rep(0,1992), rep(NA, 1998-1996))
UC$D <- c(rep(1,10), rep(0,1988), rep(NA, 1998-1998))
UC$X <- c(rep(1,11), rep(0,1983), rep(NA, 1998-1994))

UC_long <- pivot_longer(UC, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogitUC <- glm(cons ~ letter_name, data = UC_long, family = "binomial")
summary(mylogitUC)



#No significance 
HP <- data.frame(matrix(ncol=5, nrow=1307))
HP$A <- c(rep(1,86), rep(0,959), rep(NA, 1307-1045))
HP$B <- c(rep(1,109), rep(0,1180), rep(NA, 1307-1289))
HP$C <- c(rep(1,118), rep(0,1189), rep(NA, 1307-1307))
HP$D <- c(rep(1,104), rep(0,1053), rep(NA, 1307-1157))
HP$X <- c(rep(1,106), rep(0,1117), rep(NA, 1307-1223))

HP_long <- pivot_longer(HP, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogitHP <- glm(cons ~ letter_name, data = HP_long, family = "binomial")
summary(mylogitHP)



#No significance 
SF <- data.frame(matrix(ncol=5, nrow=1195))
SF$A <- c(rep(1,45), rep(0,845), rep(NA, 1195-890))
SF$B <- c(rep(1,42), rep(0,1083), rep(NA, 1195-1125))
SF$C <- c(rep(1,56), rep(0,1139), rep(NA, 1195-1195))
SF$D <- c(rep(1,34), rep(0,885), rep(NA, 1195-919))
SF$X <- c(rep(1,50), rep(0,982), rep(NA, 1195-1032))

SF_long <- pivot_longer(SF, cols = c(A, B, C, D, X), names_to = "letter_name", values_to = "cons")

mylogitSF <- glm(cons ~ letter_name, data = SF_long, family = "binomial")
summary(mylogitSF)






# Or one letter consent by Site? This may not be very accurat because of the vast size discrepencies 
## Every site significant to very significant 
letterA <- data.frame(matrix(ncol=5, nrow=2375))
letterA$HF <- c(rep(1,54), rep(0,2321), rep(NA, 2375-2375))
letterA$MF <- c(rep(1,18), rep(0,311), rep(NA, 2375-329))
letterA$UC <- c(rep(1,15), rep(0,1934), rep(NA, 2375-1949))
letterA$HP <- c(rep(1,86), rep(0,959), rep(NA, 2375-1045))
letterA$SF <- c(rep(1,45), rep(0,845), rep(NA, 2375-890))

letterA_long <- pivot_longer(letterA, cols = c(HF, MF, UC, HP, SF), names_to = "site_name", values_to = "let")

mylogitletterA <- glm(let ~ site_name, data = letterA_long, family = "binomial")
summary(mylogitletterA)

















