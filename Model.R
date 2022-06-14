############################################################
#                                                          #
#            MODEL ORDINAL LOGISTIC REGRESSION             #
#                                                          #
############################################################

require(nnet)

# data --------------------------------------------------------------------

load("dataMinAcc.rda")
# Model -------------------------------------------------------------------


model2 <- MASS::polr(SL~BodyPart+OpPr+Experience+
                       Day, data=dataN, Hess=T)
summary(model2)


# coefficients and p-value ------------------------------------------------


coef_table2 <- coef(summary(model2))
p_mod2 <- pnorm(abs(coef_table2[, "t value"]), lower.tail = FALSE) * 2
(coef_table2 <- cbind(coef_table2, "p value" = p_mod2))

# Confidence intervals ----------------------------------------------------

(ci_model2 <- confint(model2,level=.95))

# Odd Ratio ---------------------------------------------------------------

or <- exp(cbind(OR = -coef(model2)))
