
# (1) Debugging--in the 3 cases below (a through c), identify the major coding error in each case and explain how to fix it, in 1-2 
# sentences. DO NOT actually copy/paste corrected code:
  
# (a) https://gist.github.com/diamonaj/2e5d5ba5226b7b9760f5d1bf1e7bf765
"""
Match should be run before MatchBalance instead of GenMatch so we can get the output (mout) and insert it in
match.out argument of MatchBalance so we can receive balance statistics for both after and before matching.
"""

# (b) https://gist.github.com/diamonaj/3b6bc83d040098486634184d99fc4c55

"""
The parameters for Match() needs to be the same of GenMatch(). To make sure the weights from GemMatch() works
we need to run Match() with the same arguments so it should also have estimand = "ATE" on Match() since 
ATT is the default value if we do not specify it would run 
"""


# (c) https://gist.github.com/diamonaj/a88cb40132ed8584e5182b585e1c84c8

"""
Its the same problem as the above, arguments to GenMatch() and Match() should be the same. 
You cannot have M=2 in only one of them, so you should add M=2 in GenMatch().
"""
# ---------------------------------------------------------------------
# (2) Replicate figure 8 in https://gking.harvard.edu/files/counterf.pdf.

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
which(is.na(foo) == TRUE)
no_interaction <- foo[c('pbs2s3','wartype', 'logcost', 'wardur', 'factnum', 'factnum2', 'trnsfcap', 'develop', 'exp', 'decade', 'treaty', 'untype4')]

for (a in names(no_interaction)){
  no_interaction[[a]] <- mean(no_interaction[[a]], na.rm = TRUE)
}

interaction <- no_interaction[1,]
no_interaction <- no_interaction[1,]
# Adding interaction term to the interaction term data
interaction['wardur*untype4'] <- interaction['wardur']*interaction['untype4']

regression <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + 
                  develop + exp + decade + treaty + untype4, 
                  data=foo, family = 'binomial')
regression_interaction <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + 
                                develop + exp + decade + treaty + untype4 + wardur*untype4, 
                              data=foo, family = 'binomial')

treatment <- data.frame(no_interaction)
treatment[['untype4']] <- 1
head(treatment)
control <- data.frame(no_interaction)
control[['untype4']] <- 0

treatment_interaction <- data.frame(interaction)
treatment_interaction[['untype4']] <- 1

control_interaction <- data.frame(interaction)
control_interaction[['untype4']] <- 0

effect <- matrix()
effect_interaction <- matrix()

i = 0
for(d in 1:315){
  
  treatment['wardur'] = d
  control['wardur'] = d
  
  treatment_interaction['wardur'] = d
  control_interaction['wardur'] = d
  
  i = i+1
  effect[i] <- predict(regression, treatment, 'response') - predict(regression, control, 'response')
  effect_interaction[i] <- predict(regression_interaction, treatment_interaction, 'response') - predict(regression_interaction, control_interaction, 'response')
}
effect
effect_interaction

# Plotting the variables and making the graph similar to figure 8
plot(effect,xlim=c(5,315),ylim=c(0,0.8),type='l',lty = 'dotted', xlab = "Duration of war in months", 
ylab="Marginal effects of UN peacebuilding operations",xaxs="i",yaxs="i",xaxt="n",yaxt="n", sub="FIG. 8. Causal Effect of Multidimensional UN Peacekeeping Operations")
lines(effect_interaction,xaxt="n",yaxt="n",xaxs="i",yaxs="i")
axis(1, xaxp=c(0,315,63))
axis(2, yaxp=c(0.0,0.8,8))
axis(3, xaxp=c(0,315,63), labels = FALSE)
axis(4, yaxp=c(0.0,0.8,8), labels = FALSE)

## Question 3
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1
# What does this mean? What is "treatment"? 
# This is making a list full of zeros but with '1's where there was intervention by the UN. Treatment is all
# the wars that have had any UN intervention to help the country achieve peace, and the control is wars that
# had no UN intervention. So we are sending wars with on interaction to control group and wars with interaction
# to treatment group.

## Question 4
  
(4) Let's pretend you work for an NGO and your manager asks you to estimate the impact of the treatment 
identified above on lenient peacebuilding success 2 years and 5 years after the war. You will have to 
search for these two outcomes variables in the codebook.

(a) In no more than 1 sentence, articulate the causal question as best you can 
(being as clear as you can about treatment and control):
What is the effect of UN intervention after 2 and 5 years after the war concluded compared to wars that had no UN
intervention if we compare wars with similar characteristics(matching) ?

(b) In no more than 1 sentence, explain how/why SUTVA might be violated here. 
In no more than 1 additional sentence, explain how you could in theory use the "restrict" argument 
(in Match()/GenMatch()) to help address this potential problem.'
# SUTVA might be violated because interference in one unit might affect other units, for example, 
# if UN successfully makes peace in one place (country) that had war with a ‘neighbor’ this will 
# also affect their ‘neighbor’ and make it less likely to engage in war again with the place that 
# now has peace, so they did not receive ‘treatment’ and more likely not engage in war again since 
# the UN intervention in another ‘place’ affected them. In this case we can use restrict to restrict 
# Matching between these 2 ‘places’ (they are neighbors and might have a lot of similar aspects, 
# so one may be matched with the other) so we ensure they are not going to be matched, increasing the 
# chance that those ‘places’ will be matched with other ‘places’ that UN intervention in one do not 
# affected the other.


#(c) Use simple logistic regression, propensity score matching, and genetic matching to try to answer these 
#questions. 
library(Matching)
set.seed(123)
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
#foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
foo <- foo[c(-4, -16, -19, -47, -84, -93, - 98), ]
which(is.na(foo) == TRUE)
head(foo)

Tr <- rep(0, length(foo$untype))
foo$untype
foo$pbs5s3
Tr[which(foo$untype != "None")] <- 1

glm_5 <- glm(pbs5l ~ Tr + wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
             family = binomial, data = foo)
glm_5$fitted
# 0.82
summary(glm_5)
# < 0.00010717 
MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade, nboots=500)

glm_2 <- glm(pbs2l ~ Tr + wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
            family = binomial, data = foo)
# 0.59
summary(glm_2)
# < 0.00010717
MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade, nboots=500)

# ----------
glm_pscore <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade,
             family = binomial, data = foo)

mout_5 <- Match(Tr = Tr, X = glm_pscore$fitted, Y = foo$pbs5l, BiasAdjust = TRUE)
mb_5  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade, match.out = mout_5, nboots=500)

summary(mout_5)
mout_5$est
# 0.39
mout_5$est.noadj
# 0.39

# Before Matching Minimum p.value: 0.00010717 
# Variable Name(s): foo$logcost  Number(s): 2 
#
# After Matching Minimum p.value: 0.008 
# Variable Name(s): foo$trnsfcap  Number(s): 6 

mout_2 = Match(Tr = Tr, X = glm_pscore$fitted, Y = foo$pbs2l, BiasAdjust = TRUE)
mb_2  <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + foo$trnsfcap  + foo$treaty + foo$develop + foo$exp +foo$decade,match.out = mout_2, nboots=500)

summary(mout_2)
mout_2$est
#0.36
mout_2$est.noadj
#0.36

# Before Matching Minimum p.value: 0.00010717 
# Variable Name(s): foo$logcost  Number(s): 2 
#
# After Matching Minimum p.value: 0.01 
# Variable Name(s): foo$trnsfcap  Number(s): 6 

# -------------------------------------

BalanceMat <- cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$treaty, foo$develop,foo$decade)

gen5 <- GenMatch(Tr=Tr, X=BalanceMat, estimand = "ATT", M=1,pop.size=200, max.generations = 25, wait.generations = 10)

mgen5 <- Match(Tr=Tr, X=BalanceMat, Y=foo$pbs5l, Weight.matrix=gen5, estimand= "ATT", M=1, BiasAdjust = TRUE)
summary(mgen5)
mgen5$est
mgen5$est.noadj
mout_gen5 <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + 
                         foo$trnsfcap  + foo$treaty + foo$develop + foo$exp + foo$decade, 
                         match.out = mgen5, nboots=500)


gen2 <- GenMatch(Tr=Tr, X=BalanceMat, estimand = "ATT", M=1,pop.size=200, max.generations = 25, wait.generations = 10)
mgen2 <- Match(Tr=Tr, X=BalanceMat, Y=foo$pbs2l, Weight.matrix=gen2, estimand= "ATT", M=1, BiasAdjust = TRUE)
summary(mgen2)
mgen2$est
mgen2$est.noadj
mout_gen2 <- MatchBalance(Tr ~ foo$wartype + foo$logcost + foo$wardur + foo$factnum + foo$factnum2 + 
                           foo$trnsfcap  + foo$treaty + foo$develop + foo$exp + foo$decade, 
                         match.out = mgen2, nboots=500)

