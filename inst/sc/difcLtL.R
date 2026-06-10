library(difNLR)

# loading data
data(LearningToLearn, package = "ShinyItemAnalysis")
data <- LearningToLearn[, 87:94] # item responses from Grade 9 from subscale 6
group <- LearningToLearn$track # school track - group membership variable
match <- scale(LearningToLearn$score_6) # standardized test score from Grade 6

# detecting differential item functioning in change (DIF-C) using
# the generalized logistic regression DIF method with 3PL model
# with the same guessing parameter for both groups
# and standardized total score from Grade 6 as the matching criterion
(fit <- difNLR(
  Data = data, group = group, focal.name = "AS", model = "3PLcg",
  match = match, type = "all", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve of item 1
plot(fit, item = 1)

# estimated coefficients for item 1 with SE
coef(fit, SE = TRUE)[[1]]
