library(tidyverse)
library(ggplot2)
library(summarytools)

#Import & misc cleaning ----
data <- read_csv("Speed Dating Data copy.csv")
#aight, getting rid of rows where it's match OG-OG, get rid of waves 6-9 due to severe methodological differences
data <- data %>% filter(ageDiff <= 18) %>% filter(!between(wave, 6, 9))
#create ImpSameRace which weights whether two parners are of the same race by how important the OG determines it to be
data <- data %>% mutate(impSameRace = (imprace * .1) * samerace)
#create optimism, which is their expectation of how happy they'll be (out of 10) by how popular they'll be
data <-data %>% mutate(optimism = (exphappy * .1) * expnum)

#weighted attractions generation ----
#weighted attractions - essentially, take the "attr_o" set of charactersitics, and "attr" set of characteristics, 
#which are weighted by out of 100, and then put them as a decimal

#For Other Partner's preferences
data[, 10:15] <- data[, 10:15] / 100 
#For Other partner's perspective of the OG
data[, 36:41] <- data[, 36:41] / 100

#create the weighted attributes of the perspective of the other onto the OG
data <- data %>% mutate(weightAttr_o = pf_o_att * attr_o,
                weightSinc_o = pf_o_sin * sinc_o,
                weightIntel_o = pf_o_int * intel_o,
                weightFun_o = pf_o_fun * fun_o,
                weightAmb_o = pf_o_amb * amb_o,
                weightShar_o = pf_o_sha * shar_o) %>%
  mutate(sumWeights_o = weightAmb_o + weightAttr_o + weightSinc_o + weightIntel_o + weightFun_o + weightShar_o) %>%
  mutate(diffWeightLike_o = abs(sumWeights_o - like_o))

#create the weighted attributes of the perspective of the OG onto the other
data <- data %>% mutate(weightAttr = attr * attr1_2,
                weightSinc = sinc * sinc1_2,
                weightIntel = intel * intel1_2,
                weightFun = fun * fun1_2,
                weightAmb = amb * amb1_2,
                weightShar = shar * shar1_2) %>%
  mutate(sumWeights = weightAttr + weightSinc + weightIntel + weightFun + weightAmb + weightShar) %>%
  mutate(diffWeightLike = abs(sumWeights - like))

#match statistics generation --
data <- data %>% mutate(weightGap = abs(sumWeights - sumWeights_o),
                likeGap = abs(like - like_o)) #can add for all attributes / let's see how this goes though

#making a dictionary of distributions of certain bits of quant information ----
distList <- list()
appealSet <- distinct(.data = data[, 36:41]) %>% filter(!is.na(attr1_2)) #pull the individual people
distList[["appealDist"]] <- summary(appealSet) #attraction is the median/mean highest wanted, ambition is mean/median lowest
distList[["perceptionGapDist"]] <- data %>% filter(!is.na(weightGap)) %>% select("diffWeightLike") %>% summary()
distList[["weightGapDist"]] <- data %>% filter(!is.na(weightGap)) %>% select("weightGap") %>% summary()
distList[["likeGapDist"]] <- data %>% filter(!is.na(likeGap)) %>% select("likeGap") %>% summary()
distList[["ageDifferencesDist"]] <- summary(data$ageDiff)
distList[["optimismDist"]] <- summary(data$optimism)

#determine average like/weight gaps with matched individuals, average optimism ----
#weight gap distributions by match
weightGapMat <- data %>% filter(!is.na(weightGap) & (match == 1)) %>% select(weightGap) %>% summary() 
weightGapNooMat <- data %>% filter(!is.na(weightGap) & (match == 0)) %>% select(weightGap) %>% summary()
#like gaps distributions by match
likeGapMat <- data %>% filter(!is.na(likeGap) & (match == 1)) %>% select(likeGap) %>% summary() 
likeGapNoMat <- data %>% filter(!is.na(likeGap) & (match == 0)) %>% select(likeGap) %>% summary()
#optimism distribution overall
data %>% filter(!is.na(optimism)) %>% select(optimism) %>% summary()
#probit model: Whether an individual likes the other ---- 
decData <- data %>% select("dec", "sumWeights", "int_corr", "like", "impSameRace", "optimism")
decProbitWOLike <- decData %>% filter(!is.na(dec) & !is.na(sumWeights) &
                                  !is.na(int_corr) & !is.na(impSameRace) & !is.na(optimism)) %>%
  glm(data = ., dec ~ sumWeights + int_corr + impSameRace + optimism,
      family = binomial(link = "probit"))
decProbitWLike <- decData %>% filter(!is.na(dec) & !is.na(sumWeights) &!is.na(like) &
                                        !is.na(int_corr) & !is.na(impSameRace) & !is.na(optimism)) %>%
  glm(data = ., dec ~ sumWeights + like + int_corr + impSameRace + optimism,
      family = binomial(link = "probit"))
#summary(decProbit) #it's all about attitude

#probit model: Whether a pair becomes a match ---- 
matchData <- data %>% select("match", "int_corr", "weightGap", "likeGap", "samerace", "ageDiff")
matchProbit <- matchData %>% filter(!is.na(match) & !is.na(weightGap) & 
                                  !is.na(int_corr) & !is.na(likeGap) & !is.na(samerace) & !is.na(ageDiff)) %>%
  glm(data = ., match ~ weightGap + int_corr + likeGap + samerace + ageDiff,
      family = binomial(link = "probit"))
#summary(matchProbit) #we probably don't have as much awareness for our types as we think we do