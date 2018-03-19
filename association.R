library(arules)
install.packages("arulesViz")
install.packages("mclust")
library(arulesViz)
library(wordcloud)
#1. Groceries Data의 특성과 품목 확인
data("Groceries")
summary(Groceries)
class(Groceries)
Groceries@itemsetInfo
Groceries@itemInfo
Groceries@data
inspect(Groceries)
#Groceries의 상품을 시각화
itemFrequencyPlot(Groceries,topN=20, type="absolute")#빈도수로로
itemFrequencyPlot(Groceries,topN=20, type="relative")#비율로

#tranasatino Data Set은 'inspect'라는 함수로 확인
inspect(Groceries[1:10])
itemFrequency(Groceries)
itemFrequency(Groceries,type ='absolute')
itemFrequency(Groceries,type ='relative')

#지지도와 신뢰도의 임계치가 0.05인 적어도 두 개의 품목에 대한 연관규칙을 연관성 분석을 수행
rules<- apriori(Groceries, parameter = list(supp= 0.05, conf =0.05, minlen = 2))
inspect(head(sort(rules, by="support", decreasing = TRUE),10))
rules

rules1<- apriori(Groceries, parameter = list(supp= 0.005, conf =0.005, minlen = 3))
inspect(head(sort(rules1, by="support", decreasing = TRUE),10))
rules1

rules2<- apriori(Groceries, parameter = list(supp= 0.005, conf =0.05, minlen = 2))
inspect(head(sort(rules2, by="support", decreasing = TRUE),10))
rules2

rules<- apriori(Groceries, parameter = list(supp= 0.005, conf =0.8))
rules
#S4object  

#6.전항이 "soda"인 규칙 중 신뢰도가 높은 5개 규칙을 확보한후 지지도, 신뢰도, 향상도를 비교 검토해 의미 있는 규칙을 선정
rules_before<-apriori(Groceries, parameter = list(supp= 0.05, conf = 0.05, minlen =2),
                      appearance = list(default="rhs", lhs ="soda"))
inspect(sort(rules_before, by="support", decreasing = TRUE))

#7.6)에서 생성된 규칙 시각화
# Available methods: ‘matrix’, ‘mosaic’, ‘doubledecker’, ‘graph’, ‘paracoord’, 
# ‘scatterplot’, ‘grouped matrix’, ‘two-key plot’, ‘matrix3D’, ‘iplots’
plot(rules, method='graph', intractive =TRUE, shading =NA)
plot(rules, method='grouped', intractive =TRUE, shading =NA)
plot(rules, method='two-key plot', intractive =TRUE, shading =NA)
plot(rules, method="grouped")
plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules1, method='graph', intractive =TRUE, shading =NA)        
plot(rules1, method="grouped")
plot(rules1, method="paracoord", control=list(reorder=TRUE))

#ridge regression, word trackter

#Word Cloud
itemLabels(rules1)
itemFrequency(Groceries)

wordcloud(words = itemLabels(Groceries), freq = itemFrequency(Groceries), min.freq = 1, scale = c(3, 0.2), col = brewer.pal(9, "Blues")
 , random.order = FALSE)

#package(recommender)

install.packages("recommender")
#순차분석
#wordVec