library(data.table)#대용량 데이터 처리용, 처리 속도가 data.frame에 비해 몇 십배 빠름
library(arules)#연관성 분석 패키지
library(datasets)
library(dplyr)#데이터 핸들링 패키지
library(arulesViz)#연관성 분석 시각화 함수

#대용량 패키지 :data.table, dplyr, tidyr
#지지도(support, P(A∩B), A와 B가 일어난 회수/ 전체일어난 회수
#신뢰도(confidence, P(A∩B) / P(A), A와 B가 일어난 회수 / A가 일어난 회수)
#향상도(Lift, P(A∩B) / P(A)*P(B)  = P (B|A) / P (B), A와 B가 동시에 일어난 회수 / A, B가 독립적인 사건일 때 A,B가 동시에 일어날 확률)
#options(digits = 2, scipen = 100) #100자리 구분, 끝에서 두자리 표시

#1. 데이터 불러오기
#kaggle https://www.kaggle.com/psparks/instacart-market-basket-analysis/data Instacart Market Basket Analysis

aisles<-read.csv("aisles.csv")
dept<-read.csv("departments.csv")
products<-read.csv("products.csv")
orders<-fread("order_products__prior.csv") #data table로 불러올 때는 fread 함수 사용
head(orders, 20)

#2. 상품 정보에 색션, 카테고리 정보를 병합
str(aisles)
str(dept)
str(products)

TolData<-merge(merge(products, aisles, all.x = T, by= "aisle_id"), dept, all.x= T, by="department_id")
head(TolData)

#3. 상품명과 카테고리명을 통합해 가장 많이 구매하는 상품정보를 확인
TolData<-as.data.table(TolData) #데이터 프레임을 데이터 테이블로 변환
setkey(TolData,"product_id")
setkey(orders,"product_id")
Data_master<-orders[TolData, nomatch=0]
Data_master[3300:3350]
tail(Data_master)

Data_master[,.(.N),by="aisle"][order(-N)] #.N은 카운트 함수, 앞의 .은 구분자
Data_master[,.(order_no=mean(reordered),.N),by="aisle"]


#4. 연관 분석을 할 수 있도록 transaction 실시
Data_master1<-Data_master[1:100000] #용량 문제로 10000개의 샘플 데이터 추출해서 분석
Data_master2<-split(Data_master1$aisle, Data_master1$order_id)
Data_master_trans<-as(Data_master2,"transactions")
a<-as(Data_master_trans,"data.frame")
inspect(Data_master_trans)
ecl <- eclat(Data_master_trans, parameter=list(support=2/97708,minlen=2 ,maxlen=10))#support, cofiedence, lift, 거래 품목 수 설정을 위한 확인 작업 
ecl# 2회 이상 거래된 2개 품목 이상 10개 품목 미만의 거래 횟수 246 
inspect(sort(ecl)[1:50])#fresh fruits, fresh vegetables의 구매 횟수가 141이고 지지도는 0.001로 나타났고 2위부터는 0.0007미만으로 낮게 나타남


#5. Data의 특성과 품목 확인
summary(Data_master_trans)
class(Data_master_trans)
Data_master_trans@itemInfo #26개 품목
Data_master_trans@itemsetInfo #9982개 값
Data_master_trans@data
inspect(Data_master_trans)

#6.품목 빈도수 확인
itemFrequency(Data_master_trans)
itemFrequency(Data_master_trans,type ='absolute')# 품목을 빈도수로 표시
itemFrequency(Data_master_trans,type ='relative')# 품목을 비율로 표시


#7. 시각화
itemFrequencyPlot(Data_master_trans,topN=30, type="absolute")#빈도수로로,topN 함수 사용하면 많은 품목순으로 왼쪽부터 나타남
itemFrequencyPlot(Data_master_trans, type="absolute")#빈도수로로
itemFrequencyPlot(Data_master_trans,topN=30, type="relative")#비율로

#8.연관 관계 탐색: support,confident, 장바구니 상품 개수 등을 조절하며 연관성 탐색

# apriori(data, parameter=list(support=0.1, confidence=0.8, minlen=1, maxlen=10, smax=1)) default 설정 값
# support=최소지지도, confidence=최소신뢰도, minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대지지도

rules<- apriori(Data_master_trans, parameter = list(supp= 0.00005, conf =0.00005, minlen = 2))
inspect(head(sort(rules, by="support", decreasing = TRUE),10))
rules#216개의 규칙 생성
rules1<- apriori(Data_master_trans, parameter = list(supp= 0.0005, conf =0.00005, minlen = 2))
rules1 # 14개의 규칙 생성
inspect(head(sort(rules1, by="support", decreasing = TRUE),15))

#9. 연관성 시각화
plot(rules1)
plot(rules1, method = "grouped") #원 크기가 지지도, 원 색이 향상도 표시
plot(rules1, method = "graph", engine='interactive') #동적으로 연관성 표시
plot(rules1, method = "graph", engine='htmlwidget') #품목 선택 가능하여 품목별로 연관성 개별 확인 가능
  
