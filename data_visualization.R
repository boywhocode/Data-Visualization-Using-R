data_v<-read.csv("C:/Users/Abhijit Rudra/Documents/R/SalesData.csv")
View(data_v)

#1.	Compare Sales by region for 2016 with 2015 using bar chart

D1 = data_v%>%
  group_by(Region)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_long = gather(D1,key = Year, value = data_v,-Region )
data_long$data_v = round(data_long$data_v,1)

ggplot(data_long,aes(Region,data_v, fill = Year,label = Sales)) +
  geom_bar(stat = "identity",position = "dodge")+ 
  geom_text(size = 4) + xlab('Region') +ylab('Sales') + 
  ggtitle("Comparision of Sales by Region")

#2.	Pie charts for sales for each region in 2016
pie = data_v%>%
  group_by(Region)%>%
  summarise(TotalSales2016 = sum(Sales2016))

piepercnt = round(pie$TotalSales2016/sum(pie$TotalSales2016)*100,1)
lvls= pie$Region
lvls = paste(lvls,":",piepercnt)
lvls = paste(lvls,"%",sep = "")

pie(pie$TotalSales2016,labels = lvls , 
    col = c("lightskyblue","royalblue","turquoise"),
    main = "2D Pie Chart of Sales 2016",radius = 1,border = "black")

pie3D(pie$TotalSales2016,labels = lvls ,explode = 0.15, col = 
        c("lightskyblue","royalblue","turquoise")) + 
  title('3D Pie Chart of Sales 2016')

#3.	Compare sales of 2015 and 2016 with Region and Tiers
D3 = data_v%>%
  group_by(Tier, Region)%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))
View(D3)

data_RT = gather(D3,key = Year,value = Sales,-c(Region,Tier))
ggplot(data_RT,aes(Tier,data_v,fill = Year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ Region) +ggtitle("Comparision of Sales by Region and Tiers")

#4.	In East region, which state registered a decline in 2016 as compared to 2015?
D4 = data_v%>%
  group_by(State)%>%
  filter(Region =="East")%>%
  summarise(TotalSales2015 = sum(Sales2015),TotalSales2016 = sum(Sales2016))

data_E = gather(D4,key = Year, value = data_v,-State)
ggplot(data_E,aes(State,data_v,fill = Year)) + geom_bar(stat = "identity",
      position = "dodge") +ggtitle("Comparision of Sales by State")

#5.In all the high tier, which division saw a decline in the number 
#of unit sold in 2016 as compared to 2015

D5 = data_v%>%
  filter(Tier == "High")%>%
  group_by(Division)%>%
  summarise(TotalUnit2015 = sum(Units2015),TotalUnit2016 = sum(Units2016))

data_d = gather(D5,key = Year, value = Sales,-Division)
gg1 = ggplot(data_d,aes(Division,data_v, fill = Year)) + geom_bar(stat = "identity",position = "dodge")
gg1 + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Comparision of Sales by Division")

#6.	Create a new column Qtr -
vec1=data_v$Month
data_v=cbind(data_v,Sales_Qtr=vec1)
View(data_v)

#7.	Compare Qtr wise sales in 2015 and 2016 in a bar plot
data_v$Sales_Qtr = if_else(data_v$Month == "Jan"|data_v$Month == "Feb"|data_v$Month == "Mar","Q1",
if_else(data_v$Month == "Apr"|data_v$Month == "May"|data_v$Month == "Jun","Q2",
if_else(data_v$Month == "Jul"|data_v$Month == "Aug"|data_v$Month == "Sep","Q3","Q4")))

D6 = data_v%>%
  group_by(Sales_Qtr)%>%
  summarise(TotalSum2015 = sum(Sales2015),TotalSum2016 = sum(Sales2016))
data_qtrwise = gather(D6,key = Year, value = data_v,-Sales_Qtr)
ggplot(data_qtrwise,aes(Sales_Qtr,data_v,fill = Year)) + geom_bar(stat = "identity",position = "dodge") + ggtitle("Comparision of Sales by Quarter")

#8.	Determine the composition of Qtr wise sales in 
#and 2015 with regards to all the Tiers in a pie chart.
