library(readxl)
data <- read_excel("E:/My Documents/Document to Go to/EFFORT/COLLEGE/ACADEMIC/SEMESTER 4/Analisis Eksplorasi Data/Final Project/sales.xlsx")
data = within(data, {
  ID <- as.character(`Invoice ID`)
  Branch <- factor(Branch)
  City <- factor(City)
  `Customer type` <- factor(`Customer type`)
  Gender <- factor(Gender)
  `Product line` <- factor(`Product line`)
  Payment <- factor(Payment)
  Day <- factor(Day, levels = c('Monday','Tuesday','Wednesday','Thursday',
                                'Friday','Saturday', 'Sunday'))
})
attach(data)
summary(data)
View(data)

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggthemes)

# histogram
ggplot(data, aes(x = `Unit price`)) +
  geom_histogram(aes(y=..count..), col = "white", fill = "#023e8a") +
  geom_density(alpha = 0.2, size = 0.85, col="#e63946") +
  geom_vline(aes(xintercept = mean(`Unit price`)), linetype='dashed',
             col = "#e63946") +
  theme_bw() +
  labs(x = "Unit Price",
       y = "Density",
       title = "Histogram",
       subtitle = "Unit Price",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

ggplot(data, aes(x = Rating)) +
  geom_histogram(aes(y=..density..), col = "white", fill = "#023e8a") +
  geom_density(alpha = 0.2, size = 0.85, col="#e63946") +
  geom_vline(aes(xintercept = mean(`Unit price`)), linetype='dashed',
             col = "#e63946") +
  theme_bw() +
  labs(y = "Density",
       title = "Histogram",
       subtitle = "Rating",
       caption = "Source: Supermarket Sales Dataset by Kaggle") +
  scale_x_continuous(name="Rating", limits=c(2, 20))

ggplot(data, aes(x = Quantity)) +
  geom_histogram(aes(y=..density..), col = "white", fill = "#023e8a",
                 bins = 15) +
  geom_density(alpha = 0.2, size = 0.85, col="#e63946") +
  geom_vline(aes(xintercept = mean(`Unit price`)), linetype='dashed',
             col = "#e63946") +
  theme_bw() +
  labs(y = "Density",
       title = "Histogram",
       subtitle = "Quantity",
       caption = "Source: Supermarket Sales Dataset by Kaggle") +
  scale_x_continuous(name="Quantity", limits=c(0, 15))

# barplot
data %>%
  select(`Product line`) %>%
  count(`Product line`, sort=TRUE) %>%
  ggplot(aes(y = reorder(`Product line`, n), x=n)) +
  geom_bar(fill = c('#2364aa','#3da5d9','#73bfb8','#fec601','#ea7317','#cd0c2b'),
           stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  geom_text(aes(label=n), vjust =0.8, hjust=1.5, col = "white",
            fontface='bold') +
  labs(y = "Product Line",
       x = "Count",
       title = "Bar Plot",
       subtitle = "Sales of Product Line",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

data %>%
  select(City) %>%
  count(City, sort=TRUE) %>%
  ggplot(aes(x = reorder(City, -n), y=n)) +
  geom_bar(fill = c('#2364aa','#3da5d9','#73bfb8'),
           stat = "identity") +
  theme_bw() +
  geom_text(aes(label=n), vjust =1.5, hjust=0.5, col = "white",
            fontface='bold') +
  labs(y = "Count",
       x = "City of Supermarket Branch",
       title = "Bar Plot",
       subtitle = "Number of Visitor in Each Supermarket Branch",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

ggplot(data=data, aes(y = Day)) +
  geom_bar(fill = c('#2364aa','#3da5d9','#73bfb8','#fec601','#ea7317',
                    '#cd0c2b', '#0a9396'), stat = "count", position=position_dodge()) +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  geom_text(aes(label = ..count..), vjust =0.5, hjust=1.5,
            col = "white", fontface='bold',
            stat = "count", position = position_dodge(0.9)) +
  labs(y = "Day",
       x = "Count",
       title = "Bar Plot",
       subtitle = "Day based on Customers Visit",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

# countplot
#1
ggplot(data=data, aes(x = `Product line`)) +
  geom_bar(aes(fill = Gender), stat = "count",position=position_dodge()) +
  scale_fill_manual(values = c("#e5989b", "#0a9396")) +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = Gender), vjust =-0.5, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Product Line",
       y = "Count",
       title = "Count Plot",
       subtitle = "Product Line based on Gender",
       caption = "Source: Supermarket Sales Dataset by Kaggle")
library(rcompanion)
cramerV(xtabs(~`Product line` + Gender))

#2
ggplot(data=data, aes(x = `Product line`)) +
  geom_bar(aes(fill = `Customer type`), stat = "count", position=position_dodge()) +
  scale_fill_brewer(palette="Set2") +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = `Customer type`), vjust = -0.5, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Product Line",
       y = "Count",
       title = "Count Plot",
       subtitle = "Product Line based on Customer Type",
       caption = "Source: Supermarket Sales Dataset by Kaggle")
library(rcompanion)
cramerV(xtabs(~`Product line` + `Customer type`))
#3
ggplot(data=data, aes(x = Day)) +
  geom_bar(aes(fill = Gender), stat = "count", position=position_dodge()) +
  scale_fill_manual(values = c("#e5989b", "#0a9396")) +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = Gender), vjust = -0.5, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Day",
       y = "Count",
       title = "Count Plot",
       subtitle = "Day based on the Number of Customers Divided by Gender",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#4
ggplot(data=data, aes(x = Day)) +
  geom_bar(aes(fill = `Customer type`), stat = "count", position=position_dodge()) +
  scale_fill_brewer(palette="Set2") +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = `Customer type`), vjust = -0.5, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Day",
       y = "Count",
       title = "Bar Plot",
       subtitle = "Day based on the Number of Customers Divided by Customer Type",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#5
ggplot(data=data, aes(x = Day)) +
  geom_bar(aes(fill = `Product line`), stat = "count", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = `Product line`), vjust = -0.5, col = "black",
            stat = "count", position = position_dodge(0.9), size = 2) +
  labs(x = "Day",
       y = "Count",
       title = "Count Plot",
       subtitle = "Day based on the Number of Customers Divided by Product Line",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#6
ggplot(data=data, aes(x = `Customer type`)) +
  geom_bar(aes(fill = Gender), stat = "count", position=position_dodge()) +
  scale_fill_manual(values = c("#e5989b", "#0a9396")) +
  theme_bw() +
  labs(x = "Customer Type",
       y = "Count",
       title = "Count Plot",
       subtitle = "Number of Customer Type based on Gender",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#6
ggplot(data=data, aes(x = Day)) +
  geom_bar(aes(fill = City), stat = "count", position=position_dodge()) +
  scale_fill_manual(values = c('#2364aa','#3da5d9','#73bfb8')) +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = City), vjust = -0.5, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Customer Type",
       y = "Count",
       title = "Bar Plot",
       subtitle = "Day based on the Number of Customers Divided by City of Supremarket",
       caption = "Source: Supermarket Sales Dataset by Kaggle")
#7
ggplot(data=data, aes(x = Payment)) +
  geom_bar(aes(fill = `Customer type`), stat = "count", position=position_dodge()) +
  scale_fill_manual(values=c("#e5989b", "#0a9396")) +
  theme_bw() +
  geom_text(aes(label = ..count.., fill = `Customer type`), vjust = -0.2, col = "black",
            stat = "count", position = position_dodge(0.9)) +
  labs(x = "Payment Methods",
       y = "Count",
       title = "Count Plot",
       subtitle = "Payment Methods based on Customer Type",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#7 ada geom_text
data%>%
  select(City,`Product line`,Rating)%>%
  group_by(`Product line`,City)%>%
  summarise(Rating=round(mean(Rating),2))%>%
  ggplot(aes(x = reorder(City, -Rating), y=Rating,fill=`Product line`)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = Rating), col = "black", 
            position = position_dodge(0.7), hjust = 1, size=2)
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "City of Supermarket Branch",
       y = "Rating",
       title = "Count Plot",
       subtitle = "Rating based on Product Line of Each City",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

#7
ggplot(data=data, aes(x = City, y=Rating,fill=`Product line`)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_fill_brewer(palette = "Set2") +
    theme_bw() +
    labs(x = "City of Supermarket Branch",
         y = "Rating",
         title = "Count Plot",
         subtitle = "Rating based on Product Line of Each City",
         caption = "Source: Supermarket Sales Dataset by Kaggle")    


# bubble plot
data%>%
  ggplot(aes(x = `Unit price`, y = Rating, size=Quantity, color=Gender)) +
  geom_point(alpha=0.27) +
  scale_colour_wsj('colors6') +
  labs(x = "Unit Price",
       y = "Rating",
       title = "Bubble Plot",
       subtitle = "Unit Price vs Rating based on Quantity and Gender",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

data%>%
  ggplot(aes(x = `Unit price`, y = Rating, size=Quantity, color=`Customer type`)) +
  geom_point(alpha=0.27) +
  scale_colour_wsj('dem_rep') +
  labs(x = "Unit Price",
       y = "Rating",
       title = "Bubble Plot",
       subtitle = "Unit Price vs Rating based on Quantity and Customer Type",
       caption = "Source: Supermarket Sales Dataset by Kaggle")

  
# pie chart
#membership
member1<-sum(`Customer type`=="Member")
member2<-sum(`Customer type`=="Normal")
piemember<-data.frame(
  Tipe=c("Member","Normal"),
  Jumlah=c(member1,member2),
  prop=c(100*member1/(member1+member2),100*member2/(member1+member2))
) 
piemember<-piemember%>%
  arrange(desc(Tipe))%>%
  mutate(lab.ypos=cumsum(prop)-0.5*prop)
ggplot(piemember,aes(x="",y=prop,fill=Tipe))+
  geom_bar(width=1,stat="identity",color="white")+
  coord_polar("y",start=0)+
  geom_text(aes(y=lab.ypos,label=paste0(prop,"%")),color="black",size=6)+
  scale_fill_manual(values=c("turquoise1","chartreuse"))+
  theme_void()+labs(title="Pie Chart",subtitle="Presentase Konsumen berdasarkan Tipe
Keanggotaan",
                    fill="Tipe Keanggotaan")
