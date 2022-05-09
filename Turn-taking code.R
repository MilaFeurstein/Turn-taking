# Study 1 

dataset<-read_excel("C:/Users/lvfeu/Desktop/Kuhl dataset.xlsx", sheet="Sheet1")
dataset<-as.data.frame(dataset)
dataset[1]
str(dataset)

install.packages("xlsx")
library(xlsx)
data<-read.xlsx2("C:/Users/lvfeu/Desktop/Kuhl dataset.xlsx", sheetName="Sheet1") 
data$Subject.ID<-as.vector(as.numeric(data$Subject.ID))
data$LM_6m_babbling_pct<-as.numeric(data$LM_6m_babbling_pct) # percentage child babbling
data$LA6mAvgD1D2_AWC.Proj<-as.numeric(data$LA6mAvgD1D2_AWC.Proj) # AWC 12-hour projection
data$LM_6m_parentese_pct<-as.numeric(data$LM_6m_parentese_pct) # percentage CDS (100 30-sec clips with highest AWC)
data$LM_6m_standard_pct<-as.numeric(data$LM_6m_standard_pct) # percentage ADS
data$LA6mAvgD1D2_CTC.Proj<-as.numeric(data$LA6mAvgD1D2_CTC.Proj) # Conversational turn count
data$SES<-as.numeric(data$SES) # SES (Hollingshead Index between 8 and 66)
data$LA10mAvgD1D2_AWC.Proj<-as.numeric(data$LA10mAvgD1D2_AWC.Proj)
data$LM10m_parentese_pct<-as.numeric(data$LM10m_parentese_pct)
data$LM10m_standard<-as.numeric(data$LM10m_standard)
data$LA10mAvgD1D2_CTC.Proj<-as.numeric(data$LA10mAvgD1D2_CTC.Proj)
data$LA14mAvgD1D2_AWC.Proj<-as.numeric(data$LA14mAvgD1D2_AWC.Proj)
data$LM14m_parentese_pct<-as.numeric(data$LM14m_parentese_pct)
data$LM14m_standard<-as.numeric(data$LM14m_standard)
data$LA14mAvgD1D2_CTC.Proj<-as.numeric(data$LA14mAvgD1D2_CTC.Proj)
data$LA18mAvgD1D2_AWC.Proj<-as.numeric(data$LA18mAvgD1D2_AWC.Proj)
data$LM_18m_parentese_pct<-as.numeric(data$LM_18m_parentese_pct)
data$LM_18m_standard_pct<-as.numeric(data$LM_18m_standard_pct)
data$LA18mAvgD1D2_CTC.Proj<-as.numeric(data$LA18mAvgD1D2_CTC.Proj)
data$LM_18m_baby_words<-as.numeric(data$LM_18m_baby_words) # percentage child English words
data$VOCAB_18mo_CDI<-as.numeric(data$VOCAB_18mo_CDI) # production at the end
str(data) # seems right

library(tidyverse)

# Avg for parentese (CDS)
select_vars <- c("LM_6m_parentese_pct", "LM10m_parentese_pct", "LM14m_parentese_pct", "LM_18m_parentese_pct")
data<-data %>% mutate(avg.CDS = rowMeans(dplyr::select(., select_vars)))

# Avg for standard (ADS)
select_vars2 <- c("LM_6m_standard_pct", "LM10m_standard", "LM14m_standard", "LM_18m_standard_pct")
data<-data %>% mutate(avg.ADS = rowMeans(dplyr::select(., select_vars2)))

# Avg for CTC
select_vars3 <- c("LA6mAvgD1D2_CTC.Proj", "LA10mAvgD1D2_CTC.Proj", "LA14mAvgD1D2_CTC.Proj", "LA18mAvgD1D2_CTC.Proj")
data<-data %>% mutate(avg.CTC = rowMeans(dplyr::select(., select_vars3)))


attach(data)
# Nadine
data<-data %>% dplyr::add_row("Subject.ID"=72, "SES" = 49.5, "VOCAB_18mo_CDI" = 58, "avg.CTC"=525, "avg.CDS" = 43.8, "avg.ADS" = 56.2)

Participant<-data$`Subject.ID`
avg.CDS<-data$avg.CDS
avg.ADS<-data$avg.ADS
avg.CTC<-data$avg.CTC
SES<-data$SES
VOCAB_18mo_CDI<-data$VOCAB_18mo_CDI

str(avg.CDS)

data<-data.frame(Participant, VOCAB_18mo_CDI, avg.CDS, avg.ADS, avg.CTC, SES)


# GLM
library(lme4)


fit <- glmer(VOCAB_18mo_CDI ~ -1 + avg.CDS + avg.ADS + avg.CTC*SES + (1|Participant), family=poisson(link = log))
summary(fit)

# HGLM
data<-na.omit(data) 

library(hglm)
turns.pois<-hglm(fixed = VOCAB_18mo_CDI ~ -1 + avg.CDS + avg.CTC*SES,
              random = ~ 1|Participant,
              family = poisson(link = log),
              rand.family = gaussian(link = identity), data=data)
summary(turns.pois) 

data$Prediction<-round(exp(turns.pois$fixef[1]*data$avg.CDS +
                                   turns.pois$fixef[2]*data$avg.CTC +
                                   turns.pois$fixef[3]*data$SES +
                                   turns.pois$fixef[4]*(data$SES*data$avg.CTC)))

# order data by vocabulary

data<-data[order(data$VOCAB_18mo_CDI),]
data$Participant<-1:69

library(ggplot2)
library(ggthemes) 


gghglm<-ggplot(data, aes(x = Participant)) +
  geom_point(aes(y = VOCAB_18mo_CDI, color = "Vocabulary observed"), size=2)+
  geom_point(aes(y = Prediction, color= "Vocabulary predicted"), size=2) +
  geom_smooth(aes(y = Prediction, color = "Vocabulary predicted"), method = "loess") +
    ylab("Vocabulary at 18mo") + xlab("Participants ordered by vocabulary size") +
    scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("black", "red")) +
  guides(color=guide_legend("Values"))+
  theme_economist_white() +
  theme(legend.position = c(0.2, 0.8),
        axis.line = element_line(color="black", size = 0.4) +
          theme_economist()  ) 
  
  
range(data.turns$VOCAB_18mo_CDI)
mean(data.turns$VOCAB_18mo_CDI)
sd(data.turns$VOCAB_18mo_CDI)

range(data.turns$SES)
mean(data.turns$SES)
sd(data.turns$SES)

range(data.turns$avg.CDS)
mean(data.turns$avg.CDS)
sd(data.turns$avg.CDS)

range(data.turns$avg.CTC)
mean(data.turns$avg.CTC)
sd(data.turns$avg.CTC)

# Discussion of Study 1

t<-c(210,119,37,0,26,0,0,26,21,0,11,99,189)
q<-c(190,52,23,1,38,1,2,33,39,6,17,87,90)

library(stats)
cor.test(t,q)

save(data.turns, file = "data.turns.RData")

# Study 2

library(readxl)
mydata<-read_excel("C:/Users/lvfeu/Desktop/CS_CDS.xlsx", sheet="CS_CDS.csv")
summary(mydata)
str(mydata)
mydata$Utterance_type<-as.factor(mydata$Utterance_type)
mydata$Participant<-as.factor(mydata$Participant)
mydata$Language<-as.factor(mydata$Language)

table(mydata$TCH_Language)

mydata$Transcription<-as.factor(mydata$Transcription)
mydata$Age<-as.factor(mydata$Age)
mydata$`Dialogue/Interaction_unit`<-as.factor(mydata$`Dialogue/Interaction_unit`)

attach(mydata)

library(plyr)
library(tidyverse)
mydata2<-mydata %>% filter(CDS == 0 & Age == 12)
mydata3<-mydata %>% filter(CDS == 0 & Age == 18)

mydata4<-mydata %>% filter(Age == 6 & Participant==c("S1","S2"))
mydata41<-mydata %>% filter(Age == 6 & Participant=="S2")
mydata5<-mydata %>% filter(Age == 12 & Participant==c("S1","S2","S3"))
mydata51<-mydata %>% filter(Age == 12 & Participant==c("S1","S2"))
mydata52<-mydata %>% filter(Age == 12 & Participant==c("S2","S3"))
mydata53<-mydata %>% filter(Age == 12 & Participant=="S2")
mydata6<-mydata %>% filter(Age == 18 & Participant==c("S2","S3"))
mydata61<-mydata %>% filter(Age == 18 & Participant=="S2")

mydata7<-mydata %>% filter(Participant=="TCH")

t2<-count(mydata$Transcription[CDS==0])
t2

library(DT)
mm<-as.data.frame(sort(table(mydata2$Transcription), decreasing = T))
datatable(mm)
mm2<-as.data.frame(sort(table(mydata3$Transcription), decreasing = T))
datatable(mm2)

require(ggplot2)

# 6 mo

df = tibble::as_tibble(mydata41)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("6 m.o.") +
  labs(x="Participants", y ="Relative Frequency")

# 12 mo

df = tibble::as_tibble(mydata53)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("12 m.o.") +
  labs(x="Participants", y ="Relative Frequency")

# 18 mo

df = tibble::as_tibble(mydata61)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("18 m.o.") +
  labs(x="Participants", y ="Relative Frequency")

# TCH

df = tibble::as_tibble(mydata7)

d <- df %>%
  group_by(Age,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Age),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("TCH Utterances") +
  labs(x="Age", y ="Relative Frequency")


# Mean turns

mydata8<-mydata %>% filter(Age == 6)
library(plyr)
df2<-ddply(mydata8,c('`Dialogue/Interaction_unit`'),function(x) x[which(x$Number==max(x$Number)),])
mean(df2$Number)# 3.568
max(mydata8$Number) # 17

mydata9<-mydata %>% filter(Age == 12)
df2<-ddply(mydata9,c('`Dialogue/Interaction_unit`'),function(x) x[which(x$Number==max(x$Number)),])
mean(df2$Number)# 4.653
max(mydata9$Number) # 63

mydata10<-mydata %>% filter(Age == 18)
df2<-ddply(mydata10,c('`Dialogue/Interaction_unit`'),function(x) x[which(x$Number==max(x$Number)),])
mean(df2$Number)# 5.155
max(mydata10$Number) # 41

# Contingency
mydata2<-mydata %>% filter(Age == 18)
max(as.numeric(mydata2$`Dialogue/Interaction_unit`))

mydata12<-mydata %>% filter(Age == 6 & CDS==0 & Response_yes==1)
mydata13<-mydata %>% filter(Age == 6 & CDS==0)
count(mydata12)/count(mydata13) # 86%

mydata14<-mydata %>% filter(Age == 12 & CDS==0 & Response_yes==1)
mydata15<-mydata %>% filter(Age == 12 & CDS==0)
count(mydata14)/count(mydata15) # 91%

mydata16<-mydata %>% filter(Age == 18 & CDS==0 & Response_yes==1)
mydata17<-mydata %>% filter(Age == 18 & CDS==0)
count(mydata16)/count(mydata17) # 89% 

# Initiation

mydata18<-mydata %>% filter(Age == 6 & CDS==0 & Inititation==1)
mydata19<-mydata %>% filter(Age == 6)
count(mydata18)/max(as.numeric(mydata19$`Dialogue/Interaction_unit`)) # 24%

mydata20<-mydata %>% filter(Age == 12 & CDS==0 & Inititation==1)
mydata21<-mydata %>% filter(Age == 12)
count(mydata20)/max(as.numeric(mydata21$`Dialogue/Interaction_unit`)) # 53%

mydata22<-mydata %>% filter(Age == 18 & CDS==0 & Inititation==1)
mydata23<-mydata %>% filter(Age == 18)
count(mydata22)/max(as.numeric(mydata23$`Dialogue/Interaction_unit`)) # 63.4%

# Utterance vs Response

mydata11<-mydata[!(mydata$Participant=="TCH"),]

df = tibble::as_tibble(mydata11)

d <- df %>%
  group_by(Utterance_type,Response_yes) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Utterance_type),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Utterances Encouraging Child Response") +
  labs(x="Utterance Type", y ="Response Obtained") +
  scale_fill_manual(values = colors) 

# Iconicity

mydata24<-mydata %>% filter(Age == 6 & CDS==0 & Utterance_type=="Icon") #0
mydata25<-mydata %>% filter(Age == 6 & CDS==1 & Utterance_type=="Icon") #10

mydata26<-mydata %>% filter(Age == 12 & CDS==0 & Utterance_type=="Icon") #195
mydata27<-mydata %>% filter(Age == 12 & CDS==1 & Utterance_type=="Icon") #128

mydata28<-mydata %>% filter(Age == 18 & CDS==0 & Utterance_type=="Icon") #100
mydata29<-mydata %>% filter(Age == 18 & CDS==1 & Utterance_type=="Icon") #52

# Language
summary(mydata$Language)
mydata18<-mydata %>% filter(CDS==1)
mydata19<-mydata[!c(mydata$Language=="TCH"),]

df = tibble::as_tibble(mydata19)

d <- df %>%
  group_by(Age,Language) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Age),
           y=freq,
           fill = Language)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Language Exposure Distribution") +
  labs(x="Age", y ="Input Proportion")


data_long <- gather(mydata20, Age, Language, Age, Language, factor_key=TRUE)


# more on long to wide and back:
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/


gg4 <- ggplot(data_long, aes(fill=Language, y=Value, x=Age)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x = "Date",
       y = "Input (direct vs. indirect)",
       color = "Languages",
       title="Exposure to Main Languages_Nadine_Minutes_Daily_Stacked Barchart") +
  scale_fill_manual(values = colors) 

gg4

# Standardized colours

colors <- c("Att" = "firebrick1", "Decl" = "cadetblue4", "Excl" = "maroon1",
            "Icon" = "seagreen1", "Imit" = "darkslategray", "Imper" = "grey73", 
            "Interr" = "lightslateblue", "Sing" = "magenta", "Valid" = "cyan")

mydata4<-mydata %>% filter(Age == 6 & Participant==c("S1","S2"))
df = tibble::as_tibble(mydata4)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("6 m.o.") +
  labs(x="Participants", y ="Relative Frequency") +
  scale_fill_manual(values = colors) 

mydata5<-mydata %>% filter(Age == 12 & Participant==c("S1","S2"))
df = tibble::as_tibble(mydata5)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("12 m.o.") +
  labs(x="Participants", y ="Relative Frequency") +
  scale_fill_manual(values = colors) 


mydata52<-mydata %>% filter(Age == 12 & Participant==c("S2","S3"))
df = tibble::as_tibble(mydata52)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("12 m.o.") +
  labs(x="Participants", y ="Relative Frequency") +
  scale_fill_manual(values = colors) 

mydata6<-mydata %>% filter(Age == 18 & Participant==c("S2","S3"))
df = tibble::as_tibble(mydata6)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("18 m.o.") +
  labs(x="Participants", y ="Relative Frequency") +
  scale_fill_manual(values = colors) 


mydata41<-mydata %>% filter(Age == 6 & Participant=="S2")
mydata53<-mydata %>% filter(Age == 12 & Participant=="S2")
mydata61<-mydata %>% filter(Age == 18 & Participant=="S2")

df = tibble::as_tibble(mydata41)

d <- df %>%
  group_by(Participant,Utterance_type) %>%
  summarise(Total = n()) %>%
  mutate(freq=Total/sum(Total))

ggplot(d,
       aes(x = factor(Participant),
           y=freq,
           fill = Utterance_type)
) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("6 m.o.") +
  labs(x="Participants", y ="Relative Frequency") +
  scale_fill_manual(values = colors) 

