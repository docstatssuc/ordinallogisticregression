############################################################
#                                                          #
#                          PLOTS                           #
#                                                          #
############################################################


# LIBRARIES ---------------------------------------------------------------

require(tidyverse)
require(ggplot2)
require(extrafont)
require(stringr)
require(ggthemes)
require(vioplot)
require(ggmosaic)



# DATA ------------------------------------------------------------

load("dataMinAcc.rda")
head(dataN)

#  proportion operative process -------------------------------------------

ggplot(dataN, aes(BodyPart)) + 
  geom_bar() +
  ggtitle("Percentage of physical injuries ")+
  xlab("Body part injured") +
  ylab("Number of accidents")+
  geom_text(aes(y = (..count..), 
                label =   ifelse((..count..)==0,"",
                                 scales::percent((..count..)/sum(..count..)))),
            stat="count", nudge_y = 7,size=6)+theme_gdocs()+
  scale_x_discrete(labels=c("Heads", "Lower Limbs", "Multiple", 
                            "Torso", "Upper Limbs"))+
  theme_clean()

# Accidents per year ------------------------------------------------------

ggplot(dataN, aes(fill=SL, x=OpPr)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(x=count)),
             position=position_fill(vjust = 0.5), colour="black")+
  scale_fill_manual(name="Severity level",  labels = c("Slight", 
                                                       "Medium", 
                                                       "High"),
                    values = c("#AAAAAA","#73777B","#444444"))+
  scale_x_discrete(labels=c("Underground Mining", "Surface Mining",
                            "Smelters and Refineries", 
                            "Others", "Maintenance",
                            "Hydrometallurgy",
                            "Crushing",
                            "Concentration"))+
  coord_flip()+
  xlab("Operative Process") +
  ylab("Number of accidents")+ ggtitle("Number of Accidents per Operative Process
according to Severity Level")+
  theme_clean()+
  theme(legend.position="bottom")

# Physical injuries frquency ----------------------------------------------


ggplot(dataN %>% filter(Year!=2018) %>% count(Month,Year), aes(x=Month,y=n,group=Year)) + 
  geom_point(size=2)+ 
  geom_line(aes(linetype=Year),size=1)+
  ggtitle("Accidents per year ")+
  xlab("Month") +
  ylab("Number of accidents")+
  scale_x_discrete(labels=c("01"="January", "02"="February", "03"="March", 
                            "04"="April", "05"="May", "06"="June",
                            "07"="July","08"="August","09"="September",
                            "10"="October","11"="November","12"="December"))+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust=1))+ labs(colour = "Year")+
  theme(legend.position="bottom")


# Mosaic body part --------------------------------------------------------


df <- dataN
levels(df$BodyPart) <- c("Heads", "Lower Limbs", "Multiple", 
                         "Torso", "Upper Limbs")


ggplot(df) +
  geom_mosaic(aes(x = product(BodyPart), fill = SL)) +
  guides(fill=guide_legend(title = "Severity Level", reverse = TRUE))+
  ggtitle("Body Part Injured and Severity Level")+
  xlab("Body part injured")+
  ylab(" ")+
  scale_fill_manual(name="Severity level",  labels = c("Slight", 
                                                       "Medium", 
                                                       "High"),
                    values = c("#AAAAAA","#73777B","#444444"))+
  geom_mosaic_text(aes(x = product(SL, BodyPart), 
                       label = after_stat(.n)), repel=TRUE, size=4)+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  theme(legend.position="bottom")

rm(df)

# Violin plots ------------------------------------------------------------

## Age

ggplot(dataN %>% filter(Age!=is.na(Age)),aes(x=" ",y= Age)) +
  geom_violin()+
  geom_boxplot(fill = "#AAAAAA",
               alpha = 0.5,
               color = 1) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none",
        plot.title = element_text(size=11)) +
  ggtitle("Age of employees") +
  xlab("")+theme_clean()

## Experience

ggplot(dataN %>% filter(Experience!=is.na(Experience)),aes(x=" ",y= Experience)) +
  geom_violin()+
  geom_boxplot(fill = "#AAAAAA",  
               alpha = 0.5,       
               color = 1) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none",
        plot.title = element_text(size=11)) +
  ggtitle("Years of experience") +
  theme_clean()+scale_y_continuous(breaks = seq(0,50,10))+xlab("")

