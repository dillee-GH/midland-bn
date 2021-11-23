library(dplyr)
library(tidyverse)
library(ggplot2)

#Fig.2 Typology of investment locaitons
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Invest_location_thresh.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Resource_frontier <- as.factor(trial$Resource_frontier)
trial$Agglomeration_economies <- as.factor(trial$Agglomeration_economies)
trial$Investment_location <- as.factor(trial$Investment_location)
trial$Tag <- as.factor(trial$Tag)
sapply(trial, class)

trial$Investment_location <- factor(trial$Investment_location,levels = c("Smallholder populated land", "Subsistence frontier", "Emerging commercial frontier", "Established markets"))
trial$Resource_frontier <- factor(trial$Resource_frontier,levels = c("low", "medium", "high"))
trial$Agglomeration_economies <- factor(trial$Agglomeration_economies,levels = c("low", "medium", "high"))

investment_location_labels <- c("A. Populated \nsmallholder land", "B. Subsistence \nfrontier", "C. Emerging \ncommercial frontier", "D. Established \nmarkets")
names(investment_location_labels) <- c("Smallholder populated land", "Subsistence frontier", "Emerging commercial frontier", "Established markets")

p2<-ggplot(trial, aes(Agglomeration_economies, Resource_frontier, fill= Probability)) +
facet_grid(~Investment_location, labeller = labeller(Investment_location = investment_location_labels)) +
geom_tile() +
theme_bw()+
labs (y = "Resource frontier", x= "Agglomeration economies") +
theme(axis.text.y = element_text(family = NULL, size = rel(1.8), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.8), colour = "grey30"),
axis.title.x = element_text (family = NULL, size = rel(1.4), colour = "grey30", margin = margin(t = 25, b = 0)),
axis.title.y = element_text (family = NULL, size = rel(1.4), colour = "grey30", margin = margin(r = 15, l = 0)),
legend.title = element_text(family = NULL, size = rel(1.3), colour = "grey30", margin = margin(t = 0, b = 5)),
legend.text = element_text(family = NULL, size = rel(1.1), colour = "grey30", margin = margin(r = 0, l = 5)),
strip.text.x = element_text(family = NULL, size = rel(1.8), colour = "grey30"),
axis.ticks = element_blank(),
strip.background = element_blank(),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
scale_fill_gradientn(colours = c("#205164", "#228592", "#2dbca5", "#b7e6a5", "#f7feae"), name = "Probability", breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
scale_x_discrete(expand = c(0,0)) +
scale_y_discrete(expand = c(0,0)) +
coord_fixed(ratio=1)


p2

#Fig.3 Investor track records
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/investor_profile_data_for_plots.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Variable <- as.factor(trial$Variable)
trial$Label <- as.factor(trial$Label)
trial$Category <- as.factor(trial$Category)
trial$Group <- as.factor(trial$Group)
sapply(trial, class)

trial$Variable <- factor(trial$Variable,levels = c("Similar farming or forestry experience", "Other farming or forestry experience", "No farming or forestry experience", "Regional experience", "No skill set", "Export market control", "Local market control", "No market control"))
trial$Category <- factor(trial$Category,levels = c("Skill set (farming/forestry experience)", "Skill set (regional experience)", "Skill set", "Market control"))
trial$Group <- factor(trial$Group,levels = c("Skill set", "Market Control"))
trial$Label <- factor(trial$Label,levels = c("Similar", "Other", "None","Regional","None","Export","Local","None"))

scale_x_discrete(labels = c("Similar farming or forestry experience" = "Similar", "Other farming or forestry experience" = "Other", "No farming or forestry experience" = "None", "Regional experience" = "Regional", "No skill set"= "None", "Export market control" = "Export", "Local market control" = "Local", "No market control" = "None"))
p3 <- ggplot(trial, aes(x=Variable, y=Percentage, fill = Category)) +
geom_col(width = 0.9) + facet_grid(~Group, scales = "free_x", space = "free_x") +
theme_minimal() +
labs (y = "Frequency %") +
scale_fill_manual(values =c("#91f2c9", "#2dbca5", "#228592", "#f7feae"), limits=c("Skill set (farming/forestry experience)", "Skill set (regional experience)", "Skill set", "Market control"), labels = c("Skill set (farming/forestry experience)", "Skill set (regional experience)", "Skill set", "Market control")) +
theme(axis.text.y = element_text(family = NULL, size = rel(1.2), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.2), colour = "grey30"),
axis.title.y = element_text (family = NULL, size = rel(1.0), colour = "grey30", margin = margin(r = 15, l = 0)),
legend.text = element_text(family = NULL, size = rel(1.0), colour = "grey30"),
axis.title.x = element_blank(),
axis.ticks.x = element_blank(),
legend.title = element_blank(),
strip.text.x = element_blank(),
legend.position = "bottom",
legend.direction = "vertical",
legend.justification = c("left", "bottom"),
panel.grid.major.x = element_blank(),
panel.spacing = unit(0, "lines")) +
coord_cartesian(ylim = c(0, 65))+
scale_y_continuous(expand = c(0,0)) +
scale_x_discrete(labels = c("Similar farming or forestry experience" = "Similar", "Other farming or forestry experience" = "Other", "No farming or forestry experience" = "None", "Regional experience" = "Regional", "No skill set"= "None", "Export market control" = "Export", "Local market control" = "Local", "No market control" = "None"))

p3


#Fig.4 selection criteria frequency
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Selection_criteria_freq_for_R_plots.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Criterion <- as.factor(trial$Criterion)
sapply(trial, class)

p4 <- trial %>%
mutate(Criterion = fct_reorder(Criterion, (Frequency))) %>%
ggplot() +
theme_classic() +
labs (x = "Frequency %") +
geom_point(aes(x = 5, y = Criterion),
size = 0, col = "white") +
theme(axis.title.x = element_text(family = NULL, size = rel(1.1), colour = "grey30", margin = margin(t = 15, b = 0)),
axis.text.x = element_text(family = NULL, size = rel(1.3), colour = "grey30"),
axis.text.y = element_text(family = NULL, size = rel(1.3), colour = "grey30"),
axis.ticks.y = element_blank(),
axis.title.y = element_blank(),
axis.line = element_blank()) +
geom_hline(yintercept = 1:15, col = "grey80") +
geom_point(aes(x = Frequency, y = Criterion),
size=3.2, color="#2dbca5") +
scale_x_continuous(limits = c(0,75), breaks=c(0, 15, 30, 45, 60, 75)) +
coord_cartesian(xlim = c(0, 75))

p4

#Fig.6 sensitivity scores
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Sensitivity_scores_for_R_plot.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Criteria <- as.factor(trial$Criteria)
trial$Label <- as.factor(trial$Label)
trial$Configuration <- as.factor(trial$Configuration)
sapply(trial, class)
trial$Criteria <- factor(trial$Criteria,levels = c("Economic and financial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and environmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility", "Types of production", "Investor track record"))

p6 <-
ggplot( trial, aes(x=Criteria, y=Sensitivity)) +
geom_bar(stat="identity", fill="#86f4d0", width = 0.5) +
facet_grid(~Label, scales = "free", space = "free_x") +
coord_flip() +
theme_bw() +
theme(strip.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.text.y = element_text(family = NULL, size = rel(1.35), colour = "grey30"),
axis.title.x = element_text(family = NULL, size = rel(1.19), colour = "grey30", margin = margin(t = 15, b = 0)),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2),
panel.spacing = unit(0.5, "lines"),
axis.title.y = element_blank(),
strip.background = element_blank(),
axis.ticks = element_blank(),
panel.grid.major.x = element_blank()) +
ylab("Mutual information %") +
scale_y_continuous(breaks=c(0, 10, 20, 30), expand = expansion (add= c(0,5))) +
scale_x_discrete(breaks = c("Economic and financial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and environmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility", "Types of production", "Investor track record"),
labels = c("Economic and \nfinancial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and \nenvironmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility", "Types of production", "Investor track record"))


p6

# Fig.7 Shifts investor location
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Shifts_investorprofile_and_combinations_for_R_plot.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Location <- as.factor(trial$Location)
trial$Condition <- as.factor(trial$Condition)
trial$Label <- as.factor(trial$Label)
trial$Tag <- as.factor(trial$Tag)
sapply(trial, class)
trial$Label <- factor(trial$Label,levels = c("F","OA", "HVC", "N","LTD","EXT", "LTD and OA", "EXT and F", "EXT and HVC"))
trial$Condition <- factor(trial$Condition,levels = c("Production type", "Investor track record", "Combination scenarios"))
trial$Location <- factor(trial$Location,levels = c("Populated smallholder land", "Subsistence frontier", "Emerging commercial frontier", "Established market"))

p7 <- ggplot(trial, aes(x=Label, y=Score, fill = Location)) +
geom_col(width = 0.5) +
facet_grid(Condition~., scales = "free", space = "free") +
coord_flip() +
theme_bw () +
labs (y = "Shift %") +
scale_fill_manual(values =c("#f7feae", "#2dbca5", "#228592", "#c71e1d"),
limits=c("Populated smallholder land", "Subsistence frontier", "Emerging commercial frontier", "Established market")) +
theme(axis.text.y = element_text(family = NULL, size = rel(1.35), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.title.x = element_text (family = NULL, size = rel(1.19), colour = "grey30", margin = margin(t = 15, b = 0)),
legend.text = element_text(family = NULL, size = rel(1.19), colour = "grey30"),
axis.title.y = element_blank(),
axis.ticks = element_blank(),
legend.title = element_blank(),
strip.background = element_blank(),
strip.text.y= element_blank(),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2),
panel.grid.major.x = element_blank()) +
scale_x_discrete(labels = c("HVC" = "High-value crops", "OA" = "Other agriculture", "F" = "Forestry", "EXT" = "Extensive", "LTD"= "Limited", "N" = "None", "EXT and HVC" = "High-value crops, Extensive", "EXT and F" = "Forestry, Extensive", "LTD and OA" = "Other agriculture, Limited")) +
geom_text(aes(label=Tag), x = Inf, y = -Inf, hjust = -0.5, vjust = 1.3, inherit.aes = FALSE, colour="grey30") +
scale_y_continuous(limits = c(-20,20), breaks=c(-20, -10, 0, 10, 20))


p7

#Fig.8 selection criteria shift
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Selection_criteria_shift_for_R_plots.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Criterion <- as.factor(trial$Criterion)
trial$Condition <- as.factor(trial$Condition)
trial$Tag <- as.factor(trial$Tag)
sapply(trial, class)
trial$Criterion <- factor(trial$Criterion,levels = c("Governance", "Pioneering spirit", "Economic and financial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and environmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility"))
View(trial)

p8 <- ggplot(trial, aes(x=Criterion, y=Shift)) +
geom_col(width = 0.5, fill = "#86f4d0") +
facet_grid(~Tag, scales = "free", space = "free") +
coord_flip() +
theme_bw () +
labs (y = "Shift %") +
theme(axis.text.y = element_text(family = NULL, size = rel(1.35), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.title.x = element_text (family = NULL, size = rel(1.19), colour = "grey30", margin = margin(t = 15, b = 0)),
strip.text.x= element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.ticks = element_blank(),
legend.title = element_blank(),
axis.title.y = element_blank(),
strip.background = element_blank(),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2),
panel.spacing = unit(0.5, "lines"),
panel.grid.major.x = element_blank()) +
scale_x_discrete(breaks = c("Economic and financial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and environmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility", "Types of production", "Investor track record"),
labels = c("Economic and \nfinancial conditions", "Other inputs supply", "Socio-cultural conditions", "Socio-economic and \nenvironmental impact", "Operations-related reasons", "Labor supply", "Strategic reasons","Infrastructure and logistics", "Policy environment", "Market drivers", "Market proximity", "Agroecology", "Land accessibility", "Types of production", "Investor track record"))

p8

#Fig.S1 shifts res frontier
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Shifts_resFrontier_for_R_plot.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Index <- as.factor(trial$Index)
trial$State <- as.factor(trial$State)
trial$Condition <- as.factor(trial$Condition)
trial$Tag <- as.factor(trial$Tag)
sapply(trial, class)
trial$Condition <- factor(trial$Condition,levels = c("F","OA", "HVC", "N","LTD","EXT", "LTD and OA", "EXT and F", "EXT and HVC"))

pS1 <- ggplot(trial, aes(x=Condition, y=Shift, fill = State)) +
geom_col(width = 0.5) +
facet_grid(Tag~., scales = "free", space = "free") +
coord_flip() +
theme_bw () +
labs (y = "Shift %") +
scale_fill_manual(name = "Resource frontier \ncondition",values =c("#91f2c9", "#2dbca5", "#228592"),
limits=c("Low", "Medium", "High")) +
theme(axis.text.y = element_text(family = NULL, size = rel(1.35), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.title.x = element_text (family = NULL, size = rel(1.19), colour = "grey30", margin = margin(t = 15, b = 0)),
legend.text = element_text(family = NULL, size = rel(1.19), colour = "grey30"),
legend.title = element_text(family = NULL, size = rel(1.19), colour = "grey30"),
axis.title.y = element_blank(),
axis.ticks = element_blank(),
strip.background = element_blank(),
strip.text.y= element_blank(),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
scale_x_discrete(labels = c("HVC" = "High-value crops", "OA" = "Other agriculture", "F" = "Forestry", "EXT" = "Extensive", "LTD"= "Limited", "N" = "None", "EXT and HVC" = "High-value crops, Extensive", "EXT and F" = "Forestry, Extensive", "LTD and OA" = "Other agriculture, Limited")) +
geom_text(aes(label=Tag), x = Inf, y = -Inf, hjust = -0.5, vjust = 1.2, inherit.aes = FALSE, colour="grey30") +
scale_y_continuous(limits = c(-25,25), breaks=c(-20, -10, 0, 10, 20))

pS1

#Fig.S2 shifts agg economies
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/Shifts_aggEconomies_for_R_plot.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE)
View(trial)
sapply(trial, class)
trial$Index <- as.factor(trial$Index)
trial$State <- as.factor(trial$State)
trial$Condition <- as.factor(trial$Condition)
trial$Tag <- as.factor(trial$Tag)
sapply(trial, class)
trial$Condition <- factor(trial$Condition,levels = c("F","OA", "HVC", "N","LTD","EXT", "LTD and OA", "EXT and F", "EXT and HVC"))

pS2 <- ggplot(trial, aes(x=Condition, y=Shift, fill = State)) +
geom_col(width = 0.5) +
facet_grid(Tag~., scales = "free", space = "free") +
coord_flip() +
theme_bw () +
labs (y = "Shift %") +
scale_fill_manual(name = "Agglomeration \neconomies",values =c("#91f2c9", "#2dbca5", "#228592"),
limits=c("Low", "Medium", "High")) +
theme(axis.text.y = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.text.x = element_text(family = NULL, size = rel(1.4), colour = "grey30"),
axis.title.x = element_text (family = NULL, size = rel(1.19), colour = "grey30", margin = margin(t = 15, b = 0)),
legend.text = element_text(family = NULL, size = rel(1.19), colour = "grey30"),
legend.title = element_text(family = NULL, size = rel(1.19), colour = "grey30"),
axis.title.y = element_blank(),
axis.ticks = element_blank(),
strip.background = element_blank(),
strip.text.y= element_blank(),
panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
scale_x_discrete(labels = c("HVC" = "High-value crops", "OA" = "Other agriculture", "F" = "Forestry", "EXT" = "Extensive", "LTD"= "Limited", "N" = "None", "EXT and HVC" = "High-value crops, Extensive", "EXT and F" = "Forestry, Extensive", "LTD and OA" = "Other agriculture, Limited")) +
geom_text(aes(label=Tag), x = Inf, y = -Inf, hjust = -0.5, vjust = 1.2, inherit.aes = FALSE, colour="grey30") +
scale_y_continuous(limits = c(-20,20), breaks=c(-20, -10, 0, 10, 20))

pS2

#Fig.S3 investment number charts
trial <- read.csv("C:/Users/Abeygunawardane/OneDrive/BN paper/Data_and_Code/Grpahs/Data/BNpaper_graph1_data.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", ""), fill = TRUE, check.names=FALSE)
long<-reshape(trial,
              varying = c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
              v.names = "Value",
              timevar = "Year",
              times = c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"), new.row.names = 1:187,
              direction = "long")
View(long)
deal_no <-subset(long, Country == "Ethiopia"| Country == "Mozambique"| Country == "Tanzania"| Country == "Zambia", select = c("Country", "Item", "Tag", "Unit", "Year", "Value"))
View(deal_no)
sapply(deal_no, class)
deal_no$Year <- as.factor(deal_no$Year)
deal_no$Unit <- as.factor(deal_no$Unit)
deal_no$Item <- as.factor(deal_no$Item)
deal_no$Country <- as.factor(deal_no$Country)
deal_no$Tag <- as.factor(deal_no$Tag)
sapply(deal_no, class)

p9 <- ggplot(deal_no,aes(fill= Country, y=Value, x=Year)) +
  facet_wrap(~Item, scales = "free_y", nrow = 2, strip.position = "left") +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =c("#91f2c9", "#2dbca5", "#228592", "#205164")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(family = NULL, color = "grey30", size = rel (1.5)),
    axis.text.y = element_text(family = NULL, color = "grey30", size = rel (1.5)),
    legend.text = element_text(family = NULL, color = "grey30", size = rel (1.2)),
    strip.text.y= element_text(family = NULL, size = rel(1.5), colour = "grey30", vjust = 1.5),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    strip.placement = "outside",
    legend.position= "bottom",
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
  scale_x_discrete(breaks =c("2000", "2004", "2008", "2012", "2016")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  geom_text(aes(label=Tag), x = -Inf, y = Inf, hjust = -0.5, vjust = 1.5, inherit.aes = FALSE, colour="grey30")
p9

p9 <- ggplot(data = subset (deal_no,Item == "Number of deals"),
             aes(fill=Country, y=Value, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =c("#91f2c9", "#2dbca5", "#228592", "#205164")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(color = "grey30", size = rel (1.5)),
    axis.text.y = element_text(color = "grey30", size = rel (1.5)),
    axis.title.y = element_text(family = NULL, size = rel (1.5), colour = "grey30", hjust = 0.5, vjust = 2),
    legend.text = element_text(color = "grey30", size = rel (1.2)),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(), 
    panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
  labs(y = "Number of investments") +
  scale_x_discrete(breaks =c("2000", "2004", "2008", "2012", "2016"))
p9

library(scales)
p10 <- ggplot(data = subset (deal_no,Item == "Area of deals"),
              aes(fill=Country, y=Value, x=Year)) + geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values =c("#91f2c9", "#2dbca5", "#228592", "#205164")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(color = "grey30", size = rel (1.5)), axis.text.y = element_text(color = "grey30", size = rel (1.2)),
    axis.title.y = element_text(family = NULL, size = rel (1.5), colour = "grey30", hjust = 0.5, vjust = 2),
    legend.text = element_text(color = "grey30", size = rel (1.2)),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.border = element_rect(color = "#828282", fill = NA, size = 0.2)) +
  labs(y = "Area of investments (M ha)") +
  scale_x_discrete(breaks =c("2000", "2004", "2008", "2012", "2016")) +
  scale_y_continuous(labels = unit_format(scale = 1e-6,unit = "", (accuracy = 0.5)))

trial <- read.csv("final.csv", header = TRUE, sep = ",", strip.white = TRUE, na.strings = c("NA", "", "NILL"), fill = TRUE)
trial_filter = select(trial, -c(3:6, 9))



trial_long<- trial_filter %>%
group_by(ccifid, CCI) %>%
summarise(cci_popden=first(cci_popden),ccifszmean =first(ccifszmean),ccimktmean = first(ccimktmean), x=first(x), y=first(y), N= n()) %>%
mutate (cciprop = N / sum(N))

write.csv(trial_long,'four_var_long.csv', na = "NA")

trial_wide <- trial_long %>% pivot_wider(names_from=CCI, values_from=cciprop)
trial_wide
write.csv(trial_wide,'four_var_wide.csv', na = "NA")

col_nms=c("130", "100", "110", "120", "40", "60", "50")
trial_wide$unconverted<-rowSums(trial_wide[,col_nms], na.rm = TRUE)

trial_wide_colforBN = select(trial_wide, -c(8:36))
trial_wide_colforBN <- mutate(trial_wide_colforBN, f_sz_mean_recalc = ccifszmean - 3502)
View(trial_wide_colforBN)

write.csv(trial_wide_colforBN,'spatial_var_ready_for_BN.csv', na = "NA")

library(pastecs)
stat.desc(trial)