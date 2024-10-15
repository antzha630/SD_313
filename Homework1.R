# Anthony Zhang
# UT eid : az22589

bond <- read.csv('Homework1_Bonds.csv')
head(bond)

# 1
# display how many bonds carried out vs defeat
table(bond$Result)
# dataframe with only rows with that specific type of gov
cities <- bond[bond$Type=='CITY',]
isd <- bond[bond$Type=='ISD',]
wd <- bond[bond$Type == "WD",]
county <- bond[bond$Type == "COUNTY",]
#display proportion of carried out vs defeated (percentage)
prop.table(table(cities$Result))
prop.table(table(isd$Result))
prop.table(table(wd$Result))
prop.table(table(county$Result))


#2
# make new column in bonds.csv for total of votes for and votes against
bond$Votes_Total <- bond[,"VotesFor"] + bond[, "VotesAgainst"]
# which row number had the most votes total 
# which gives the row number for the condition
x <- bond$Votes_Total
y <- max(bond$Votes_Total)
z <- which(x == y)
print(z)
# prints the entire row of the row number with the most votes total
bond[z,]


#3
#subset of bonds that were carried out and had total votes of over 100 in that row
goodbonds <- bond[(bond$Votes_Total >= 100) & (bond$Result == "Carried"),]
# add column in good bonds for percentage of total votes that were for the bond measure
goodbonds$percent <- (goodbonds$VotesFor / goodbonds$Votes_Total) * 100
# histogram for Margin of Bond Passing vs Frequency 
hist(goodbonds$percent,main='Histogram of Percent',xlab='Margin of Bond Passing (percentage)',ylab = 'Frequency',col='Blue',xlim=c(0,100))
## median used for center and IQR used for spread bc its skewed right
median(goodbonds$percent)
fivenum(goodbonds$percent)
IQR(goodbonds$percent)

#4
# display cost vs margin a bond approved
plot(goodbonds$Amount,goodbonds$percent,main='Cost vs Margin Bond Approved (Percent)',xlab='Cost in $',ylab='Margin Bond Approved (percent)',pch=20)
# find correlation coefficient which describes how correlated the two vars are
# around 0 for correlation coefficient means weak to no linear relationship between vars
cor(goodbonds$Amount,goodbonds$percent)
