require(data.table)
fin.dt <- fread('C:/Users/Tom/OneDrive/OD_Documents/EfficientPortfolioStuff/EffPort_R_BarChart/R_POC/Data/fin_data.csv')

str(fin.dt)
fincross.dt <- dcast(fin.dt, date~ticker, value.var = 'price')
head(fincross.dt)
head(fin.dt)
