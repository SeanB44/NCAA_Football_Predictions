source('./Functions.R')
source('./Point Differential Model.R')

### Model to use ###
summary(final.model)
PointDiff.model<-final.model

source('./Win Probability Model.R')
summary(prob.model)
WinProb.model<-prob.model

# Load Workbook
wb<-loadWorkbook('../Tools/CFB Prediction Tool - Week 8 - 2023 (2023-10-23).xlsx')

### Apply Function and Export Current Workbook ###
## Inputs
model<-PointDiff.model
WinProb<-WinProb.model
year <-2023
wk<-9
date <- today()
wb<-wb
# Run Function
Gen_Tool_cfb(model,WinProb, year, wk, date, wb = wb)             
