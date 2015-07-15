library(plm)
crime <- read.delim("F:/Cornwell and Trumbull/nc_crime.csv")
head(crime)
class(crime)

#Replicating test of Baltagi's (2009)



#Fixed effect with time dummies
baltagi<-lcrmrte~lprbarr+lprbconv+lprbpris+lpolpc+ldensity+lwtuc+lwmfg+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87
baltagi.fe<-plm(baltagi, data=crime , model="within")
summary(baltagi.fe)   #Baltagi column 2 table 2 page 6
summary(fixef(baltagi.fe))
crime$r<-resid(baltagi.fe)


#Create ledas and lags for all variables
data81<-subset(crime,crime$year==81)
data82<-subset(crime,crime$year==82)
data83<-subset(crime,crime$year==83)
data84<-subset(crime,crime$year==84)
data85<-subset(crime,crime$year==85)
data86<-subset(crime,crime$year==86)
data87<-subset(crime,crime$year==87)


#3SLS estimation

r81<-lm(data81$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)
summary(r81)


r82<-lm(data82$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)
summary(r82)

r83<-lm(data83$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)
summary(r83)


r84<-lm(data84$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)
summary(r84)


r85<-lm(data85$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)
summary(r85)


r86<-lm(data86$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)

summary(r86)


r87<-lm(data87$r~data81$lprbarr+data82$lprbarr+data83$lprbarr+data84$lprbarr+data85$lprbarr+data86$lprbarr+data87$lprbarr+
          data81$lprbconv+data82$lprbconv+data83$lprbconv+data84$lprbconv+data85$lprbconv+data86$lprbconv+data87$lprbconv+
          data81$lprbpris+data82$lprbpris+data83$lprbpris+data84$lprbpris+data85$lprbpris+data86$lprbpris+data87$lprbpris+
          data81$lpolpc+data82$lpolpc+data83$lpolpc+data84$lpolpc+data85$lpolpc+data86$lpolpc+data87$lpolpc+
          data81$ldensity+data82$ldensity+data83$ldensity+data84$ldensity+data85$ldensity+data86$ldensity+data87$ldensity+
          data81$lwtuc+data82$lwtuc+data83$lwtuc+data84$lwtuc+data85$lwtuc+data86$lwtuc+data87$lwtuc+
          data81$lwmfg+data82$lwmfg+data83$lwmfg+data84$lwmfg+data85$lwmfg+data86$lwmfg+data87$lwmfg+
          data81$lpctmin+data82$lpctmin+data83$lpctmin+data84$lpctmin+data85$lpctmin+data86$lpctmin+data87$lpctmin+
          data81$west+data82$west+data83$west+data84$west+data85$west+data86$west+data87$west+
          data81$central+data82$central+data83$central+data84$central+data85$central+data86$central+data87$central+
          data81$urban+data82$urban+data83$urban+data84$urban+data85$urban+data86$urban+data87$urban)

summary(r87)


sr81<-(summary(r81)$r.squared)*(summary(r81)$df[2])
sr82<-(summary(r82)$r.squared)*(summary(r82)$df[2])
sr83<-(summary(r83)$r.squared)*(summary(r83)$df[2])
sr84<-(summary(r84)$r.squared)*(summary(r84)$df[2])
sr85<-(summary(r85)$r.squared)*(summary(r85)$df[2])
sr86<-(summary(r86)$r.squared)*(summary(r86)$df[2])
sr87<-(summary(r87)$r.squared)*(summary(r87)$df[2])

Statistics<-sr81+sr82+sr83+sr84+sr85+sr86+sr87
Statistics
