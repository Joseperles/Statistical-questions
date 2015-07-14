library(plm)
crime <- read.delim("F:/Cornwell and Trumbull/nc_crime.csv")
head(crime)
model<-lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban
modelt<-lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87

#Pooled model
panel.ols<-plm(model, data=crime , model="pooling")
summary(panel.ols)

#Between effects reproduce exactly the data of the paper
panel.be<-plm(model, data=crime , model="between")
summary(panel.be)  #Baltagi column 1 table 7.1 page 133

#Fixed effect with time dummies
panel.fe<-plm(modelt, data=crime , model="within")
summary(panel.fe)   #Baltagi column 2 table 7.1 page 133
summary(fixef(panel.fe))

#Random effects
panel.re<-plm(modelt, data=crime , model="random")
summary(panel.re)

#Hausman test comparing time effects model and random effects
phtest(panel.fe, panel.re)

#Test for individual and time effects

plmtest(panel.ols, effect="individual", type="bp")
plmtest(panel.ols, effect="individual", type="honda")


#Instrumental variables estimation


iv.fe<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="within")
summary(iv.fe)  #FE2SLS Baltagi Table 7.1 p.133 column 3

iv.be<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="between")
summary(iv.be)  #BE2SLS Baltagi Table 7.1 p.133 column 4

iv.re<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="random", inst.method="baltagi")
summary(iv.re) #BE2SLS Baltagi Table 7.1 p.133 column 5


