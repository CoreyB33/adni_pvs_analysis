######### Read in libraries #########
library(ggplot2)
library(dplyr)


######### Read in dataframes #########
# Read in PVS data
df <- read.csv('C:/Users/bowncw/Desktop/VMAC/ADNI/adni_pvs_data.csv')

# Read in ADNI data
adnimerge <- read.csv('C:/Users/bowncw/Desktop/VMAC/ADNI/ADNIMERGE.csv')

# Update column names for PVS data
colnames(df) <- c('adni.session.id','basal.ganglia.pvs.vol','basal.ganglia.pvs.count')

# Create visit code from session id
df$adni.id <- strtrim(df$adni.session.id, 4)
df$rid <- as.numeric(df$adni.id)
df$viscode <- gsub("[0-9]{4}","",df$adni.session.id)
df$viscode <- gsub("[0-9]{3}","",df$viscode)
df$viscode <- gsub("_","",df$viscode)
df$viscode <- gsub("M","m",df$viscode)

# Remove rid of 0
df <- df[!df$rid==0,]

######### Identify Baseline Image for Each Participant #########
# Create dataframe for each new viscode and subset them based on new rids that do not exist
# in previous dataframes
df_sc <- df %>% group_by(rid) %>% filter(viscode=="sc")

df_gom03 <- df %>% group_by(rid) %>% filter(viscode=="GOm03")
df_gom03 <- df_gom03 %>% filter(!(rid %in% df_sc$rid))

df_gom06 <- df %>% group_by(rid) %>% filter(viscode=="GOm06")
df_gom06 <- df_gom06 %>% filter(!(rid %in% df_sc$rid))
df_gom06 <- df_gom06 %>% filter(!(rid %in% df_gom03$rid))

df_gom12 <- df %>% group_by(rid) %>% filter(viscode=="GOm12")
df_gom12 <- df_gom12 %>% filter(!(rid %in% df_sc$rid))
df_gom12 <- df_gom12 %>% filter(!(rid %in% df_gom03$rid))
df_gom12 <- df_gom12 %>% filter(!(rid %in% df_gom06$rid))

df_gom18 <- df %>% group_by(rid) %>% filter(viscode=="GOm18")
df_gom18 <- df_gom18 %>% filter(!(rid %in% df_sc$rid))
df_gom18 <- df_gom18 %>% filter(!(rid %in% df_gom03$rid))
df_gom18 <- df_gom18 %>% filter(!(rid %in% df_gom06$rid))
df_gom18 <- df_gom18 %>% filter(!(rid %in% df_gom12$rid))

df_gom24 <- df %>% group_by(rid) %>% filter(viscode=="GOm24")
df_gom24 <- df_gom24 %>% filter(!(rid %in% df_sc$rid))
df_gom24 <- df_gom24 %>% filter(!(rid %in% df_gom03$rid))
df_gom24 <- df_gom24 %>% filter(!(rid %in% df_gom06$rid))
df_gom24 <- df_gom24 %>% filter(!(rid %in% df_gom12$rid))
df_gom24 <- df_gom24 %>% filter(!(rid %in% df_gom18$rid))

df_gom36 <- df %>% group_by(rid) %>% filter(viscode=="GOm36")
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_sc$rid))
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_gom03$rid))
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_gom06$rid))
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_gom12$rid))
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_gom18$rid))
df_gom36 <- df_gom36 %>% filter(!(rid %in% df_gom24$rid))

df_gom48 <- df %>% group_by(rid) %>% filter(viscode=="GOm48")
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_sc$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom03$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom06$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom12$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom18$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom24$rid))
df_gom48 <- df_gom48 %>% filter(!(rid %in% df_gom36$rid))

df_2init <- df %>% group_by(rid) %>% filter(viscode=="2init")
df_2init <- df_2init %>% filter(!(rid %in% df_sc$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom03$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom06$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom12$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom18$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom24$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom36$rid))
df_2init <- df_2init %>% filter(!(rid %in% df_gom48$rid))

df_2sc <- df %>% group_by(rid) %>% filter(viscode=="2sc")
df_2sc <- df_2sc %>% filter(!(rid %in% df_sc$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom03$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom06$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom12$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom18$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom24$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom36$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom36$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_gom48$rid))
df_2sc <- df_2sc %>% filter(!(rid %in% df_2init$rid))

df_2m03 <- df %>% group_by(rid) %>% filter(viscode=="2m03")
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_sc$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom03$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom06$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom12$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom18$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom24$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom36$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom36$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_gom48$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_2init$rid))
df_2m03 <- df_2m03 %>% filter(!(rid %in% df_2sc$rid))

df_2m06 <- df %>% group_by(rid) %>% filter(viscode=="2m06")
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_sc$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom03$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom06$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom12$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom18$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom24$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom36$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom36$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_gom48$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_2init$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_2sc$rid))
df_2m06 <- df_2m06 %>% filter(!(rid %in% df_2m03$rid))

df_2m12 <- df %>% group_by(rid) %>% filter(viscode=="2m12")
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_sc$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom03$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom06$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom12$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom18$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom24$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom36$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom36$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom48$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2init$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2sc$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2m03$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2m06$rid))

df_2m12 <- df %>% group_by(rid) %>% filter(viscode=="2m12")
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_sc$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom03$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom06$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom12$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom18$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom24$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom36$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom36$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_gom48$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2init$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2sc$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2m03$rid))
df_2m12 <- df_2m12 %>% filter(!(rid %in% df_2m06$rid))

# Bind the separate dataframes together
df_bind <- rbind(df_sc,df_gom03,df_gom06,df_gom12,df_gom18,df_gom24,df_gom36,
                 df_gom48,df_2init,df_2m03,df_2m06,df_2m12,df_2sc)

remove(df_sc,df_gom03,df_gom06,df_gom12,df_gom18,df_gom24,df_gom36,
       df_gom48,df_2init,df_2m03,df_2m06,df_2m12,df_2sc)

######### Merge by rid and viscode #########
# Change viscode for sc to bl
df_bind$viscode <- gsub("sc","bl",df_bind$viscode)

# Get rid of GO tag
df_bind$viscode <- gsub("GO","",df_bind$viscode)

# Change 2bl to bl
df_bind$viscode <- gsub("2bl","bl",df_bind$viscode)


# Need to create an array of all participants with 2init viscodes,
# loop through them in the ADNI merge dataset, identify the earliest ADNI2 scan,
# take the mXX tag of that scan, and use it in place of 2init

init <- c(df_bind$rid[df_bind$viscode=="2init"])

for(i in init)
{
  subset <- adnimerge[adnimerge$RID==i,]
  subset_2 <- subset[subset$COLPROT=="ADNI2",]
  subset_2$VISCODE <- gsub("m","",subset_2$VISCODE)
  subset_2$VISCODE <- as.numeric(subset_2$VISCODE)
  m <- min(subset_2$VISCODE)
  df_bind$viscode[df_bind$rid==i] <- paste("m",m,sep="")
}

# Now changing 2m03 to m03
df_bind$viscode <- gsub("2m03","m03",df_bind$viscode)

# Now changing 2m06 to m06
df_bind$viscode <- gsub("2m06","m06",df_bind$viscode)


# Now changing 2m12 to m12, only 10 participants with 2m12 so will change custom
df_bind$viscode[df_bind$rid==454] <- gsub("2m12","m78",df_bind$viscode[df_bind$rid==454])
df_bind$viscode[df_bind$rid==519] <- gsub("2m12","m72",df_bind$viscode[df_bind$rid==519])
df_bind$viscode[df_bind$rid==731] <- gsub("2m12","m72",df_bind$viscode[df_bind$rid==731])
df_bind$viscode[df_bind$rid==1155] <- gsub("2m12","m72",df_bind$viscode[df_bind$rid==1155])
df_bind$viscode[df_bind$rid==2010] <- gsub("2m12","m24",df_bind$viscode[df_bind$rid==2010])
df_bind$viscode[df_bind$rid==2073] <- gsub("2m12","m24",df_bind$viscode[df_bind$rid==2073])
df_bind$viscode[df_bind$rid==2133] <- gsub("2m12","m24",df_bind$viscode[df_bind$rid==2133])
df_bind$viscode[df_bind$rid==4196] <- gsub("2m12","m12",df_bind$viscode[df_bind$rid==4196])
df_bind$viscode[df_bind$rid==4225] <- gsub("2m12","m12",df_bind$viscode[df_bind$rid==4225])
df_bind$viscode[df_bind$rid==4417] <- gsub("2m12","m12",df_bind$viscode[df_bind$rid==4417])

# Remove minf
df_bind <- df_bind[df_bind$viscode!="mInf",]



# Merge df_bind and adnimerge

adnimerge <- merge(adnimerge,df_bind,by.x=c("RID","VISCODE"),by.y=c("rid","viscode"))

df_cn <- adnimerge[adnimerge$DX=="CN",]


# Ended up with n=793, more than double VMAP
# Start building models
mod_1 <- lm(ADAS13~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_1)
# p=0.06

mod_2 <- lm(ADAS11~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_2)
# p=0.04

mod_3 <- lm(ADASQ4~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_3)
# p=0.41

mod_4 <- lm(MMSE~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_4)
# p=0.05

mod_5 <- lm(RAVLT_immediate~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_5)
# p=0.08

mod_6 <- lm(RAVLT_learning~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_6)
# p=0.92

mod_7 <- lm(RAVLT_forgetting~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_7)
# p=0.75

mod_8 <- lm(DIGITSCOR~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_8)
# p=0.93

mod_9 <- lm(TRABSCOR~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_9)
# p=0.01

p <- ggplot(data = adnimerge, aes(x = basal.ganglia.pvs.vol, y = TRABSCOR,colour=DX))
p + geom_point() + geom_smooth(method=lm)



mod_10 <- lm(EcogPtMem~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_10)
# p=0.92

mod_11 <- lm(EcogPtLang~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_11)
# p=0.72

mod_12 <- lm(EcogPtVisspat~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_12)
# p=0.26

mod_13 <- lm(EcogPtPlan~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_13)
# p=0.21

mod_14 <- lm(EcogPtOrgan~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_14)
# p=0.95

mod_15 <- lm(EcogPtDivatt~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_15)
# p=0.54

mod_16 <- lm(mPACCdigit~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_16)
# p=0.36

mod_17 <- lm(mPACCtrailsB~PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_17)
# p=0.03

mod_18 <- lm(mPACCtrailsB~ICV+PTETHCAT+PTEDUCAT+PTGENDER+APOE4+DX_bl+AGE+basal.ganglia.pvs.vol,data=adnimerge)
summary(mod_18)
# p=0.07
