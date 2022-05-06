##Convert ASR

filePath <- file.choose()

data = readxl::read_excel(filePath)
rawdata = as.data.frame(data)

IDs<-unique(raw_data$Subject_ID) ##get unique subjects
Trial_ID<-unique(raw_data$Trial_ID) ##get unique groups


output_data<-data.frame(subjects=IDs) ##create data storage


###for one variable

i<-0
for(id in IDs){ ##loop over unique subjects
  i<-i+1 #create tracer
  
  subset_data<-raw_data[raw_data$Subject_ID==id,] ##subset data by subject
  
  for(j in 1:length(Trial_ID)){
    
    t_id<-Trial_ID[j]
    
    k<-j+1 ##add value of 1 so that you don't overwrite "subjects" column

    output_data[i,k]<-mean(subset_data$mV_Max[subset_data$Trial_ID==t_id],na.rm=T) ##calculate mean for jth trial

    
  }
  
  
  
  
}

colnames(output_data)[-c(1)]<-paste(Trial_ID,"mv_Max",sep="_")
output_data


###for multiple variables

###list variables of interest
vars_of_interest<-c("V_Start","mV_Max","T_Max","mV_Avg")
output_data<-data.frame(subjects=IDs) ##create data storage


i<-0
var_index<-1

for(id in IDs){ ##loop over unique subjects
  i<-i+1 #create tracer
  
  subset_data<-raw_data[raw_data$Subject_ID==id,] ##subset data by subject
  

     for(j in 1:length(Trial_ID)){
    
          t_id<-Trial_ID[j]

          for(var in vars_of_interest){
            
            output_data2<-output_data
            
            output_data2[i,2]<-mean(subset_data[,var][subset_data$Trial_ID==t_id],na.rm=T) ##calculate mean for jth trial
    
            if(var_index==1){
               main_data<-output_data2
             }else{
               main_data<-cbind(main_data,output_data2[,-c(1)])
             }
         
            
         var_index<-var_index+1 ##create tracer for var loop
         colnames(main_data)[var_index]<-paste(t_id,"_",var,sep="") ##record group and trial IDs
    
     }
    
    
    
    
  }
  
  
}

main_data












###more complex function
###for multiple variables

###list variables of interest
calc_diff<-function(data,numerator,denominator){
  
  return_values<-100-((data[,numerator]/denominator)*100)
  
  return(return_values)
  
}


vars_of_interest<-c("mV_Max")
denom<-"mV_Max"

AS_50_dat<-raw_data[raw_data$Trial_ID=="AS50",]

output_data<-data.frame(trials=unique(raw_data[,c("Trial_ID","Subject_ID")])) ##create data storage


i<-0
var_index<-1

for(id in IDs){ ##loop over unique subjects
  i<-i+1 #create tracer
  
  subset_data<-raw_data[raw_data$Subject_ID==id,] ##subset data by subject
  subset_as50<-AS_50_dat[AS_50_dat$Subject_ID%in%id,]
  
  for(j in 1:length(Trial_ID)){
    
    t_id<-Trial_ID[j]
    
    for(var in vars_of_interest){
      
      output_data2<-subset_data[subset_data$Trial_ID==t_id,]
      
      output_data2[,ncol(output_data2)+1]<-calc_diff(data=subset_data[subset_data$Trial_ID==t_id,],
                                   numerator=var,
                                   denominator=subset_as50$mV_Max) 
      
      if(var_index==1){
        main_data<-output_data2
      }else{
        main_data<-rbind(main_data,output_data2)
      }
      
      
      var_index<-var_index+1 ##create tracer for var loop

    }
    
    
    
    
  }
  
  
}

main_data


wide_data<-reshape(main_data[,c("Trial_ID","Subject_ID","V18")], idvar = "Subject_ID", timevar = "Trial_ID", direction = "wide")
