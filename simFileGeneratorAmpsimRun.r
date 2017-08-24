simFileGeneratorApsimRun<-function(crop,country){

# generate .sim files from templates by inserting the reference to .met files

# set the dir where the source files are stored
# It is assumed that this directory has the following structure:
# MET_SAgrids
#  |__ all the metfiles.met
# simTemplates
#   |___early
#     |__ all the simtemplates.sim
#  
#
# the result of this script is a new directory with the following structure:
# simFiles
# |___gridpoint
#   |___early
#    |___medium
#    |___late
#     |__ all the simfiles.sim
#  

Sys.setenv(
  PATH = paste(
    Sys.getenv("PATH"),
    "C:\\Program Files (x86)\\Apsim74-r2286\\Model",
    sep = ";"
  )
)

setwd("C:/Users/User/Desktop/Aspwork")

if (crop %in% c("soybean","bean","cowpea")){
  forCult=c("early")
  if(crop=="soybean"){
    mulTime=4
  }else{
    mulTime=3
    }
} else{
  forCult=c("early","medium","late")
  mulTime=5
}

workDir = getwd()

# read the dir with the .met files and put in list
metDir<-file.path(paste("C:/Users/User/Dropbox/CropDST/APSIM/MET_",country,"grids",sep=""))
listOfMetFiles <- list.files(metDir)
simFilesDir=paste("simFiles",country,crop,sep="")
simTemplatesDir=paste("simTemplates",country,crop,sep="")
if(!dir.exists(file.path(workDir,simFilesDir))){
  dir.create(file.path(workDir,simFilesDir))
}
apsimResultDir<-file.path(paste("C:/Users/User/Desktop/Aspwork/",simFilesDir,sep=""))
listOfDoneFiles <- list.files(apsimResultDir)

procNo <-15 # number of processors


counter <-1
simCount<-1
countDone <-0

start<-as.numeric(Sys.time())
for (metGrid in listOfMetFiles){
  metGrid <- substr(metGrid,1,nchar(metGrid)-4)
  if(metGrid %in% listOfDoneFiles){
    print(paste("Already done ", metGrid))
    counter<-counter+1
    countDone<-countDone+1
  }
  else{
  print(paste("processing ", metGrid))
  setwd(workDir)
  print(paste(counter," of ",length(listOfMetFiles), sep=""))
  print(paste("processed .sim Files:",simCount, sep=" "))
  counter<-counter+1
  
  for (cult in forCult){
    print (cult)
    dir.create(file.path(workDir,simFilesDir, metGrid, cult), recursive=TRUE, showWarnings=FALSE)
    for (soil in list.files(file.path(workDir,simTemplatesDir,cult))){
      # open new .sim file to start writing into
      outPath <- file.path(workDir,simFilesDir,metGrid,cult)
      fout<-file(file.path(outPath,soil), open ="w")
      # read the template .sim file line by line
      simPath<-file.path(workDir,simTemplatesDir,cult,soil)
      fin<-file(simPath, open="r")
      for (l in readLines(fin, warn=FALSE)){
        # if line contains <filename> replace line
        if(grepl("<filename name=",l)){
          writeLines(paste("      <filename name=\"filename\" input=\"yes\">",file.path(metDir,metGrid),".met</filename>",sep=""),con=fout)
        }
        # else write the line as is from the template
        else{
          writeLines(l,con=fout)
        }
      }
      #close both files
      close(fin)
      close(fout)
      
      # run apsim on the .sim file
      # determine if run can be sequential (wait = FALSE) or not (wait=TRUE)
      ifelse(simCount%%procNo==0,wait<-TRUE,wait<-FALSE)
      #go to correct dir
      setwd(outPath)
      # run apsim on the .sim file without generating .sum file
      shell(shQuote(paste("Apsim.exe ",soil," > nul",sep="")),intern=FALSE, wait=wait, invisible=TRUE)

      # keep track of number of simfiles that are processed
      simCount <- simCount+1
      current<-as.numeric(Sys.time())
      processTime<-current-start
      if (wait == TRUE){
        print(paste("time per .sim file:", round(processTime/simCount,digits=1), " seconds"))
        print(paste("time to go: ", round(processTime/simCount*((length(listOfMetFiles)-countDone)*length(forCult)*27*mulTime-simCount)/60/60,digits=1), " hours"))
        print(paste("processed .sim Files:",simCount, sep=" "))
      }  
      setwd(workDir)
    }
    
  }
 } # end of else
} # end of for loop

} # end of function



# use country code used in the metGrid Dir names
# makse sure the sim templates are in a dir named: simTemplatesCOUNTRYCODEcrop
simFileGeneratorApsimRun("soybean","MWI")
