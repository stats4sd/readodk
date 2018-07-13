############################################ START ######################################################

############################################ readodk.R ################################################

# Version: 0.5.1, "Tajikistan" 
# Changes to previous version:  Complete overhaul of odkFormat: output is now a labeled data frame, the XLSForm 
#                               and a codebook using expss package
# Built under R version 3.4.3

#### Queries: Sebastian Steinmueller, s.steinmuller@stats4sd.org

#### Functions to read in and manage data coming from ODK platforms


### function odkFormat
### Sets data format in data based on types in survey tab of XLSform, creates labeled data set and codebook
### Arguments:  form:       path to XLSform as character string 
###                         OR object of class "form" created by readForm function
###             dat:        path to file as character string 
###             formout:    modified XLSForm included in output: TRUE or FALSE (default)
###             codebook:   include codebook list as slot in output: TRUE or FALSE (default)
###             del:        delimiter for group names used in data file exported from platform
###                         possible values: '/' (default) or any other character string
###             groupnames: TRUE if the data file has groupnames appended to its variable names, FALSE if not
###             variablenames:  "short":    remove group name and repeat group name appendixes from data file variable names 
###                                         and leave only variable names from name column in form survey tab (see also option "check.names")
###                             "long":     keep full group names and repeat group names in variable names
###                             "repnames": remove groupnames, but keep repeat group names
###                             "asis":     keep names from data (default)
###             language:   language for labels from form (text string of label column names in survey and choices tabs after "label::". Put "" if column name is just "label")
###             na.strings: vector of character strings in data file to be set to NA
###             check.names:Convert short variable names from data file to unqiue R compatible names (default TRUE, FALSE only possible for data from .xls or .xlsx files)
###             select_one to rdate: set XLSForm questions type to R type ('numeric', 'character', 'integer', 'factor', 'date', 'datetime')
###             platform:   aggregate platform used (currently unused, options for the future should be 'kobo' and 'ona' and possibly others)
### Value:      returns odkdata object with slots data (labeled data frames with expss variable labels and value labels 
###             as attributes for every variable), 
###             codebook (list) and form (list with two elements survey and choices)

#begin function odkFormat
odkFormat<-function(form, dat, tab = 1,
                    formout=F,
                    codebook=TRUE, 
                    groupnames = TRUE, 
                    variablenames="asis",
                    check.names=TRUE, 
                    na.strings = NULL, 
                    col_types="text",
                    del='/', 
                    language="English",
                    select_one='factor',
                    select_multiple='factor',
                    select_multiple_unsplit='character',
                    calculate='numeric',
                    integer='integer',
                    decimal='numeric',
                    text='character',
                    geopoint='character',
                    start='datetime',
                    end='datetime',
                    today='date',
                    rdate='date',
                    datetime='datetime',
                    dateTimeFormat="%Y-%m-%dT%H:%M:%OS", 
                    dateFormat= "%Y-%m-%d", 
                    origin="1899-12-30", 
                    select_multiple_codes=c('FALSE', 'TRUE'), 
                    select_multiple_labels=c('FALSE', 'TRUE'),
                    platform='kobo'){

  require(dplyr)
  require(readr)
  require(readxl)
  require(expss)
  require(stringr)
  
  # define class "odkdata"
  setClass(Class="odkdata",
           representation(
             data="data.frame",
             codebook="list",
             form="list"
           )
  )
  
  # specify language columns
  langname<-ifelse(language=="","label",paste0("label..",language))
  
  # read in files and create objects
  form<-revarnames(form, choicelist=T, groupnames = groupnames, del=del, language=language)
  survey<-form$survey
  choices<-form$choicelist
  
  #if(check.names){survey$shortname<-make.names(survey$shortname, unique = T)
  #                survey$longname<-make.names(survey$longname, unique = T)
  #                survey$repname<-make.names(survey$repname, unique = T)}
  
  dat<-readodkdata(dat, na.strings=na.strings, col_types = col_types, 
                                         tab=tab, check.names=check.names) 
  dat.varnames<-dat.varnames_ori<-dat@varnames
  ds<-dat@data
  
  # strip repeat group indicator ([*]) from varnames and substitute special characters
  dat.varnames<-str_replace_all(dat.varnames,'\\[\\d{1,}\\]','')
  
  if(codebook){cb<-list()}
  i<-0
  choicel<-0
  
  for(j in 1:dim(ds)[2]){
    
    # check number of NAs in variable
    warn1<-sum(is.na(ds[,j]))
    
    cursurvey<-filter(survey, varnames==dat.varnames[j])
    
    odktype<-cursurvey$type2
    if(!length(odktype)==1){odktype='empty'}
    
    choicename<-cursurvey$listname
    csvpull<-grepl('search', cursurvey$appearance, fixed=T)|grepl('pulldata', cursurvey$calculation, fixed=T)|grepl('concat', cursurvey$calculation, fixed=T)
    other<-ifelse(cursurvey$other=='or_other' & !is.na(cursurvey$other), TRUE, FALSE)
    
    # translate from ODK types to R types    
    rtype<-switch(odktype,
                  integer=integer,
                  decimal=decimal,
                  text=text,
                  geopoint=geopoint,
                  start=start,
                  end=end,
                  today=today,
                  date=rdate,
                  datetime=datetime,
                  select_one=select_one,
                  select_multiple=select_multiple,
                  select_multiple_unsplit=select_multiple_unsplit,
                  calculate=ifelse(csvpull,'empty' ,calculate),
                  'empty')
    if(odktype=='select_multiple' & rtype=='factor'){rtype<-'mfactor'}
    if(odktype=='select_multiple'){choicel<-length(choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),'name']) 
    if(i<choicel){i<-i+1}}
    
    if(rtype=='mfactor'){ds[,j]<-toupper(ds[,j])}
    varlab<-cursurvey[1,langname]
    
    # set variable type for unlabeled data
    # suppressWarnings added because of expss warning for factor labelling - should be removed at some point
    suppressWarnings(
      ds[,j]<-switch(rtype,
                   integer=set_var_lab(as.integer(as.character(ds[,j])), varlab),
                   numeric=set_var_lab(as.numeric(as.character(ds[,j])), varlab),
                   double=set_var_lab(as.numeric(as.character(ds[,j])), varlab),
                   float=set_var_lab(as.numeric(as.character(ds[,j])), varlab),
                   character=set_var_lab(as.character(ds[,j]), varlab),
                   text=set_var_lab(as.character(ds[,j]), varlab),
                   factor={if(!(choicename=='' | is.na(choicename) | is.null(choicename) | csvpull)){
                              (function(x){
                                  y<-as.factor(ds[,j])
                                  var_lab(y)<-varlab
                                  y.labs<-choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),'name']
                                  names(y.labs)<-choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),langname]
                                  val_lab(y)<-y.labs
                                  return(y)})(ds[,j])
                                } 
                        else {as.factor(ds[,j])}},
                   mfactor=(function(x){
                              y<-as.factor(ds[,j])
                              var_lab(y)<-varlab
                              y.labs<-select_multiple_codes
                              names(y.labs)<-select_multiple_labels
                              val_lab(y)<-y.labs
                              return(y)})(ds[,j]),
                   date=set_var_lab(as.Date(ds[,j], format=dateFormat, origin=origin), varlab),
                   datetime=set_var_lab(as.POSIXct(ds[,j], format=dateTimeFormat, origin=origin), varlab),
                   ds[,j])
    )
    
    # remove group names if specified
    if(variablenames=="short"&dim(cursurvey)[1]>0){colnames(ds)[j]<-cursurvey[1,"shortname"]}
    
    if(variablenames=="long"&dim(cursurvey)[1]>0){
      curvarname<-cursurvey[1,"longname"]
      if(!is.na(cursurvey$repeatgroups)){
        repeatgroups<-str_split(cursurvey$repeatgroups, " ", simplify = T)
        repl1<-str_extract(dat.varnames_ori[j], paste0(repeatgroups, '\\[\\d{1,}'))
        if(any(is.na(repl1))) repl<-repeatgroups else repl<-make.names(repl1)   
        names(repl)<-repeatgroups
        colnames(ds)[j]<-str_replace_all(curvarname, repl)
        }
      else 
        colnames(ds)[j]<-curvarname 
    }
    
    if(variablenames=="repnames"&dim(cursurvey)[1]>0){
      curvarname<-cursurvey[1,"repname"]
      if(!is.na(cursurvey$repeatgroups)){
        repeatgroups<-str_split(cursurvey$repeatgroups, " ", simplify = T)
        repl1<-str_extract(dat.varnames_ori[j], paste0(repeatgroups, '\\[\\d{1,}'))
        if(any(is.na(repl1))) repl<-repeatgroups else repl<-make.names(repl1)
        names(repl)<-repeatgroups
        colnames(ds)[j]<-str_replace_all(curvarname, repl)
        }
      else 
        colnames(ds)[j]<-curvarname 
      }
    
    # check if number of NAs changed by conversion
    if(warn1!=sum(is.na(ds[,j]))){warning(paste0("In variable ", cursurvey$name, "(j=", j, "): ", sum(is.na(ds[,j]))-warn1 ," NAs generated"))}
    

    # create codebook
    if(codebook){
      cb[[j]]<-list()
      cb[[j]][[1]]<-colnames(ds)[j]
      if(odktype=='select_multiple'){cb[[j]][[2]]<-paste(cb[[j]][[2]]<-cursurvey[,langname], 
                                                         choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),langname][i], 
                                                         sep=' - ')}else{
                                                           cb[[j]][[2]]<-cursurvey[,langname]}
      cb[[j]][[3]]<-cursurvey$type2
      cb[[j]][[4]]<-cursurvey$listname
      cb[[j]][[5]]<-switch(rtype,
                           integer=NA_character_,
                           numeric=NA_character_,
                           double=NA_character_,
                           float=NA_character_,
                           character=NA_character_,
                           text=NA_character_,
                           factor={if(!(choicename=='' | is.na(choicename) | is.null(choicename)))
                           {choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),'name']} else {levels(ds[,j])}},
                           mfactor=levels(ds[,j]),
                           date=NA_character_,
                           datetime=NA_character_,
                           NA_character_)
      #cb[[j]][[6]]<-list()
      cb[[j]][[6]]<-switch(rtype,
                           integer=NA_character_,
                           numeric=NA_character_,
                           double=NA_character_,
                           float=NA_character_,
                           character=NA_character_,
                           text=NA_character_,
                           factor={if(!(choicename=='' | is.na(choicename) | is.null(choicename)))
                           {choices[[choicename]][which(!duplicated(choices[[choicename]]$name)),langname]} else {levels(ds[,j])}},
                           mfactor=levels(ds[,j]),
                           date=NA_character_,
                           datetime=NA_character_,
                           NA_character_)
      cb[[j]][[7]]<-dat.varnames[j]
      cb[[j]][[8]]<-cursurvey$name
      names(cb[[j]])<-c('Variable name in R', 'Question text (label) in XLSForm', 'Variable type in XLSForm', 'Choice list name in XLSForm', 
                        'Codes', 'Code labels', 'Variable name in platform export', 'Variable name in XLSForm')
    }
    if(i==choicel & odktype=='select_multiple'){i<-0} 
  }
  
  # label codebook
  if(codebook){names(cb)<-colnames(ds)}
  
  if(check.names){colnames(ds)<-make.names(colnames(ds))}
  
  # return odkdata-object with slots for labeled data and codebook
  if(codebook){
    if(formout){
    return(new("odkdata", data=ds, codebook=cb, form=form))
    }
    else{
    return(new("odkdata", data=ds, codebook=cb, form=list()))
    }
  }
  else{
    if(formout){
      return(new("odkdata", data=ds, codebook=list(), form=form))
    }
    else{
      return(new("odkdata", data=ds, codebook=list(), form=list()))
    }
  }
}
#end function odkFormat





### function readform(filename)
### reads the two tabs "survey" and "choices" from a XLSform to be used in data management later on
### tabs have to be named "survey" and "choices"
### Arguments:  form:   path to xlsx as character string
###             languages:  character vector of languages provided in the form (as written behind label:: in the form)
###             ...:        other arguments to be passed on to read.xlsx
### Value:      returns object of newly defined class "form" with objects "survey" and "form"
###             access these with @
### requries xlsx package

#begin function readForm
readForm<-function(form){
  require(readxl)
  
  # define class "form"
  
  setClass(Class="form",
           representation(
             survey="data.frame",
             choices="data.frame"
           )
  )
  
  # read in sheets 
  survey<-as.data.frame(read_excel(form, sheet="survey", col_types = "text", trim_ws = F))
  choices<-as.data.frame(read_excel(form, sheet="choices", col_types = "text", trim_ws = F))
  
  # return object of class "form"
  return(new("form",survey=survey, choices=choices))
  
}
#end function readForm





### function readodkdata(filepath)
### reads in data from csv downloaded from platform and preserves extra vector of original variable names 
### Arguments:  filepath:   path to excel or csv as character string
### Value:      returns object of newly defined class "odkdata" with objects "data" and "varnames"
###             access these with @

#begin function readodkdata
readodkdata<-function(filepath, tab=1, col_types = col_types, na.strings = c("NA", 'n/a'), check.names=T){
  require(readxl)
  require(readr)
  # define class "odkdata"
  
  setClass(Class="readodk",
           representation(
             data="data.frame",
             varnames="character"
           )
  )
  
  # read in tabs 
  excel<-ifelse(substr(filepath, nchar(filepath)-2, nchar(filepath))%in%c('lsx', 'xls'),TRUE,FALSE)
  if(is.null(na.strings)) na.strings<-character()
  if(excel){
    data<-as.data.frame(read_excel(path=filepath, sheet=tab, na=na.strings, trim_ws = F, col_types=col_types))
    varnames<-colnames(data)
    if(check.names){colnames(data)<-make.unique(colnames(data))}
  }
  else{
  data<-as.data.frame(read_csv(file=filepath,  na=na.strings, trim_ws = F, col_types=cols(.default = "c")))
  varnames<-colnames(data)
  if(check.names){colnames(data)<-make.names(colnames(data), unique = T)}}
  
  # return object of class "odkdata"
  return(new("readodk",data=data, varnames=varnames))
  
}
#end function readodkdata





### function revarnames
### auxiliary function for odkFormat. Recreates varnames used in a plain csv export from the server platform (split multiple, delimiter = '/')
### and adds name column from XLSform to enable lookups for label and codelables from data to XLSform
### Arguments:  form:       path to XLSform as character string 
###                         OR object of class "form" created by readForm function
### Value:      returns survey tab with new column varnames containing the server platform csv variable names

#begin function revarnames
revarnames<-function(form, choicelist=F, del='/', groupnames=T, language="English"){
  require(readxl)
  require(dplyr)
  require(stringi)
  
  # read in form
  if(class(form)=="form"){survey<-form@survey 
                          choices<-form@choices} else if(is.character(form)){
                            survey<-as.data.frame(read_excel(form, sheet="survey", col_types = "text", trim_ws = F))
                              choices<-as.data.frame(read_excel(form, sheet="choices", col_types = "text", trim_ws = F))} 
  
  # make names columns compatible with R variable names
  colnames(survey)<-make.names(colnames(survey), unique = FALSE)
  colnames(choices)<-make.names(colnames(choices), unique = FALSE)
  
  # specify language columns
  langname<-ifelse(language=="","label",paste0("label..",language))
  
  #survey$name<-make.names(survey$name)
  
  # create new vector of variable names varnames and use it to recreate odk-style names with 'group/name'
  # create new vector of variable names shortname (for removed groupnames)
  group<-survey$name[sort(union(grep('begin group', survey$type), grep('begin repeat', survey$type)))]
  reps<-survey$name[sort(grep('begin repeat', survey$type))]
  groupind<-logical(length=length(group))
  repind<-logical(length=length(reps))
  survey$varnames<-character(length=dim(survey)[1])
  survey$repeatgroups<-character(length=dim(survey)[1])
  survey$longname<-survey$name
  survey$shortname<-survey$name
  survey$repname<-survey$name
  
  k=0
  m=0
  
  # add group names to varnames
  for (i in 1:dim(survey)[1]){
    if(grepl('begin group', survey$type[i]) || grepl('begin repeat', survey$type[i])){
      k=k+1
      groupind[k]<-TRUE
      if(sum(groupind)>0){l=max(which(groupind))} 
      else {l=0}
      }
    if(grepl('end group', survey$type[i]) || grepl('end repeat', survey$type[i])){
      groupind[l]=FALSE
      if(sum(groupind)>0){l=max(which(groupind))} 
      else {l=0}
      }
    
    if(grepl('begin repeat', survey$type[i])){
      m=m+1
      repind[m]<-TRUE
      if(sum(repind)>0){p=max(which(repind))} 
      else {p=0}
      }
    if(grepl('end repeat', survey$type[i])){
      repind[p]=FALSE
    if(sum(repind)>0){p=max(which(repind))} 
      else {p=0}
      }
    
    if(groupnames){
      survey$varnames[i]<-paste0(c(group[groupind], as.character(survey$name[i])), collapse=del)
      survey$longname[i]<-paste0(c(group[groupind], as.character(survey$name[i])), collapse=del)
      survey$repname[i]<-paste0(c(reps[repind], as.character(survey$name[i])), collapse=del)
      if(length(reps[repind])>0) survey$repeatgroups[i]<-paste(reps[repind], collapse = " ") else survey$repeatgroups[i]<-NA
      }
    else{
      survey$varnames[i]<-as.character(survey$name[i])
      survey$longname[i]<-paste0(c(group[groupind], as.character(survey$name[i])), collapse=del)
      survey$repname[i]<-paste0(c(reps[repind], as.character(survey$name[i])), collapse=del)
      if(length(reps[repind])>0) survey$repeatgroups[i]<-paste(reps[repind], collapse = " ") else survey$repeatgroups[i]<-NA
    }
  }
  
  # split multiple choice variables and add answer options from choices tab
  # add variable type2 to indicate data type and other to indicate 'or_other' answer
  survey$type<-stri_trim_both(survey$type)
  choices$name<-stri_trim_both(choices$name)
  choices$list_name<-stri_trim_both(choices$list_name)
  
  typesplit<-stri_split_charclass(survey$type,  "\\p{WHITE_SPACE}", omit_empty = T)
  survey$type2<-unlist(lapply(typesplit, function(x){x[1]}))
  survey$listname<-unlist(lapply(typesplit, function(x){if(x[1]%in%c('select_multiple', 'select_one')) {x[2]}else{''}}))
  survey$other<-unlist(lapply(typesplit, function(x){x[3]}))
  multiplechoices<-lapply(typesplit, function(x){if(x[1]%in%c('select_multiple', 'select_one')) filter(choices, list_name==x[2]) else ''})
  names(multiplechoices)<-lapply(typesplit, function(x){name<-character() 
                                                        if(x[1]%in%c('select_multiple', 'select_one')){(name<-x[2])}else{(name<-NA_character_)}})
  
  survey2<-survey
  k<-1
  p<-ncol(survey)
  surveynames<-colnames(survey)
  
  for (i in 1:dim(survey)[1]){
    if(!is.na(typesplit[[i]][1]) & typesplit[[i]][1]=='select_multiple'){
      add<-data.frame(matrix(nrow = dim(multiplechoices[[i]])[1]+1, ncol = p, byrow = T))
      add[,]<-survey[i,]
      colnames(add)<-surveynames
      add[1,'type2']<-'select_multiple_unsplit'
      add$varnames<-c(add$varnames[1],paste(add$varnames[2:length(add$varnames)], multiplechoices[[i]]$name, sep=del))
      add$shortname<-c(add$shortname[1],paste(add$shortname[2:length(add$shortname)], multiplechoices[[i]]$name, sep=del))
      add$longname<-c(add$longname[1],paste(add$longname[2:length(add$longname)], multiplechoices[[i]]$name, sep=del))
      add$repname<-c(add$repname[1],paste(add$repname[2:length(add$repname)], multiplechoices[[i]]$name, sep=del))
      add[2:dim(add)[1],langname]<-paste(add[2:dim(add)[1],langname], multiplechoices[[i]][,langname], sep=" :: ")
      survey2[k,]<-add[1,]
      survey2<-insertRows(survey2, add[2:dim(add)[1],], k+1)
      k<-k+dim(add)[1]
    } else {k<-k+1}
    
    if(!is.na(typesplit[[i]][3]) & typesplit[[i]][3]=='or_other'){
      add<-data.frame(matrix(nrow = 2, ncol = p, byrow = T))
      add[,]<-survey[i,]
      colnames(add)<-surveynames
      add$varnames<-paste(add$varnames, c(paste0(del,'other'), '_other'), sep='')
      add$shortname<-paste(add$shortname, c(paste0(del,'other'), '_other'), sep='')
      add$longname<-paste(add$longname, c(paste0(del,'other'), '_other'), sep='')
      add$repname<-paste(add$repname, c(paste0(del,'other'), '_other'), sep='')
      add$type2=c('select_multiple', 'text')
      survey2<-insertRows(survey2, add, k)
      addother<-as.data.frame(matrix(nrow=1, ncol =dim(multiplechoices[[i]])[2]))
      colnames(addother)<-colnames(multiplechoices[[i]])
      addother[1,"list_name"]<-multiplechoices[[i]][1,"list_name"]
      addother[1,"name"]<-'other'
      addother[1,langname]<-'Other'
      multiplechoices[[i]]<-rbind(multiplechoices[[i]], addother)
      k<-k+2
    }
    
    if(!is.na(survey$type2[i]) & survey$type2[i]=='geopoint'){
      add<-data.frame(matrix(nrow = 4, ncol = p, byrow = T))
      add[,]<-survey[i,]
      colnames(add)<-surveynames
      add$varnames<-paste(add$varnames, c('_latitude', '_longitude', '_altitude', '_precision'), sep='')
      add$varnames<-stri_replace_last_fixed(add$varnames, del, paste0(del,'_'))
      add$shortname<-paste(add$shortname, c('_latitude', '_longitude', '_altitude', '_precision'), sep='')
      add$shortname<-stri_replace_last_fixed(add$shortname, del, paste0(del,'_'))
      add$longname<-paste(add$longname, c('_latitude', '_longitude', '_altitude', '_precision'), sep='')
      add$longname<-stri_replace_last_fixed(add$longname, del, paste0(del,'_'))
      add$repname<-paste(add$repname, c('_latitude', '_longitude', '_altitude', '_precision'), sep='')
      add$repname<-stri_replace_last_fixed(add$repname, del, paste0(del,'_'))
      add$type2=rep('decimal', 4)
      survey2<-insertRows(survey2, add, k)
      k<-k+4
    }
  }
    if(choicelist){out<-list() 
                out[[1]]<-survey2
                out[[2]]<-multiplechoices
                names(out)<-c('survey', 'choicelist')
                out} else {survey2}
}
#end function revarnames



### function insertRows
### source: http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
### auxiliary function to add rows at specified place to exisiting data frame
### Arguments:  existingDF:       data frame
###             newrows:          new row(s) with same number of columns as existingDF or vector with length equal to number of columns of existingDF
###             r:                beginning of row index where new rows are to be inserted as a block
### Value:      returns data frame with new row added at r

#begin function insertRows
insertRows <- function(existingDF, newrows, r) {
  if(is.null(dim(newrows))){newrows<-as.data.frame(t(newrows))}
  if(r<=nrow(existingDF)){existingDF[seq(r+dim(newrows)[1],nrow(existingDF)+dim(newrows)[1]),] <- existingDF[seq(r,nrow(existingDF)),]}
  existingDF[r:(r+dim(newrows)[1]-1),] <- newrows
  existingDF
}
#end function insertRows



### function xlsform_spsslabels
### Does:       Creates SPSS syntax to label .sav file (variable and value labels) based on XLSForm, and excel data file to be read in SPSS and use the syntax on. 
###             The SPSS syntax is written to a .txt file and needs to be applied to the .sav file which is obtained by reading in the excel file into SPSS.
### Arguments:  form:       path to XLSform as character string
###             dat:        path to file (.csv, .xls or .xlsx) as character string
###             tab:        for excel data files: sheet number (integer) or name (character string). Default 1
###             del:        delimiter for group names used in data file exported from platform
###                         possible values: '/' (default) or any other (e.g. "." or ":")
###             language:   language for labels from form (text string of label column names in survey and choices tabs after "label::". 
###                         Put "" if column name is just "label")
###             dataout:    path to data output file (default "spss.xlsx")
###             syntaxout:  path to syntax output file (default "labelsyntax.txt")
###             groupnames: TRUE if the data file has groupnames appended to its variable names (correct delimiter between groups has to be specified using argument "del"), FALSE if not
###             variablenames: If TRUE (default) removes groupname appendixes from data file variable names and leaves only variable names from name column in form survey tab
###             varnamelength: Maximum length of variable names (default 32)
###             varlabellenght: Maximum length of variable labels (default 255)
###             na.strings: vector of character strings in data file to be set to NA (default NULL, i.e. no values set to NA)
###             check.names:TRUE (default) coerces variable names to R-compatible names. This is not necessary for SPSS compatible names, but recommended since it makes the names more readable
### Output:     Writes excel data file and SPSS syntax file to specified paths.
### Needs:      Packages: dplyr, xlsx

#begin function xlsform_spsslabels
xlsform_spsslabels<-function(form, dat, tab=1, del="/", language="English", 
                             dataout="spss.xlsx", syntaxout="labelsyntax.txt",
                             groupnames = TRUE,
                             variablenames = TRUE, 
                             varnamelength=32, varlabellenght=255, 
                             na.strings = NULL,
                             check.names = TRUE){
  
  #source("readodk.R")    
  
  ### read in data using odkFormat function
  main<-odkFormat(form=form, 
                  dat=dat, 
                  tab=tab, 
                  na.strings = na.strings, 
                  del=del, 
                  language=language, 
                  groupnames = groupnames, 
                  variablenames = variablenames,
                  check.names = check.names,
                  calculate='character', 
                  start='character',
                  end='character',
                  today='character',
                  rdate='character',
                  datetime='character',
                  which.out = "data",
                  codebook = T)
  dat<-main@data
  cb<-main@codebook
  
  varnamelength<-varnamelength-1
  
  varnames_ori<-colnames(dat)
  
  for(j in 1:length(colnames(dat))){
    colnames(dat)[j]<-ifelse(length(cb[[varnames_ori[j]]]$`Variable name in XLSForm`)>0,cb[[varnames_ori[j]]]$`Variable name in XLSForm`,
                             substr(varnames_ori[j],nchar(varnames_ori[j])-varnamelength,nchar(varnames_ori[j])))
  }
  colnames(dat)<-make.unique(colnames(dat))
  
  for(j in 1:length(colnames(dat))){
    colnames(dat)[j]<-ifelse(length(cb[[colnames(dat)[j]]]$`Variable name in XLSForm`)>0,cb[[colnames(dat)[j]]]$`Variable name in XLSForm`,
                             substr(colnames(dat)[j],nchar(colnames(dat)[j])-varnamelength,nchar(colnames(dat)[j])))
  }
  
  colnames(dat)<-make.unique(colnames(dat))
  
  varnames<-cbind(colnames(dat), varnames_ori)
  
  varlabels<-character()
  vallabels<-character()
  
  for(j in 1:length(colnames(dat))){
    if(!is.null(cb[[varnames_ori[j]]])){
      if(length(cb[[varnames_ori[j]]]$`Question text (label) in XLSForm`)>0){
        tempvarlab<-cb[[varnames_ori[j]]]$`Question text (label) in XLSForm`
        if(cb[[varnames_ori[j]]]$`Variable type in XLSForm`%in%c("note")){tempvarlab<-"note"}
        if(nchar(tempvarlab)==0|tempvarlab==''){tempvarlab<-colnames(dat)[j]}
        if(nchar(tempvarlab)>varlabellenght){tempvarlab<-substr(tempvarlab,1,varlabellenght)}
        tempvarlab<-gsub("'","",tempvarlab)
        tempvarlab<-paste0(colnames(dat)[j]," '",tempvarlab,"'")
        varlabels<-c(varlabels,tempvarlab)
      }
      
      if(length(cb[[varnames_ori[j]]]$`Variable type in XLSForm`)>0){if(cb[[varnames_ori[j]]]$`Variable type in XLSForm`%in%c("select_multiple","select_one" )){
        if(length(cb[[varnames_ori[j]]]$`Codes`)>0&length(cb[[varnames_ori[j]]]$`Code labels`)>0){
          cleanvallabels<-gsub("'","",cb[[varnames_ori[j]]]$`Code labels`)
          cleancodes<-cb[[varnames_ori[j]]]$Codes
          if(!(is.numeric(cleancodes))){cleancodes<-paste0("'",cleancodes,"'")}
          if(cb[[varnames_ori[j]]]$`Variable type in XLSForm`%in%c("select_multiple")){cleancodes<-ifelse(cb[[varnames_ori[j]]]$Codes=="FALSE",0,1)}
          tempvallab<-paste0(cleancodes," ",  paste0("'",cleanvallabels,"'"))
          tempvallab[length(tempvallab)]<-paste0(tempvallab[length(tempvallab)])
          tempvallab<-c("VALUE LABELS",colnames(dat)[j], tempvallab,".", "EXECUTE.")
          vallabels<-c(vallabels, tempvallab)
        }}
      }
    }
    
  }
  
  varlabels<-c("VARIABLE LABELS", varlabels, ".", "EXECUTE.")
  
  labelsyntax<-c(varlabels, vallabels)
  writeLines(noquote(labelsyntax), syntaxout)
  write.xlsx2(dat, dataout, row.names = F)
  print(paste("Path to data file:", dataout, ", path to SPSS syntax file:", syntaxout))
}
#end function xlsform_spsslabels





### function ft
### hijack of expss::fctr with default prepend_var_lab = F (avoid printing variable label, instead just keep value labels)
### does not order levels automatically

#begin function ft
ft<-function(x, drop_unused_labels=F, prepend_var_lab = F, sort=F, ...){
  require(expss)
  # return(fctr(x, drop_unused_labels=drop_unused_labels, prepend_var_lab=prepend_var_lab, ordered=ordered))  
  
  x = as.labelled(x)
  vallab = val_lab(x)
  varlab = var_lab(x)
  x = unlab(x)
  expss:::stopif(anyDuplicated(vallab), "duplicated values in labels: ", 
                 paste(vallab[duplicated(vallab)], collapse = " "))
  uniqs = unique(x)
  
  # begin expss:::labelled_and_unlabelled
  uniqs = unlab(uniqs)
  if (length(uniqs) > 0) {
    uniqs = uniqs[!is.na(uniqs)]
    names(uniqs) = uniqs
  }
  vallab = vallab %u% uniqs
  if (length(vallab) > 1 & sort) 
    sort(vallab)
  else vallab
  #end expss:::labelled_and_unlabelled
  
  if (drop_unused_labels) {
    vallab = v_intersect(vallab, uniqs)
  }
  if(sort) vallab = sort(vallab)
  if (!is.null(varlab) && (varlab != "") && prepend_var_lab) {
    names(vallab) = paste(varlab, names(vallab), sep = LABELS_SEP)
  }
  if (length(vallab) > 1) {
    names(vallab) = expss:::make_items_unique(names(vallab), with_warning = "duplicated labels: ")
  }
  ordered = expss:::if_null(list(...)$ordered, FALSE)
  res = expss:::fast_match(x, vallab)
  levels(res) = names(vallab)
  class(res) = c(if (ordered) "ordered", "factor")
  res
}
#end function ft 




### function outersect
### returns negative intersect of the values of two vectors

#begin function outersect
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
#end function outersect





### function set_val_lab_nosort
### hijack of expss:::set_val_lab.default so levels do not get sorted automatically

#begin function set_val_lab_nosort
set_val_lab_nosort<-function(x, value = value, sort=F, add = F){
  expss:::stopif(is.null(names(value)), "Labels should be named vector.")
  expss:::stopif(anyDuplicated(value), "duplicated values in labels: ", 
                 paste(value[duplicated(value)], collapse = " "))
  if (is.integer(x)) 
    x[] = as.double(x)
  if (add) 
    value = combine_labels(value, val_lab(x))
  names(value) = expss:::make_items_unique(names(value))
  if(sort) value = sort(value)
  attr(x, "labels") = value
  class(x) = union("labelled", class(x))
  x
}
#end function set_val_lab_nosort


############################################ END ###################################################################