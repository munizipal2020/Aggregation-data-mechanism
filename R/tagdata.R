#Построение фрейма тегированных данных
add_tag_data<-function(D){
  if (is.data.frame(D))
  {
    library(uuid)
    t<-as.character(as.POSIXlt(Sys.time(), tz = "UTC"))
    time_m<-c();uuid_m<-c();name<-c();value<-c()
    for (i in c(1:length(D[,1]))){
      uuid<-UUIDgenerate()
      for (j in c(1:length(colnames(D)))){
        time_m<-c(time_m,t)
        uuid_m<-c(uuid_m, uuid)
        name<-c(name, colnames(D)[j])
        value<-c(value, as.character(D[i,j]))
      }
    }
    result<-data.frame(uuid_m, time_m, name, value)
    colnames(result)<-c("uuid","time", "name","value")
    return(result)
  }
  else
    return(NULL)
}

#Функция добавления нового значения, свойства или объекта
add_value<-function(data_tag, uuid, time, name, value){
  if (is.na(data_tag$uuid[(data_tag$uuid == uuid) &
                          (data_tag$time == as.character(time)) &
                          (data_tag$name == name)][1]))
  { dd<-data.frame(uuid, time, name, as.character(value))
  colnames(dd)<-c("uuid", "time", "name", "value")
  return(rbind(data_tag,dd))}
  else
  {return(data_tag)}
}

#Функция слияния тегированных данных и обычного фрейма данных
union_data_tag<-function(data1, data2){
  result<-data1
  for (i in c(1:length(data2[,1]))){
    result<-add_value(result,as.character(data2[i,1]), as.character(data2[i,2]),
                      as.character(data2[i,3]), as.character(data2[i,4]))
  }
  return(result)
}

#Функция слияния списка обычных фреймов
create_data_tag<-function(lst){
  result<-add_tag_data(lst[1])
  for (i in c(1:length(lst))){
    result<-union_data_tag(result, lst[i])
  }
  return(result)
}

#Функция получения имен атрибутов у заданного объекта на определенный промежуток времени без повторения
get_attribute<-function(dat, uuid, time_lower=NA, time_upper=NA){
  if ((!(is.na(time_lower))) & (!(is.na(time_upper))))
  {return(as.character(unique(dat$name[(dat$uuid==uuid) & (as.POSIXlt(dat$time)>=as.POSIXlt(time_lower))& (as.POSIXlt(dat$time)<=as.POSIXlt(time_upper))])))}
  else
  {
    if ((is.na(time_lower)) & (!(is.na(time_upper))))
    {return(as.character(unique(dat$name[(dat$uuid==uuid) & (as.POSIXlt(dat$time)<=as.POSIXlt(time_upper))])))}
    else
    {
      if ((!(is.na(time_lower))) & (is.na(time_upper)))
      {return(as.character(unique(dat$name[(dat$uuid==uuid) & (as.POSIXlt(dat$time)>=as.POSIXlt(time_lower))])))}
      else
      {return(as.character(unique(dat$name[(dat$uuid==uuid)])))}
    }
  }
}

#Функция получения уникальных uuid объектов на определенный момент времени без повторения
get_uuid<-function(dat,time_lower=NA, time_upper=NA){
  if ((!(is.na(time_lower))) & (!(is.na(time_upper))))
  {return(as.character(unique(dat$uuid[(as.POSIXlt(dat$time)>=as.POSIXlt(time_lower))& (as.POSIXlt(dat$time)<=as.POSIXlt(time_upper))])))}
  else
  {
    if ((is.na(time_lower)) & (!(is.na(time_upper))))
    {return(as.character(unique(dat$uuid[(as.POSIXlt(dat$time)<=as.POSIXlt(time_upper))])))}
    else
    {
      if ((!(is.na(time_lower))) & (is.na(time_upper)))
      {return(as.character(unique(dat$uuid[(as.POSIXlt(dat$time)>=as.POSIXlt(time_lower))])))}
      else
      {return(as.character(unique(dat$uuid)))}
    }
  }
}

#Функция получения актуального на заданный момент значения заданного свойства у заданного объекта
get_value<-function(dat,uuid,name, time = NA){
  if (is.na(time)){
    lst<-as.POSIXlt(dat$time[(dat$uuid==uuid)&(dat$name==name)])
    return(as.character(dat$value[(dat$uuid==uuid) & (dat$name==name) & (as.POSIXlt(dat$time) == max(lst))]))
  }
  else
  {
    lst<-as.POSIXlt(dat$time[(dat$uuid==uuid)&(dat$name==name)&(as.POSIXlt(dat$time))<=as.POSIXlt(time)])
    return(as.character(dat$value[(dat$uuid==uuid) & (dat$name==name) & (as.POSIXlt(dat$time) == max(lst))]))
  }
}

#Функция групповых запросов get_attribute
get_attribute_group<-function(data_list, uuid, time_lower=NA, time_upper=NA){
  result<-c()
  for (i in data_list){
    result<-c(result, as.character(get_attribute(i, uuid, time_lower, time_upper)))
  }
  return(unique(result))
}

#Функция групповых запросов get_uuid
get_uuid_group<-function(data_list, time_lower=NA, time_upper=NA){
  result<-c()
  for (i in data_list){
    result<-c(result, as.character(get_uuid(i, time_lower, time_upper)))
  }
  return(unique(result))
}

#Функция групповых запросов get_value
get_value_group<-function(data_list, uuid, name,  time=NA){
  if (is.na(time)){
    r<-c()
    for (i in data_list){
      r<-c(r,as.character(max(as.POSIXlt(i$time[(i$uuid==uuid)&(i$name==name)]))))}
    max_index<-1; j<-1
    mmax<-r[1]
    for (i in r){
      if (as.POSIXlt(mmax)<as.POSIXlt(i)){
        mmax<-i; max_index<-j
      }
      j<-j+1
    }
    dd<-as.data.frame(data_list[max_index])
    return(get_value(dd, uuid, name, time))
  }
  else
  {
    r<-c()
    for (i in data_list){
      r<-c(r,as.character(max(as.POSIXlt(i$time[(i$uuid==uuid)&(i$name==name)&(as.POSIXlt(i$time))<=as.POSIXlt(time)]))))}
    max_index<-1; j<-1
    mmax<-r[1]
    for (i in r){
      if (as.POSIXlt(mmax)<as.POSIXlt(i)){
        mmax<-i; max_index<-j
      }
      j<-j+1
    }
    dd<-as.data.frame(data_list[max_index])
    return(get_value(dd, uuid, name, time))
  }
}

#Функция получения однородный данных
get_homo_data<-function(dat, list_attr, time){
  all_uuid<-get_uuid(dat)
  r<-c()
  for (i in all_uuid){
    all_attribute<-as.character(get_attribute(dat, i, NA, time))
    p<-TRUE
    for (j in list_attr){
      if (!(j %in% all_attribute)) {p<-FALSE}
    }
    if (p) {r<-c(r,i)}
  }
  result<-matrix(data=NA,nrow=length(r), ncol=length(list_attr)+1)
  for (i in c(1:length(r))){
    for (j in c(2:(length(list_attr)+1))){
      result[i,j]<-as.character(get_value(dat,r[i],list_attr[j-1], time))
    }
    result[i,1]<-r[i]
  }
  result<-data.frame(result)
  colnames(result)<-c("uuid", list_attr)
  return(result)
}


