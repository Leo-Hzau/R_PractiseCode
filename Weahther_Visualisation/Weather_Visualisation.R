#1.爬虫准备
library(RCurl)        # 加载类库
library(XML)

getWeather<-function (x){
  url<-paste('http://weather.yahooapis.com/forecastrss?w=',x,'&u=c',sep="")       # yahoo的数据源地址
  doc = xmlTreeParse(getURL(url),useInternal = TRUE)                              # 解析XML文档
  ans<-getNodeSet(doc, "//yweather:atmosphere")
  humidity<-as.numeric(sapply(ans, xmlGetAttr, "humidity"))                       # 温度
  visibility<-as.numeric(sapply(ans, xmlGetAttr, "visibility"))                   # 能见度
  pressure<-as.numeric(sapply(ans, xmlGetAttr, "pressure"))                       # 气压
  rising<-as.numeric(sapply(ans, xmlGetAttr, "rising"))                           # 气压变动
  ans<-getNodeSet(doc, "//item/yweather:condition")
  code<-sapply(ans, xmlGetAttr, "code")                                           # 天气情况
  ans<-getNodeSet(doc, "//item/yweather:forecast[1]")
  low<-as.numeric(sapply(ans, xmlGetAttr, "low"))                                 # 最高气温
  high<-as.numeric(sapply(ans, xmlGetAttr, "high"))                               # 最低气温
  print(paste(x,'==>',low,high,code,humidity,visibility,pressure,rising))
  cbind(low,high,code,humidity,visibility,pressure,rising)                        # 以data.frame格式返回
}
#test
#w<-getWeather(2151330) 

#数据准备
#en，城市英文名
#woeid， Yahoo天气API定义的WOEID，用于匹配城市
#zh，城市中文名
#prov，城市所在省的中文名
#long，经度(中国处于东经，不区别东经西经)
#lat，纬度(中国处于北纬，不区别南纬北纬)
#low，最低温度
#high，最高温度
#code，天气概括代码
#humidity，湿度
#visibility，能见度
#pressure，大气压
#rising，气压变动
filename<-function(date=Sys.time()){            # 文件根据日期来命名
  paste(format(date, "%Y%m%d"),".csv",sep="")
}

loadDate<-function(date){                       # 读取城市列表，调用爬虫函数，合并数据保存到一个文件中。
  print(paste('Date','==>',date))
  city<-read.csv(file="WOEID.csv",header=FALSE, encoding="utf-8")  # 加载城市列表
  names(city)<-c("en","woeid","zh",'prov','long','lat')
  city<-city[-nrow(city),]
  wdata<-do.call(rbind, lapply(city$woeid,getWeather))
  w<-cbind(city,wdata)
  write.csv(w,file=filename(date),row.names=FALSE,fileEncoding="utf-8")
}

date <- Sys.time()              # 选择日期
loadDate(date)                    # 爬取数据

library(maps)
library(mapdata)
library(maptools)

map<-readShapePoly('mapdata/bou2_4p.shp')     # 加载中国行政区地图数据
plot(map)                                     # 画出中国行政区图

#数据可视化
library("RColorBrewer")
getColors2<-function(map,prov,ctype){
  #name change to ADCODE99
  #ADCODE99 = unique(data.frame(ADCODE99 = map$ADCODE99,prov = map$NAME))
  ADCODE99<-read.csv(file="ADCODE99.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  
  fc<-function(x){ADCODE99$ADCODE99[which(x==ADCODE99$prov)]}
  code<-sapply(prov,fc)
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(map$ADCODE99,f,code);
  ctype[which(is.na(ctype))]=19
  return(ctype[colIndex])
}
summary<-function(data=data,output=FALSE,path=''){
  colors<-c(rev(brewer.pal(9,"Blues")),rev(c('#b80137','#8c0287','#d93c5d','#d98698','#f6b400','#c4c4a7','#d6d6cb','#d1b747','#ffeda0')))    # 定义18种天气特征对应的颜色
  
  temp<-data$code
  title<-"中国各省天气概况"
  ofile<-paste(format(date,"%Y%m%d"),"_code.png",sep="")
  sign<-''
  colors<-rev(colors)
  code<-read.csv(file="code.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  labelcode<-read.csv(file="labelcode.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  ctype<-sapply(temp,function(x){code$type[which(x==code$code)]})

  if(output)png(file=paste(path,ofile,sep=''),width=600,height=600)
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
  par(mar=c(0,0,3,12),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  plot(map,border="white",col=colors[getColors2(map,data$prov,ctype)])       # 地图和天气可视化
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)                     # 标出采样城市
    
  #=======================================          # 图片中的辅助文字
  if(FALSE){
    grid()
    axis(1,lwd=0);axis(2,lwd=0);axis(3,lwd=0);axis(4,lwd=0)
    }
    text(100,58, title,cex=2)
    text(105,54,format(date,"%Y-%m-%d"))
    text(98,65,paste('每日中国天气','http://apps.weibo.com/chinaweatherapp'))
    text(120,-8,paste('provided by The Weather Channel',format(date, "%Y-%m-%d %H:%M")),cex=0.8)
    
    #=======================================          # 文字说明
    for(row in 1:nrow(data)){
      name<-as.character(data$zh[row])
      label<-labelcode$alias[labelcode$type==ctype[row]]
      x1<-ceiling(row/7)
      x2<-ifelse(row%%7==0,7,row%%7)
      x3<-ctype[row]
      fontCol<-'#000000'
      if(x3<=5)fontCol<-head(colors,1)
      if(x3>=12)fontCol<-tail(colors,1)
      text(68+x1*11,17-x2*3,paste(name,' ',label,sign,sep=''),col=fontCol)
      }
    
    #=======================================          # 图例
    par(mar = c(5, 0, 15, 10))
    image(x=1, y=1:length(colors),z=t(matrix(1:length(colors))),col=rev(colors),axes=FALSE,xlab="",ylab="",xaxt="n")
    axis(4, at = 1:(nrow(labelcode)-1), labels=rev(labelcode$alias)[-1], col = "white", las = 1)
    abline(h=c(1:(nrow(labelcode)-2)+0.5), col = "white", lwd = 2, xpd = FALSE)
    if(output)dev.off()
}

data<-read.csv(file=filename(date),header=TRUE,fileEncoding="utf-8", encoding="utf-8")   # 定义数据源
path=''                                                                                  # 定义输出路径
summary(data,output=TRUE,path=path)      # 生成中国各省天气概况图


#可交互的静态图
library(devtools)                     # 加载devtools
install_github("taiyun/recharts")     # 下载安装recharts包
library(recharts)                     # 加载recharts包

eather_html<-function(data=data,type='high',output=FALSE,path=''){    # 输入HTML的天气图
  if(type=='high') {                                   # 白天气温
    df<-data[,c('prov','high')]
    names(df)<-c("prov","气温")
    title<-paste(format(date,"%Y-%m-%d"),"中国各省白天气温",sep="")
    ofile<-paste(format(date,"%Y%m%d"),"_day.html",sep="")
    }else if(type=='low'){                               # 夜间气温
      df<-data[,c('prov','low')]
      names(df)<-c("prov","气温")
      title<-paste(format(date,"%Y-%m-%d"),"中国各省夜间气温",sep="")
      ofile<-paste(format(date,"%Y%m%d"),"_night.html",sep="")
      }
  
  df[,1]<-substr(df[,1],0,2)                           # 数据格式整理
  df[which(df$prov=='黑龙'),]$prov<-'黑龙江'
  df[which(df$prov=='内蒙'),]$prov<-'内蒙古'
  recharts.eMap <- eMap(df, namevar=1, datavar = 2, title=title)      # 数据JSON化处理
  if(output){     
    recharts.eMap$outList[c('chartid','type')]<-NULL
    writeLines(unlist(recharts.eMap$outList),paste(path,ofile,sep=''))
    }else{          
      plot(recharts.eMap)
    }
  }

date<-as.Date('20141001',format='%Y%m%d')     # 设置日期
data<-read.csv(file=filename(date),header=TRUE,fileEncoding="utf-8", encoding="utf-8")      # 加载数据
path=''                                                                                     # 设置文件输出路径

weather_html(data,type='high',output=FALSE,path='')           # 输出中国各省白天气温
weather_html(data,type='low', output=FALSE,path='')           # 中国各省夜间气温

#http://blog.fens.me/r-app-china-weather/