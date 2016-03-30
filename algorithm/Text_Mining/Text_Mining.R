library(Rwordseg)
library(tmcn)
library(wordcloud)

#读取数据
Evaluation <- read.csv("DataSource.csv", encoding = 'UFT-8')
#Evaluation <- read.csv(file = file.choose(), encoding = 'UFT-8')
#剔除评论数据中含有的英文和数字
text <- gsub('[a-zA-Z0-9]','',Evaluation$Evaluation)
#分词
segword <- segmentCN(strwords = text)
#查看第一条评论的分词效果
segword[1]
#注：经分割后的词中有许多无意义的停止词，如“是”，“只”，“了”，“也”等，这些词是需要剔除的。关于停止词，可以到网上搜索获取。
#读取停止词
mystopwords <- read.table("mystopwords.txt", stringsAsFactors = FALSE)
head(mystopwords)
class(mystopwords)
#由于读入的数据为数据框格式，需要将其转换为向量格式
mystopwords <- as.vector(mystopwords[,1])
head(mystopwords)
#将分割后的词与停止词词库进行比对，将含有停止词的词进行剔除。下面是自定义删除停止词的函数：
removewords <- function(target_words,stop_words){
  target_words = target_words[target_words%in%stop_words==FALSE]
  return(target_words)
}
#将该函数应用到已分割的词中
segword2 <- sapply(X = segword, FUN = removewords, mystopwords)
#查看已删除后的分词结果
segword2[[1]]
#一些无意义的停止词已经被剔除，下面就使用比较干净的词绘制文字云，以大致查看分词效果。
word_freq <- getWordFreq(string = unlist(segword2))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#绘制出现频率最高的前50个词
wordcloud(words = word_freq$Word, freq = word_freq$Freq, max.words = 50,
          random.color = TRUE, colors = rainbow(n = 7))
par(opar)
#根据频繁出现词汇，还原初始评价
index <- NULL
for(i in 1:length(segword)){
  if (any(segword[[i]] %in% '不错') == TRUE)
    index = unique(c(index, i))
} 
text[index]
#自定义词汇
words <- c('房间干净','服务不错','酒店不错','不错的酒店','不错的地方','卫生不错','设施不错',
           '设备不错','硬件不错','位置不错','地段不错','景色不错','景观不错','环境不错','风景不错',
           '视野不错','夜景不错','口味不错','味道不错','感觉不错','态度不错','态度冷漠','态度冷淡',
           '服务差劲','热情','热心','不热情','态度好','态度差','态度不好','素质差','质量不错',
           '房间不错','浴缸不错','早餐不错','早餐质量差','自助餐不错','下午茶不错','强烈推荐',
           '推荐入住','值得推荐','性价比不错','隔音不错','体验不错','不错的体验','设施陈旧',
           '五星级酒店','性价比不错','交通便利','交通方便','出行方便','房间小','价格不错',
           '前台效率太低','携程','地理位置','陆家嘴')
#插入自定义词汇
insertWords(strwords = words)
#根据业务情况、需要在原始评论中删除的字和词
pattern <- c('还是','很也','了','点','可以','还','是','真心','都','相当','大家','确实','挺','非常',
             '应该','蛮','整体','里面','就','实在','总体','听说','有点','比较','质量','都是','够',
             '十分','还算','极其','也算','方面','太','算是')
#将这些词组成“正则表达式”
pattern2 <- paste("[",paste(pattern,collapse = ','),"]", sep = '')
#剔除原始评论中含有的这些干扰词汇
text2 <- gsub(pattern = pattern2, replacement = '', x = text)
#分词
segword3 <- segmentCN(strwords = text2)
head(segword3)
#新建停止词
stopwords_v2 <- c('不错','酒店','交通','前台','出差','价','去','免费','入','入住','大道','吃','退',
                  '上海','说','床','态度','升级','地理','很好','号','住','服务员','房间','服务',
                  '设施','环境','位置')
#创建新添加的停止词
mystopwords <- c(mystopwords,stopwords_v2)
#排除停止词
segword4 <- sapply(X = segword3, FUN = removewords, mystopwords)
#查看已删除后的分词结果
segword4[[1]]
#再一次绘制文字云，具体如下：
word_freq2 <- getWordFreq(string = unlist(segword4))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#绘制出现频率最高的前50个词
wordcloud(words = word_freq2$Word, freq = word_freq2$Freq, scale = c(4,0.1), 
          max.words = 50, random.color = TRUE, colors = rainbow(n = 7))
par(opar)
#再一次清除停止词
stopwords_v3 <- c('早餐','嘴','电话','订','楼','人员','钟','修','办理','客人','品种','朋友','带'
                  ,'出门','房','影响','硬件','感觉','想','验','洁','希望','送') 
segword5 <- sapply(X = segword4, FUN = removewords, stopwords_v3)
#查看已删除后的分词结果
segword5[[1]]
#再绘制一次文字云：
word_freq3 <- getWordFreq(string = unlist(segword5))
opar <- par(no.readonly = TRUE)
par(bg = "black")
#绘制出现频率最高的前50个词
wordcloud(words = word_freq3$Word, freq = word_freq3$Freq, 
          scale = c(4,0.1), max.words = 50,random.color = TRUE, colors = rainbow(n = 7))
par(opar)
#将推荐和值得推荐合并
segword6 <- unlist(segword5)
segword6[segword6 == '推荐'] <- '值得推荐'
#重新绘制文字云
word_freq4 <- getWordFreq(string = unlist(segword6))
opar <- par(no.readonly = TRUE)
par(bg = "black")
#绘制出现频率最高的前50个词
wordcloud(words = word_freq4$Word, freq = word_freq4$Freq, scale = c(4,0.1), max.words = 50,
          random.order = F, random.color = T, colors = rainbow(n = 7))
par(opar)
#注：也可以使用工具tagxedo绘制文字云，绘制之前需要将txt文件输入该工具中，该工具的使用可至网站：http://www.tagxedo.com
#将前50的词频写出到txt文件
write.table(head(word_freq4,50),'word_freq.txt', row.names = FALSE, sep = ' ', quote = FALSE)














