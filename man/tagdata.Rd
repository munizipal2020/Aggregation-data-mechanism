\name{add_tag_data}
\alias{add_tag_data (D)}
\title{add_tag_data}
\usage{
add_tag_data(D)
}
\description{
Получает data.frame тегированных данных.
D - исходный data.frame
}
\examples{
D1<-read.csv("https://raw.githubusercontent.com/junaart/DataForR/main/model_2.csv", sep=",",header=TRUE)
D2<-read.csv("https://raw.githubusercontent.com/junaart/DataForR/main/org_all.csv", sep=",",header=TRUE)
eav_city1<-add_tag_data(D1[1,])
eav_city2<-add_tag_data(D1[2,])


eav_org1<-add_tag_data(D2[1,])
eav_org2<-add_tag_data(D2[2,])
}

\name{add_value}
\alias{add_value(data_tag, uuid, time, name, value)}
\title{add_value}
\usage{
add_value(data_tag, uuid, time, name, value)
}
\description{
Получает data.frame тегированных данных.
D - исходный data.frame
}
\examples{
Функция добавления нового значения, свойства или объекта в тегированные данные data_tag

}
