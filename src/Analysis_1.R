# Load project
library("ProjectTemplate")
load.project()

#analysis the gender
enr_gender = filter(enrolments, gender == "male" | gender == "female" )
g_enr_gender = ggplot(data = enr_gender)
g_enr_gender_bar = g_enr_gender + geom_bar(aes(x = gender, fill = gender ),  width = 0.4) + theme(panel.grid.major =element_blank(),  # 去除边框
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.title =  element_text(size=12,face = "bold"),
                          axis.text.x =   element_text(hjust = 1, # 调整横坐标文字位置
                                                       size=15),  # 调整横坐标文字字号
                          plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  
g_enr_gender_bar




#analysis the country
enr_country = filter(enrolments, country != "Unknown" )
enr_country
#Find out what the countries are
index = duplicated(enr_country$country)
enr_country_list = enr_country[!index,8]    
enr_country_list
#减少数据量加速运算
enr_country = select(enr_country,country)
country_num = c()
for(i in 1:153){
  #generating single country data frame
  single_country = filter(enr_country,country == as.character(enr_country_list[i,1]))
  #compute the number of people that country
  nums = dim(single_country)[1]
  #将该步骤的未完成率加入列表
  country_num = c(country_num ,nums)
}
country_num 
#变成数据框架
country_num = as.data.frame(country_num)
#合并
enr_country_num = cbind(enr_country_list,country_num)
enr_country_num = filter(enr_country_num, country_num>10)
enr_country_num = arrange(enr_country_num,desc(country_num))

myLabel = as.vector(enr_country_num$country)   ## 转成向量，否则图例的标签可能与实际顺序不一致
myLabel = paste(myLabel, "(", round(enr_country_num$country_num / sum(enr_country_num$country_num) * 100, 2), "%) ", sep = "")   ## 用 round() 对结果保留两位小数

ggplot(enr_country_num, aes(x = "", y=country_num, fill = country)) + geom_bar(stat = "identity") +
          coord_polar(theta = "y") + 
          labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + 
          theme(legend.title = element_blank(), legend.position = "top") + 
          scale_fill_discrete(breaks = enr_country_num$country, labels = myLabel)   ## 将原来的图例标签换成现在的myLabel








#analysis the leaving
ggplot(data=leaving_survey_responses,
       aes(x=last_completed_step_number, y=last_completed_step)) +geom_point(stat = "sum", color="darkred")+
       theme(
          axis.title =  element_text(size=12,face = "bold"),
          axis.text.x =   element_text(hjust = 1, # 调整横坐标文字位置
                                       size=15),  # 调整横坐标文字字号
          plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  

view(leaving_survey_responses)







#analysis the false rate
#Find out what the problems are
index = duplicated(question_response$quiz_question)
question_response_quiz = question_response[!index,2]

#compute the false rate for each question
false_rate = data.frame()
for(i in 1:29){
  #generating single question's data frame
  question_response_false = filter(question_response,quiz_question == as.character(question_response_quiz[i,1]))
  #compute the num of true answers
  true_answers = select(question_response_false,correct)
  e = unlist(true_answers)
  true_times = sum(e == "false")
  #compute the rate
  question_response_false = mutate(question_response_false,quiz_false_rates = true_times/dim(question_response_false)[1])
  #add the rate to the data frame
  false_rate = rbind(false_rate,question_response_false) 
}
false_rate
#view(false_rate)
#analysis the false rate of each quiz
#quiz data de-duplication
index = duplicated(false_rate$quiz_question)
quiz_rate = false_rate[!index,]
quiz_rate_plot = select(quiz_rate,quiz_question, quiz_false_rates)
quiz_rate_plot
ggplot(data = quiz_rate_plot, aes(x = quiz_question, y = quiz_false_rates, fill= quiz_question)) + geom_bar(stat='identity') + 
              theme(panel.grid.major =element_blank(),  # 去除边框
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.title =  element_text(size=12,face = "bold"),
                    axis.text.x = element_text(size = 0),  # 调整横坐标文字字号
                    plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  






#analysis the activity step
#Find out what the activities are
index = duplicated(step_activity$step)
step_activity_step = step_activity[!index,2]    
step_activity_step
#compute the complete rate
uncomplete_rate = c()
for(i in 1:58){
  #generating single step's data frame
  step_activity_step_single = filter(step_activity,step == as.character(step_activity_step[i,1]))
  #compute the times that did not finish
  times = select(step_activity_step_single,last_completed_at)
  e = unlist(times)
  uncomplete_times = sum(e == "")
  #将该步骤的未完成率加入列表
  uncomplete_rate = c(uncomplete_rate,uncomplete_times/dim(step_activity_step_single)[1])
}
#变成数据框架
uncomplete_rate = as.data.frame(uncomplete_rate)
#合并
step_activity_step = cbind(step_activity_step,uncomplete_rate)
step_activity_step
#去除较低值
step_activity_step = filter(step_activity_step, uncomplete_rate > 0.15)
ggplot(data = step_activity_step, aes(x = as.factor(step), y = uncomplete_rate, fill= step)) + geom_bar(stat='identity') + 
  theme(panel.grid.major =element_blank(),  # 去除边框
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title =  element_text(size=12,face = "bold"),
        axis.text.x = element_text(size = 12),  # 调整横坐标文字字号
        plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  




#analysis weekly_sentiment_survey_responses
weekly_sentiment_survey_responses
