#archetype_survey_responses data de-duplication
index = duplicated(archetype_survey_responses$learner_id)
archetype_survey_responses2 = archetype_survey_responses[!index,]
archetype_survey_responses2



#Enrolments data de-duplication
index = duplicated(enrolments$learner_id)
enrolments2 = enrolments[!index,]
enrolments2




#leaving_survey_responses data de-duplication
index = duplicated(leaving_survey_responses$left_at)
leaving_survey_responses2 = leaving_survey_responses[!index,]
leaving_survey_responses2



#question-response data de-duplication
index = duplicated(question_response)
question_response2 = question_response[!index,]
question_response2



#step_activity data de-duplication
index = duplicated(step_activity)
step_activity2 = step_activity[!index,]
step_activity2




#team_members data de-duplication
index = duplicated(team_members)
team_members2 = team_members[!index,]
team_members2



#video_stats data de-duplication
index = duplicated(video_stats)
video_stats2 = video_stats[!index,]
video_stats2



#weekly_sentiment_survey_responses data de-duplication
index = duplicated(weekly_sentiment_survey_responses)
weekly_sentiment_survey_responses2 = weekly_sentiment_survey_responses[!index,]
weekly_sentiment_survey_responses2
