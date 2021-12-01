#remove na rows

archetype_survey_responses = na.omit(archetype_survey_responses2)

enrolments = na.omit(enrolments2)

leaving_survey_responses = na.omit(leaving_survey_responses2)

question_response = na.omit(question_response2[,-8])
question_response = na.omit(question_response)

step_activity = na.omit(step_activity2)

team_members = na.omit(team_members2)

video_stats = na.omit(video_stats2)

weekly_sentiment_survey_responses = na.omit(weekly_sentiment_survey_responses2)



#