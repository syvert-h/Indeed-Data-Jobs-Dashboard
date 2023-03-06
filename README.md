# Indeed Data Jobs Dashboard

https://trevys.shinyapps.io/Indeed-Data-Jobs-Dashboard/

A Shiny dashboard tracking the following insights of an Indeed "data" job listing in Australia and New Zealand:
- Skills: Data Tools, Hard Skills, Soft Skills
- Job Types: Remote or In-person
- Salary: Yearly, Monthly, Weekly, Daily, Hourly
- Job Market: Job Location, Education Required, Top Hiring Companies

#### Who is this for?
This dashboard is for anyone looking at listings in the data-job market. Different jobs require different skills. It is quite tedious reading through every job description to get to the requirements for a role. But with this dashboard, we can easily get an idea of what a role entails.

I have personally used this to get an idea of what skills I should prioritise learning. For example, the top 5 data tools asked for in Data Analyst jobs in New Zealand are SQL, Excel, PowerBI, R, and Python (in that order). SQL was asked for in 19.6% (almost 1 in every 5) of the jobs listings scraped.

#### More Background:
"Data" jobs is a broad term chosen on purpose. With this, we can get information for all kinds of data-related jobs. For more specific job titles, use the filters within the dashboard.

All data used was personally scraped using the Selenium package in Python (run weekly). For more information on this, feel free to get in touch.

#### Future Improvements:
1.  More Specific Filtering:
The Python script used to scrape the data can handle almost every filter seen on Indeed.com. Introducing this functionality to the dashboard will help search for very specific requirements. (Note: Indeed.com for Australia and New Zealand redirects to country-specific versions. Some filters may or may not be available on such websites)

2. Extract keywords and skills using machine learning (ML):
Currently, skills and other insights are extracted using rule-based NLP. This is a slight issue as it introduces biases; some important keywords relating to such skills may not be in our dictionary of relevant words. (Note: There is an example of keyword extraction using ML in my Data Subreddit Summary Project)