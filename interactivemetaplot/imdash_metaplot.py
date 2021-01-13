import plotly.express as px

df = px.data.gapminder().query("country=='Canada'")

print(df)
#fig = px.line(df, x="year", y="lifeExp", title='Life expectancy in Canada')
#fig.show()
