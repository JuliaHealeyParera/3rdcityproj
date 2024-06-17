import requests
from bs4 import BeautifulSoup
import pandas as pd

#URL for the Alabama prison list 
url = "https://en.wikipedia.org/wiki/List_of_Alabama_state_prisons"

response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')

#Prasing the data
# prison_table = soup.find('table', {'class' : 'sortable wikitable jquery-tablesorter'})
prison_table = soup.find('tbody')
facilities = []

#Extract the first td tag in each row of the table 
for row in prison_table.find_all('tr'): 
    td_tags = row.find_all('td')
    if len(td_tags) > 0:
        first_td_tag = td_tags[0]
        fourth_td_tag = td_tags[3]

        latitude = fourth_td_tag.find_all('span', {'class', 'latitude'})
        longitude = fourth_td_tag.find_all('span', {'class', 'longitude'})

        facilities.append([first_td_tag.text.strip(), latitude[0].text.strip(), longitude[0].text.strip()])
# print(facilities)

alabama_df = pd.DataFrame(facilities, columns = ["Name", "Latitude", "Longitude"])
alabama_df.to_csv("alabama_prisons.csv", index = False)
