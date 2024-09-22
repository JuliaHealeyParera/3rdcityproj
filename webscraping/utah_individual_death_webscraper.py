import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Utah individual deaths table 
url = "https://corrections.utah.gov/recently-deceased-inmates/"

response = requests.get(url, headers=headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser')

#Parsing the data 
utah_deaths = soup.find('table')
individual_death = []

for row in utah_deaths.find_all('tr'): 
    td_tags = row.find_all('td') 
    
    information = []
    if len(td_tags) > 0: 
        for characteristic in td_tags: 
            information.append(characteristic.text.strip())
    individual_death.append(information)

utah_death_df = pd.DataFrame(individual_death[1:], columns = ["Name", "Utah DOC Number", "Date of Death", "Age", "Facility", "Cause"])
data_path = os.path.join('data', 'webscraping_data', 'utah_individual_death.csv')
utah_death_df.to_csv(data_path, index = False)