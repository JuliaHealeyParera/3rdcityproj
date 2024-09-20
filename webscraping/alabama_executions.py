import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Alabama executions table 
url = "https://doc.alabama.gov/Executions"

response = requests.get(url, headers=headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser')

#Parsing the data 
alabama_executions = soup.find('table', {'class': 'MYTABLE'})
individual_death = []

for row in alabama_executions.find_all('tr'): 
    td_tags = row.find_all('td') 
    
    information = []
    if len(td_tags) > 0: 
        for characteristic in td_tags: 
            information.append(characteristic.text.strip())
    individual_death.append(information)
# print(individual_death[1:]) 

alabama_ex_df = pd.DataFrame(individual_death[1:], columns = ["AIS (Alabama Institution Serial)", "Name", "Race", "Sex", "County", "Crime", "Arrived on Death Row", "Birth Year", "Executed", "Governor"])
data_path = os.path.join('data', 'webscraping_data', 'alabama_executions.csv')
alabama_ex_df.to_csv(data_path, index = False)