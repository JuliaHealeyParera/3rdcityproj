import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Mississippi executions table
url = "https://www.mdoc.ms.gov/general-public/death-row/death-penalty-mississippi"

response = requests.get(url, headers = headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser')

#Parsing the data 
mississippi_executions = soup.find_all('table') 
individual_death = []

for table in mississippi_executions: 
    rows = table.find_all('tr')
    first_tr = True 
    
    for row in rows: 
        if (first_tr):
            first_tr = False
            continue 
        td_tags = row.find_all('td')         
        information = []
        if len(td_tags) > 0: 
            for characteristic in td_tags: 
                information.append(characteristic.text.strip())
        individual_death.append(information) 

mississippi_ex_df = pd.DataFrame(individual_death, columns = ["Name", "Race-Sex", "Offense", "Date Executed"])
data_path = os.path.join('data', 'webscraping_data', 'mississippi_executions.csv')
mississippi_ex_df.to_csv(data_path, index = False) 