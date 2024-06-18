import requests
from bs4 import BeautifulSoup
import csv

# URL of the page to scrape
url = "https://www.tdcj.texas.gov/death_row/dr_offenders_on_dr.html"

# Send a GET request to the page
response = requests.get(url)

# Check if the request was successful
if response.status_code == 200:
    # Parse the page content with BeautifulSoup
    soup = BeautifulSoup(response.content, 'html.parser')
    
    # Find the table containing the data
    table = soup.find('table', class_='tdcj_table indent')

    # Initialize a list to store the data
    data = []

    # Extract table headers
    headers = [header.text.strip() for header in table.find_all('th')]
    data.append(headers)

    # Extract table rows
    rows = table.find_all('tr')[1:]  # Skip the header row

    for row in rows:
        # Extract individual cells
        cells = row.find_all('td')
        row_data = [cell.text.strip() for cell in cells]
        data.append(row_data)
    
    # Write data to a CSV file
    with open('death_row_offenders.csv', 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerows(data)
    
    print("Data has been successfully scraped and saved to death_row_offenders.csv")
else:
    print(f"Failed to retrieve the page. Status code: {response.status_code}")
