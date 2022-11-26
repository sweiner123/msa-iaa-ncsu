# Import packages

import requests
import pandas as pd
from datetime import date
from datetime import datetime

api_key = '' #input your API key here

# Create the list of locations to test
loc = [
    ["Bengaluru","India"],
    ["Glasgow", "Scotland"],
    ["Gumi", "South Korea"],
    ["Lagos", "Nigeria"],
    ["Nanaimo", "Canada"],
    ["Niskayuna", "New York"],
    ["Nizhny Novgorod", "Russia"],
    ["Olongapo", "Phillipines"],
    ["Peshawar", "Pakistan"],
    ["Peterhead", "Scotland"],
    ["Quito", "Ecuador"],
    ["Simmern", "Germany"],
    ["Tainan", "Taiwan"],
    ["Tbilisi", "Georgia"],
    ["Vinh Long", "Vietnam"],
    ["Xi'an", "China"]
]

# Function to make the API call and return the data
def make_api_call(data):
    URL = 'http://api.openweathermap.org/data/2.5/forecast?'
    URL = URL + 'q=' + data[0] + ',' + data[1] + '&appid=' + api_key + "&units=metric"
    print('Calling:', URL)
    response = requests.get(URL)
    if response.status_code == 200:      # Success
        data = response.json()
        # printer = pprint.PrettyPrinter( width=80, compact=True )
        # printer.pprint( data[ 'list' ][ 0 ] )
    else:                                # Failure
        print( 'Error:', response.status_code )

    return(data)

# Function that accepts a block's "string" date as input and returns a datetime object
def get_block_date(date_string):
    return datetime.strptime(date_string, '%Y-%m-%d %H:%M:%S').date()

# Function to return the index of when "tomorrow's" block starts
def get_tomorrow_index(data):
    tom_index = 0
    today = date.today()
    for index, row in data.iterrows():
        block_date = row['dt_txt']
        block_date = block_date[11:]
        if block_date == '00:00:00':
            tom_index = index
            break
    #print(tom_index)
    return tom_index

# Function to create a temporary dataframe to perform operations for each city
def create_temp_df(data):
    for i in range(0,len(data['list'])):
        temp_df.loc[i,'dt_txt'] = data['list'][i]['dt_txt']
        temp_df.loc[i,'date'] = get_block_date(data['list'][i]['dt_txt'])
        temp_df.loc[i,'temp_min'] = data['list'][i]['main']['temp_min']
        temp_df.loc[i,'temp_max'] = data['list'][i]['main']['temp_max']

    return(temp_df)

# Create structure of the output data
output = pd.DataFrame(columns=['City','Min 1','Min 2','Min 3','Min 4','Max 1','Max 2','Max 3','Max 4'])

# FOR loop that iterates over every city, fetches the data, performs operations and writes to output
for index in range(0,len(loc)):
    city_country = loc[index][0] + ', ' + loc[index][1]
    
    data = make_api_call(loc[index])

    temp_df = pd.DataFrame(columns=['City','dt_txt','date','temp_min','temp_max'])
    temp_df = create_temp_df(data)

    tom_index = get_tomorrow_index(temp_df)

    #print(temp_df)
    temp_df = temp_df.loc[tom_index:].groupby('date').agg({"temp_min":"min","temp_max":"max"}).head(4).reset_index()

    data_row = [city_country]
    data_row = data_row + list(temp_df['temp_min'].values) + list(temp_df['temp_max'].values)
    output.loc[index] = data_row

output['Min Avg'] = output[['Min 1','Min 2','Min 3','Min 4']].mean(axis=1) # Calculate the Average of the Minimums
output['Max Avg'] = output[['Max 1','Max 2','Max 3','Max 4']].mean(axis=1) # Calculate the Average of the Maximums
output = output[['City','Min 1','Max 1','Min 2','Max 2','Min 3','Max 3','Min 4','Max 4','Min Avg','Max Avg']] #Rearrange columns as per desired output

# Write the final output to temp.csv
output.to_csv('temp.csv', float_format='%.2f',index=False)