import requests
from requests.adapters import HTTPAdapter
from requests.packages.urllib3.util.retry import Retry
from urllib.parse import urlparse, parse_qs
from bs4 import BeautifulSoup
from bs4.element import Tag, NavigableString
import re
import time
import datetime
import pandas as pd
import os
import sys
import json
import spotipy
import webbrowser
import spotipy.util as util
from json.decoder import JSONDecodeError
import csv

class songkick:
    headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}
    s = None

    def __init__(self):
        # results_dict = {}
        self.s = requests.Session()
        headers = {
            'Accept': '*/*',
            'Origin'    :   'https://www.google.com',
            'Accept-Encoding': 'gzip, deflate',
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/12.1.1 Safari/605.1.15',
            'Accept-Language': 'en-us',
            'Connection': 'keep-alive',
            }

        try:           #'Rotterdam', 'Amsterdam', 'Delft', 'The+Hague',  
            queries = ['Utrecht', 'Dordrecht', 'Gouda']            
            for query in queries:
                print(query)
                names, locations, dates = [], [], []
                url = 'https://www.songkick.com/search?utf8=%E2%9C%93&type=initial&query='+query+'%2C+netherlands'
                response = requests.get(url, headers=headers).text
                time.sleep(1)
                soup = BeautifulSoup(response, 'lxml')
                city_link = soup.find('p', class_ = 'summary').find('a')['href']
                i = 0
                if query == "Utrecht":
                    city_link = '/metro-areas/31410-netherlands-utrecht'
                    i = 1                
                # print(link)              
                while True:
                    i = i + 1
                    link = 'https://www.songkick.com'+city_link+'?page='+str(i)
                    # print(link)
                    show_resp = requests.get(link, headers=headers).text
                    time.sleep(1)
                    show_soup = BeautifulSoup(show_resp, 'lxml')
                    listings = show_soup.find_all('li', class_='event-listings-element')
                    # print(listings)
                    
                    for listing in listings:
                        date = listing.find('time')['datetime'].split("T")[0]
                        date = datetime.datetime.strptime(date, '%Y-%m-%d').date()
                        info = listing.find('div', class_='artists-venue-location-wrapper')
                        name = info.find('p', class_='artists').find('strong').text

                        if "Festival" not in name:
                            location = info.find('p', class_='location').find('a').text if info.find('p', class_='location').find('a') is not None else None
                            
                            if len(name.split('and')) > 1:
                                l = [i.replace("'", "").replace(",", "").replace(" (NL)", "").replace("s", "").strip() for i in name.split('and') if i is not '' and "Festival" not in i]
                                names.extend(l)
                                dates.extend([i for i in len(l)*[date]])
                                locations.extend([i for i in len(l)*[location]])
                                
                            elif len(name.split(',')) > 1:
                                l = [i.replace("'", "").replace(",", "").replace(" (NL)", "").replace("s", "").strip() for i in name.split(',') if i is not '' and "Festival" not in i]
                                names.extend(l)
                                dates.extend([i for i in len(l)*[date]])
                                locations.extend([i for i in len(l)*[location]])                            
                                
                            else:
                                names.append(name)
                                dates.append(date)
                                locations.append(location)

                        if date > datetime.date(2020, 6, 30):
                            break
                    
                    if date > datetime.date(2020, 6, 30):
                        break
            
                
                data = pd.DataFrame({'Date' : dates, 'Artist' : names, 'Location' : locations}).drop_duplicates(subset="Artist", keep="first")
                # print(data.index[data['Artist'] == 'Greg Dulli'])
                self._get_features(data, query)
                
        except Exception as e:
            print(e)
            pass

    def _get_features(self, data, city):
        # print(data['Artist'])
        username = 'yid5g7sr8ua7yafjhesa6xxj2'

        try:
            token = util.prompt_for_user_token(username)
        except:
            os.remove(f".cache-{username}")
            token = util.prompt_for_user_token(username)

        sp = spotipy.Spotify(auth=token)

        self.s = requests.Session()
        headers = {
            'Accept': '*/*',
            'Origin'    :   'https://www.google.com',
            'Accept-Encoding': 'gzip, deflate',
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/12.1.1 Safari/605.1.15',
            'Accept-Language': 'en-us',
            'Connection': 'keep-alive',
            }

        for index, row in data.iterrows():
            print("Artist : ", row['Artist'])

            try:
                url = 'https://www.google.com/search?q='+row['Artist']+'+on+spotify'
                response = requests.get(url, headers=headers).text
                time.sleep(1)
                soup = BeautifulSoup(response, 'lxml')
                spotify_uri = str(soup.find('div', class_='r'))
                # print("1 :", spotify_uri,'\n')
                spotify_uri = spotify_uri.split('<a href="https://open.spotify.com/artist/')[1].split(" ")[0].replace('"', '')
                print("uri : ", spotify_uri, '\n')
                top_tracks = sp.artist_top_tracks(spotify_uri)
                
                count = 0
                genres = sp.artist(spotify_uri)["genres"]
                for track in top_tracks["tracks"]:
                    count = count + 1
                    id = track["id"]
                    name = track["name"]
                    artist_name = track["artists"][0]["name"]
                    # print(artist_name)
                    pop = track['popularity']

                    print(name)
                    
                    with open(r'concerts.csv', 'a', newline='', encoding="utf-8") as csvfile:
                        fieldnames = ["name", "artist", "popularity", "genres", "danceability", "energy", "key", "loudness",
                                        "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo",
                                        "city", "date", "location"]
                        
                        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
                        writer.writerow({'name' : name,
                                        'artist' : artist_name,
                                        'popularity' : pop,
                                        'genres' : genres,
                                        'danceability' : sp.audio_features(id)[0]['danceability'],
                                        'energy' : sp.audio_features(id)[0]['energy'],
                                        'key' : sp.audio_features(id)[0]['key'],
                                        'loudness' : sp.audio_features(id)[0]['loudness'],
                                        'mode' : sp.audio_features(id)[0]['mode'],
                                        'speechiness' : sp.audio_features(id)[0]['speechiness'],
                                        'acousticness' : sp.audio_features(id)[0]['acousticness'],
                                        'instrumentalness' : sp.audio_features(id)[0]['instrumentalness'],
                                        'liveness' : sp.audio_features(id)[0]['liveness'],
                                        'valence' : sp.audio_features(id)[0]['valence'],
                                        'tempo' : sp.audio_features(id)[0]['tempo'],
                                        'city' : city.replace("+", " "),
                                        'date' : row['Date'],
                                        'location' : row['Location']})

                    if count == 5:
                        print("\n")
                        break

            except Exception as e:
                print(e)            

songkick()
