import os
import sys
import json
import spotipy
import webbrowser
import spotipy.util as util
from json.decoder import JSONDecodeError
import time
import csv

# username = sys.argv[1]

ply_uris = {'2ifz20E9OmNq4bwiM4Ljwu' : "Amsterdam",
        "594XW2cN7eFreReSU3HFK9" : 'Rotterdam',
        "2DCZWc9sIrxOph8dzcXhGD" : 'Delft',
        "2i1lQUNjc3Kbxl4XhpcqoF" : 'Den Haag',
        '5ILeJUo9AjIbWVuoOn3BPQ' : "Dordrecht",
        "1fAtstgEx5q8xiiFaZXVdI" : 'Utrecht',
        "4hUmg8HXOhhDb9u1sHDsvc" : 'Gouda',
        "1sovhQVUz8lFovpxOKV5qH" : 'Groningen',
        "7v9zp0FdhyYKTGxswaG6nE" : 'Schiedam',
        '0uehRFvXwjPbY6diTH6FO8' : 'Capelle',
        '3Xe44OPLCbblX4medSpucH' : 'Vlaardingen',
}

# 2ifz20E9OmNq4bwiM4Ljwu - Amsterdam
# 594XW2cN7eFreReSU3HFK9 - Rotterdam
# 2DCZWc9sIrxOph8dzcXhGD - Delft
# 2i1lQUNjc3Kbxl4XhpcqoF - Den Haag
# 5ILeJUo9AjIbWVuoOn3BPQ - Dordrecht
# 1fAtstgEx5q8xiiFaZXVdI - Utrecht
# 4hUmg8HXOhhDb9u1sHDsvc - Gouda
# 1sovhQVUz8lFovpxOKV5qH - Groningen
# 7v9zp0FdhyYKTGxswaG6nE - Schiedam
# 0uehRFvXwjPbY6diTH6FO8 - Capelle
# 3Xe44OPLCbblX4medSpucH - Vlaardingen

username = 'yid5g7sr8ua7yafjhesa6xxj2'

try:
    token = util.prompt_for_user_token(username)
except:
    os.remove(f".cache-{username}")
    token = util.prompt_for_user_token(username)

sp = spotipy.Spotify(auth=token)
user = sp.current_user()

for ply_uri in ply_uris:
    play = sp.playlist(ply_uri)
    artists, names, uri, pop, genres = [], [], [], [], []

    for track in play["tracks"]["items"]:
        names.append(track['track']['name'])
        uri.append(track['track']['uri'])
        pop.append(track['track']['popularity'])
        artists.append(track['track']['artists'][0]["name"])
        genres.append(sp.artist(track['track']['artists'][0]["uri"])["genres"])

    for i,track in enumerate(uri):
        with open(r'spotify.csv', 'a', newline='', encoding='utf8') as csvfile:
                                            fieldnames = ["name", "artist", "popularity", "genres", "danceability", "energy", "key",
                                            "loudness", "mode", "speechiness", "acousticness", "instrumentalness",
                                            "liveness", "valence", "tempo", "city"]
                                            
                                            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
                                            try:
                                                writer.writerow({'name' : names[i],
                                                                    'artist' : artists[i],
                                                                    'popularity' : pop[i],
                                                                    'genres' : genres[i],
                                                                    'danceability' : sp.audio_features(track)[0]['danceability'],
                                                                    'energy' : sp.audio_features(track)[0]['energy'],
                                                                    'key' : sp.audio_features(track)[0]['key'],
                                                                    'loudness' : sp.audio_features(track)[0]['loudness'],
                                                                    'mode' : sp.audio_features(track)[0]['mode'],
                                                                    'speechiness' : sp.audio_features(track)[0]['speechiness'],
                                                                    'acousticness' : sp.audio_features(track)[0]['acousticness'],
                                                                    'instrumentalness' : sp.audio_features(track)[0]['instrumentalness'],
                                                                    'liveness' : sp.audio_features(track)[0]['liveness'],
                                                                    'valence' : sp.audio_features(track)[0]['valence'],
                                                                    'tempo' : sp.audio_features(track)[0]['tempo'],
                                                                    'city' : "Rotterdam" if ply_uris[ply_uri] in ["Rotterdam", "Vlaardingen", "Capelle", "Schiedam"] else "Not"
                                                                    })
                                            except:
                                                pass

# artists_doelen = {
#         'Rotterdam Philharmonic Orchestra' : 'spotify:artist:79xQdsSFyN4cmMsxtWrvUc',
#         'Valery Gergiev' : 'spotify:artist:2LxnoYPOe0FCLC82R3xgO2',
#         'Rossini' : 'spotify:artist:0roWUeP7Ac4yK4VN6L2gF4',
#         'Shostakovich' : 'spotify:artist:6s1pCNXcbdtQJlsnM1hRIA',
#         'Wannebiezz' : 'spotify:artist:33XM0RpSrJpoFkbmGqzXfg',
#         'Bach' : 'spotify:artist:5aIqB5nVVvmFsvSdExz408',
#         'Prokofiev' : 'spotify:artist:4kHtgiRnpmFIV5Tm4BIs8l',
#         'Tchaikovsky' : 'spotify:artist:3MKCzCnpzw3TjUYs2v7vDA',
#         'Moessorgski' : 'spotify:artist:284mnx33IWcymQEpMxyfHl',
#         'Grieg' : 'spotify:artist:5ihY290YPGc3aY2xTyx7Gy',
#         'Ravel' : 'spotify:artist:17hR0sYHpx7VYTMRfFUOmY',
#         'Vee kapoor' : 'spotify:artist:6HLcCtYEqZkc4BpCuoogyY',
#         'Smetana'  : 'spotify:artist:25Eab1kIY1gh0Yo1oV04G4',
#         'Beethoven' : 'spotify:artist:2wOqMjp9TyABvtHdOSOTUS',
#         'Tata Mirando' : 'spotify:artist:5hfLQeeg7S0l4zE7KJhC90',
#         'Kagel' : 'spotify:artist:0IdJUMh8xVuehu7qlMcDth',
#         'Castrucci'  : 'spotify:artist:3tb36wlmwM1Dw8SlevHb4H',
#         'Stradella' : 'spotify:artist:5ZMpmJXUkD9Q1mV9cUHKmn',
#         'Pasquini'  : 'spotify:artist:7jzikcFG90FsgmjxokrfPP',
#         'Corelli' : 'spotify:artist:5dmMpIyAVaH6b9FLFgWPrF',
#         'Locatelli' : 'spotify:artist:2zcA2rLi9jv1i97HmFWFv4',
#         'Händel' : 'spotify:artist:1QL7yTHrdahRMpvNtn6rI2',
#         'Homay' : 'spotify:artist:2Se87n5HxpzfAkQDdeacYC',
#         'Antoine Pierre' : 'spotify:artist:5aLaAmqUGjIsoHD3mOjI2V',
#         'Richard Wagner' : 'spotify:artist:1C1x4MVkql8AiABuTw6DgE',
#         'Igor Stravinsky' : 'spotify:artist:7ie36YytMoKtPiL7tUvmoE',
#         'Carel Kraayenhof' : 'spotify:artist:12TNXxHqXaIwJjgd6C9O4X',
#         'Juan Pablo Dobal' : 'spotify:artist:3huRY6wobdYsx6gCV9yHqT',
#         'Mahler' : 'spotify:artist:2ANtgfhQkKpsW6EYSDqldz',
#         'Korngold' : 'spotify:artist:3UaJz1tq0BBPzJBPgkBarb',
#         'Mozart' : 'spotify:artist:4NJhFmfw43RLBLjQvxDuRS',
#         'Manneke' : 'spotify:artist:1695Nn6e7s9L2t8LIGa0Ci',
#         'ottla' : 'spotify:artist:3HB81lpzUkSIqMo0mesQ9L',
#         'Pinhani' : 'spotify:artist:4Bdqzh78prwuqwInMb555P',
#         'Strauss' : 'spotify:artist:6pAwHPeExeUbMd5w7Iny6D',
#         'Aretha Franklin' : 'spotify:artist:7nwUJBm0HE4ZxD3f5cy5ok',
#         'Diana Ross' : 'spotify:artist:3MdG05syQeRYPPcClLaUGl',
#         'Tina Turner' : 'spotify:artist:7nwUJBm0HE4ZxD3f5cy5ok',
#         'Jools Holland' : 'spotify:artist:6eLbRJP12OhyvUv4ntto4e',
#         'Glass Museum' : 'spotify:artist:0bq8ZdJxUOXswye4qT4zzU',
#         'Koray Avcı' : 'spotify:artist:3sV7ijrP5xMHgWRlOnPTCi',
#         'Rachmaninov' : 'spotify:artist:0Kekt6CKSo0m5mivKcoH51',
#         'Berlioz' : 'spotify:artist:11T8SOX82xraocZzUXzkvM',
#         'Erkan Ogur' : 'spotify:artist:7HIccJjwPhWkLVKu0gGJgB',
#         'Derya Türkan' : 'spotify:artist:04e8hX9MAJUSVQNEyBdth0',
#         'Coṣkun Karademir' : 'spotify:artist:2XtF4tyGEl7OOewQEoEtFN',
#         'Anton Eger' : 'spotify:artist:3Tlbv3PR6Xxton3Yp5dSZF',
#         'Ibert' : 'spotify:artist:3KSFGteIQYY5pgE3veclRk',
#         'Benjamin Herman' : 'spotify:artist:1dZtTzcPTFBsbmqNohoKUR',
#         'Ardemus Quartet' : 'spotify:artist:1O4rR1uwZTAkL741vGMOZL',
#         'MacMillan' : 'spotify:artist:5eSXFv3Ll2MEHf0eSAYJGu',
#         'Saint-Saëns' : 'spotify:artist:436sYg6CZhNefQJogaXeK0',
#         'Half Easy Trio' : '5z54KJzRVVaIcsKRtcM9xX',
#         'Ludovico Einaudi' : '2uFUBdaVGtyMqckSeCl0Qj',
#         'Barbra Streisand' : '7jmTilWYlKOuavFfmQAcu6',
#         'Petra Berger' : '5R9MbfCSz54OIpSunDrGZw',
#         'Andranik Madadian' :'6I5D7GelDj9EwRvLmqW3Ct',
#         'Sasy' : '0LkkLM2M97UEG7hi5mLn3u',
#         'Erich Wolfgang Korngold' : '3UaJz1tq0BBPzJBPgkBarb',
#         'Kurt Weill' : '69a1b61a56mJlx8iAUMBsY',
#         'Charles Koechlin' : '777ILKUd9KdXnQq0UX9G36',
#         'Darius Milhaud' : '6bwXuNL4AuC7w3AxspKXn6',
#         'Ernst Krenek' : '7ABfksc6hkcvh4mo8qzL5A',
#         'Paul Hindemith' : '3u1fWmwpwPOmMelTAo0Gb8',
#         'Baer Traa' : '770rlC0qChwv8pIKeJdZgW',
#         'Merel Sophie' : '31da3ihyifJ7rQ2i4RYUqA',
#         'DJ Amir Ghavami' : '5s83rVbyjkBDxlUXE1tI7F',
#         'Robert Schumann' : '2UqjDAXnDxejEyE0CzfUrZ',
#         'The Henk Meutgeert New Jazz Orchestra' : '0SBKeBDaxjHdKxvZWtny1y',
#         'Peter Beets' : '1k7V0GjR6nO9UBJ8kDI1r5',
#         'Felix Mendelssohn' : '6MF58APd3YV72Ln2eVg710',
#         'Jordi Savall' : '3faEZMpTmZFXpELU1EwWNL',
#         'Ballaké Sissoko' : '0OQeMFqoRD5clB0cPYVbxY',
#         'Driss El Maloumi' : '2HnfCI3Zf6r0hWwGJ8p2aB',
#         'Rajery' : '3PxqYlOoVs3HftCle5jaNB',
#         'Salvador Sobral' : '0GfYO21pue5u0sVEYk9HZO',
#         'Ludovico Einaudi' : '2uFUBdaVGtyMqckSeCl0Qj',
#         'Marutyri' : '2SoKrTVqDoyPGOZLoejWCF',
#         'Grieg' : 'spotify:artist:5ihY290YPGc3aY2xTyx7Gy',
#         'Ahmet Aslan' : 'spotify:artist:7nx9ts8pRmbvMQvnPwqDSS',
#         'Cristina Branco' : 'spotify:artist:0Naz1mk7F25xrk44Ugd8nd',
#         'Certain Animals' : '63B62h8zh5sHiv6iCvHMhZ',
#         'Tindersticks' : '3dmSPhg0tdao8ePj4pySJ5',
#         'Calefax' : '4FfYG3ysp40pXwXmht20HP',
#         'Margriet Sjoerdsma' : '4gadaf91p1qrzPBDT6oS0M',
#         'Gustav Mahler' : '2ANtgfhQkKpsW6EYSDqldz',
#         'Jean Françaix' : '3WYaVkaqreSv0ARzFzXDoW',
#         'Collabro' : '0l1nmcjqNlk9dQHb6H4sLR',
#         'David Briggs' : '6fpm54DfoAd1JokubtQiNL',
#         'Hans Abrahamsen' : '7lFNEy6KsssnTZPNg78ZJM',
#         'Claude Vivier' : '40lnPEElT844RiQV8duFgS',
#         'Efe Erdem' : '7JQrGBCocvCCPaM5sBXcaC',
#         'Aynur' : '6kZvCKj0MxeLEg35TXYZDd',
#         'Lendvai String Trio' : '7vikNY2IwXyXbUQP6WZ658',
#         'Mendelssohn' : '6MF58APd3YV72Ln2eVg710',
#         'Johannes Brahms' : '5wTAi7QkpP6kp8a54lmTOq',
#         'Maurice Ravel' : '17hR0sYHpx7VYTMRfFUOmY',
#         'Fabrizio Paterlini' : '0jrFMgW018F1XVnLtCXOKi',
#         }

# # print(artists_doelen.values())

# for artist in artists_doelen.values():
#     genres = []
#     top_tracks = sp.artist_top_tracks(artist)
#     count = 0
#     genres = sp.artist(artist)["genres"]
#     s = ', '.join(i for i in genres)
#     # print(genres)
#     # print(s)

#     for track in top_tracks["tracks"]:
#         count = count + 1
#         id = track["id"]
#         name = track["name"]
#         artist_name = track["artists"][0]["name"]
#         # print(artist_name)
#         pop = track['popularity']
        
#         with open(r'de_doelen.csv', 'a', newline='', encoding="utf-8") as csvfile:
#             fieldnames = ["name", "artist", "popularity", "genres", "danceability", "energy", "key", "loudness",
#                             "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo"]
            
#             writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            
#             try:
#                 writer.writerow({'name' : name,
#                                 'artist' : artist_name,
#                                 'popularity' : pop,
#                                 'genres' : s,
#                                 'danceability' : sp.audio_features(id)[0]['danceability'],
#                                 'energy' : sp.audio_features(id)[0]['energy'],
#                                 'key' : sp.audio_features(id)[0]['key'],
#                                 'loudness' : sp.audio_features(id)[0]['loudness'],
#                                 'mode' : sp.audio_features(id)[0]['mode'],
#                                 'speechiness' : sp.audio_features(id)[0]['speechiness'],
#                                 'acousticness' : sp.audio_features(id)[0]['acousticness'],
#                                 'instrumentalness' : sp.audio_features(id)[0]['instrumentalness'],
#                                 'liveness' : sp.audio_features(id)[0]['liveness'],
#                                 'valence' : sp.audio_features(id)[0]['valence'],
#                                 'tempo' : sp.audio_features(id)[0]['tempo']})
#             except:
#                 pass    
        
#         if count == 5:
#             break
        