import sleeper
from sleeper_wrapper import League
import requests
from sleeperpy import Leagues

league_id = '959330241636257792'
Leagues.get_league(league_id)

url = f"https://api.sleeper.app/v1/league/{league_id}/users"


league = League(league_id)
