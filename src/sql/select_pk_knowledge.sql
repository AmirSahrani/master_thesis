SELECT count () 
from response_pk
INNER JOIN voter_info ON response_pk.ID = voter_info.ID
WHERE voter_info.CONDITION = 0;