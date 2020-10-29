DELETE FROM game_stats
WHERE game_id = (select max(game_id) from game_stats)